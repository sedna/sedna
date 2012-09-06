/*
* File:  sm.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/lockmantypes.h"
#include "common/protocol/InternalConnection.h"
#include "common/protocol/messages/MStartDatabase.h"
#include <common/protocol/messages/MOkAnswer.h>
#include "common/globalobjects/globalnames.h"
#include "common/llcommon/llMain.h"
#include "common/structures/config_data.h"

#include "u/usem.h"
#include "u/usocket.h"
#include "u/uevent.h"
#include "u/ugnames.h"
#include "u/uhdd.h"

#include "auxiliary/warden.h"

#include "sm/hb_utils.h"
#include "sm/trmgr.h"
#include "sm/lm/lm_globals.h"
#include "sm/bufmgr/bm_functions.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/wu/wu.h"

#include "sm/smmsgserver.h"
#include "sm/llsm/physrcv.h"
#include "sm/lockwarden.h"
#include "sm/sm_globals.h"
#include "sm/cdb_utils.h"

#include <sstream>

struct BufferManagerBorders
{
    void start() {
        bm_startup();
        elog(EL_LOG, ("Buffer manager started"));
    }

    void stop() {
        bm_shutdown();
        elog(EL_LOG, ("Buffer manager stopped"));
    };
};

struct LogicalLogBorders
{
    int sedna_db_version;
    bool is_stopped_correctly;

    void start() {
        llInit(databaseOptions->dataFilePath.c_str(),
            databaseOptions->databaseName.c_str(),
            databaseOptions->maxLogFiles,
            &sedna_db_version,
            &is_stopped_correctly,
            false);
    };

    void stop() {
        llRelease();
    };

    void disableCheckpoints() {
        llDisableCheckpoints();
        elog(EL_LOG, ("Checkpoints are disabled"));
    }

    void enableCheckpoints() {
        llEnableCheckpoints();
        elog(EL_LOG, ("Checkpoints are enabled"));
    }
};


struct TransactionTableBorders
{
    void start() {
        init_transaction_ids_table();
        elog(EL_DBG, ("init_transaction_ids_table done"));
    };

    void stop() {
        release_transaction_ids_table();
    };
};

struct CheckpointThread
{
    bool checkpointThreadStarted;

    CheckpointThread() : checkpointThreadStarted(false) {
        init_checkpoint_sems();
        elog(EL_DBG, ("init_checkpoint_sems done"));
    };

    ~CheckpointThread() {
        stop();
        release_checkpoint_sems();
    };

    void stop() {
        if (checkpointThreadStarted) {
            shutdown_chekpoint_thread();
            checkpointThreadStarted = false;
        };
    };

    void start() {
        start_chekpoint_thread();
        elog(EL_DBG, ("start_chekpoint_thread done"));
        checkpointThreadStarted = true;
    }
};

struct WuBorders
{
    void start() {
        WuSetTimestamp(llGetPersTimestamp() + 1);
        WuInitExn(0,0, llGetPersTimestamp());
        elog(EL_LOG, ("Wu is initialized"));
    };

    void stop() {
        WuReleaseExn();
        elog(EL_LOG, ("Wu is released"));
    };
};

struct LockManagerBorders
{
    void start() {
        lm_table.init_lock_table();
        elog(EL_DBG, ("init_lock_table done"));
    };

    void stop() {
        lm_table.release_lock_table();
    };
};

static
void check_tmp_file()
{
    /* check for tmp file (may be absent in hot-backup copy) */

    std::string tmpFileName = databaseOptions->dataFilePath + databaseOptions->dataFileName + ".setmp";

    if (!uIsFileExist(tmpFileName.c_str(), __sys_call_error)) {
        USECURITY_ATTRIBUTES *sa;
        UFile tmpFileHandle;

        if (uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error) != 0) {
            throw USER_EXCEPTION(SE3060);
        }

        if ((tmp_file_handle = uCreateFile(tmpFileName.c_str(), U_SHARE_READ, U_READ_WRITE, U_NO_BUFFERING, sa, __sys_call_error)) == U_INVALID_FD) {
            throw USER_EXCEPTION(SE4301);
        }

        if (uCloseFile(tmp_file_handle, __sys_call_error) == 0) {
            throw USER_EXCEPTION(SE4305);
        }

        if (uReleaseSA(sa, __sys_call_error) != 0) {
            throw USER_EXCEPTION(SE3063);
        }
    }
};


static
void recover_database(CheckpointThread & checkpointThread)
{
    is_recovery_mode = true;

    elog(EL_LOG, ("Starting database recovery or hot-backup restoration..."));

    Warden<LogicalLogBorders> logicalLog;

    if (logicalLog.base.sedna_db_version != SEDNA_DATA_STRUCTURES_VER) {
        throw USER_EXCEPTION2(SE4212, "Possibly your Sedna installation is newer than database files. You should use export utility (se_exp) to convert database into the latest format. See documentation for details.");
    };

    checkpointThread.start();

    check_tmp_file();

    Warden<LockManagerBorders> lockman;
    Warden<BufferManagerBorders> bufferManager;

    LSN last_checkpoint_lsn = LFS_INVALID_LSN;

    if (!logicalLog.base.is_stopped_correctly) {
        last_checkpoint_lsn = llRecoverPhysicalState();
        elog(EL_LOG, ("Database has been recovered by physical log successfully"));
    };

    recreate_tmp_file();
    logicalLog.base.disableCheckpoints();

    SSMMsgServerWrapper smvmmServer;
    smvmmServer.start();

    Warden<WuBorders> wu;

    if (!logicalLog.base.is_stopped_correctly)
    {
        execute_recovery_by_logical_log_process();
        elog(EL_LOG, ("Database has been recovered by logical log successfully"));
    }

    logicalLog.base.enableCheckpoints();

    smvmmServer.stop();
    checkpointThread.stop();

    is_recovery_mode = false;
};

int main(int argc, char **argv)
{
    /* Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL, so we must block SIGPIPE with sigignore.*/
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

    init_base_path(argv[0]);

    sp_int32 instruction;

    try {

        if (argc != 4) {
            throw SYSTEM_EXCEPTION("This application should be started by Sedna master process");
        };

        char sednaDataDirectory[SEDNA_DATA_VAR_SIZE] = {};

        CHECK_ENV(uGetEnvironmentVariable(SEDNA_DATA_ENVIRONMENT, sednaDataDirectory, SEDNA_DATA_VAR_SIZE, __sys_call_error),
            SE4074, SEDNA_DATA_ENVIRONMENT);

        SEDNA_DATA = sednaDataDirectory;

        /* Set global settings */
        GlobalObjectsCollector globalObjectsCollector;
        uSetGlobalNameGeneratorBase(SEDNA_DATA);

        /* Init event log, it should be safe by now */
        CHECK_ENV(event_logger_init(EL_SM, "", eventLogShmName, eventLogSemName),
            SE0001, "Failed to start event log");

        /* This should release event log on exception */
        struct EventLogWarden {
            ~EventLogWarden() { event_logger_release(); };
        };

        EventLogWarden eventLogReleaseWarden;
        GaintLockGlobalWarden gaintLock;

        /* Register with master process */
        MasterProcessConnection govConnection(argv[1], argv[2]);

        govConnection.setDatabaseOptions(databaseOptions);
        govConnection.registerOnGov(argv[3]); // Pass ticket
        govConnection.nextMessage();

        elog(EL_INFO, ("SM process connected"));
 
        instruction = govConnection.communicator->getInstruction();
 
        if (instruction != se_int_StartDatabaseInternal 
            && instruction != se_int_CreateDatabaseInternal
            && instruction != se_Stop) {
             throw SYSTEM_ENV_EXCEPTION("Invalid instruction from master process");
        } else if (instruction == se_Stop) {
             elog(EL_INFO, ("SM process has been shut down correctly"));
             return 0;
        } else {
            proto::StartDatabase startDb(*govConnection.communicator, govConnection.communicator->getInstruction());
            std::istringstream options(startDb.options);
            databaseOptions->loadFromStream(&options);
        }
 
         /* Set local settings */
        char envVariableDbId[64];
        snprintf(envVariableDbId, 64, "%d", databaseOptions->databaseId);
        uSetGlobalNameInstanceId(GN_DATABASE, envVariableDbId);

        CHECK_ENV(uSetEnvironmentVariable(SEDNA_DB_ID_ENVIRONMENT, envVariableDbId, NULL, __sys_call_error),
            SE4074, SEDNA_DB_ID_ENVIRONMENT);
 
        int sedna_db_version = SEDNA_DATA_STRUCTURES_VER;

        Warden<TransactionTableBorders> trtable;
        CheckpointThread checkpointThread;
        
        if (instruction == se_int_CreateDatabaseInternal) {
            initializeDatabase();
        } else {
            recover_database(checkpointThread);
        }

        Warden<LogicalLogBorders> logicalLog;

        checkpointThread.start();
        logicalLog.base.enableCheckpoints();

        //cleanup temporary files
        if(uCleanupUniqueFileStructs(databaseOptions->dataFilePath.c_str(), __sys_call_error) == 1) {
            elog(EL_LOG,  ("Temporary files have been deleted"));
        } else {
            elog(EL_WARN, ("Temporary files haven't been (or partially) deleted"));
        }

        Warden<LockManagerBorders> lockman;
        Warden<BufferManagerBorders> bufferManager;

        SSMMsgServerWrapper smvmmServer;
        elog(EL_INFO, ("Starting SSMMsg..."));
        smvmmServer.start();

        Warden<WuBorders> wu;

        /* Main thread */

        if (instruction == se_int_CreateDatabaseInternal) {
            loadMetadata();
        };

        elog(EL_LOG, ("SM has been started"));
        
        
        proto::OkAnswer(databaseOptions->databaseName) >> *(govConnection.communicator);
        
        govConnection.nextMessage();
        instruction = govConnection.communicator->getInstruction();
        if (instruction != se_Stop) {
            throw SYSTEM_ENV_EXCEPTION("Invalid instruction from master process");
        }
        
        smvmmServer.stop();
        checkpointThread.stop();
        
        elog(EL_INFO, ("SM process for database %s has been shut down", 
             govConnection.databaseOptions->databaseName.c_str()));
        
    } catch (SednaException & e) {
        elog(EL_FATAL, (e.what()));
        sedna_soft_fault(e, EL_SM);
        return 1;
    } catch (std::exception & e) {
        elog(EL_FATAL, (e.what()));
        sedna_soft_fault(EL_SM);
        return 1;
    }

    return 0;
}
