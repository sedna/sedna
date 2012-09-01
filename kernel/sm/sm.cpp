/*
* File:  sm.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/
#include "sm/smtypes.h"

#include "common/lockmantypes.h"
#include "common/protocol/InternalConnection.h"
#include "common/globalobjects/globalnames.h"

#include "u/usem.h"
#include "u/usocket.h"
#include "u/uevent.h"
#include "u/ugnames.h"

#include "auxiliary/warden.h"

#include "sm/hb_utils.h"
#include "sm/trmgr.h"
#include "sm/lm/lm_globals.h"
#include "sm/bufmgr/bm_functions.h"
#include "sm/wu/wu.h"

#include "sm/smmsgserver.h"
#include "sm/llsm/physrcv.h"
#include "sm/lockwarden.h"
#include "sm/sm_globals.h"
#include "sm/cdb_utils.h"

#include <iostream>

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
    start() {
        init_transaction_ids_table();
        elog(EL_DBG, ("init_transaction_ids_table done"));
    };

    stop() {
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
    start() {
        lm_table.init_lock_table();
        elog(EL_DBG, ("init_lock_table done"));
    };

    stop() {
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


int main(int argc, char **argv)
{
    /* Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL, so we must block SIGPIPE with sigignore.*/
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

    init_base_path(argv[0]);

    if (argc != 3) {
        throw SYSTEM_EXCEPTION("This application should be started by Sedna master process");
    };

    char sednaDataDirectory[SEDNA_DATA_VAR_SIZE] = {};

    CHECK_ENV(uGetEnvironmentVariable(SEDNA_DATA_ENVIRONMENT, sednaDataDirectory, SEDNA_DATA_VAR_SIZE, __sys_call_error),
        SE4074, SEDNA_DATA_ENVIRONMENT);

    SEDNA_DATA = sednaDataDirectory;

    /* Set global settings */
    GlobalObjectsCollector globalObjectsCollector(sednaDataDirectory);
    uSetGlobalNameGeneratorBase(SEDNA_DATA);

    /* Init event log, it should be save by now */
    CHECK_ENV(event_logger_init(EL_SM, databaseOptions->databaseName.c_str(), eventLogShmName, eventLogSemName),
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

    /* Set local settings */
    char envVariableDbId[64];
    snprintf(envVariableDbId, 64, "%d", databaseOptions->databaseId);
    uSetGlobalNameInstanceId(GN_DATABASE, envVariableDbId);

    CHECK_ENV(uSetEnvironmentVariable(SEDNA_DB_ID_ENVIRONMENT, envVariableDbId, NULL, __sys_call_error),
        SE4074, SEDNA_DATA_ENVIRONMENT);

    int sedna_db_version = SEDNA_DATA_STRUCTURES_VER;

    try {
        Warden<TransactionTableBorders> trtable;
        CheckpointThread checkpointThread;
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

        if (govConnection.communicator->getInstruction() == );
        govConnection.nextMessage();
        if ()

        loadMetadata(cdb_params, &cfg);

        elog(EL_LOG, ("SM has been started"));

        smvmmServer.stop();
        checkpointThread.stop();
    } catch (SednaUserException &e) {
        return 1;
    } catch (SednaException &e) {
        sedna_soft_fault(e, EL_SM);
    } catch (ANY_SE_EXCEPTION) {
        sedna_soft_fault(EL_SM);
    }

    return 0;
}

void recover_database(int db_id)
{
    TransactionTableWrapper tridTable();
    CheckpointWrapper chkpointThread();

    elog(EL_LOG, ("Starting database recovery or hot-backup restoration..."));
    
    LogicalLogWrapper logicalLog();

    logicalLog.init();

    if (logicalLog.sedna_db_version != SEDNA_DATA_STRUCTURES_VER) {
        throw USER_EXCEPTION2(SE4212, "Possibly your Sedna installation is newer than database files. You should use export utility (se_exp) to convert database into the latest format. See documentation for details.");
    };

    chkpointThread.start();

    check_tmp_file();

    BufferManagerWrapper bufferManager();

    //recover data base by physical log
    LSN last_checkpoint_lsn = LFS_INVALID_LSN;

    if (!logicalLog.is_stopped_correctly) {
        last_checkpoint_lsn = llRecoverPhysicalState();
        elog(EL_LOG, ("Database has been recovered by physical log successfully"));
    };

    /* recover tmp file
     * we recreate it on usual start also since we want always to reset
     * its initial size
     */
    recreate_tmp_file();
    
    logicalLog.disableCheckpoints();

    LockTableWrapper lockTable();
    SSMMsgServerWrapper smMessageServerWrapper();
    
    smMessageServerWrapper.start();

    WuWrapper wuExn();

    if (!logicalLog.is_stopped_correctly)
    {
        execute_recovery_by_logical_log_process();
        elog(EL_LOG, ("Database has been recovered by logical log successfully"));
    }

    logicalLog.enableCheckpoints();

    smMessageServerWrapper.stop();
    chkpointThread.stop();
    wuExn.release();
};


void recover_database(int db_id)
{
    try {
        char buf[1024];
        bool is_stopped_correctly;
        int sedna_db_version = 0;

        if (uGetEnvironmentVariable(SM_BACKGROUND_MODE, buf, 1024, __sys_call_error) != 0)
        {//I am in running sm process

            is_recovery_mode = true;

            event_logger_init(EL_SM, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
            elog(EL_LOG, ("Event log in recovery procedure is ready"));

            //init transacion ids table
            init_transaction_ids_table();
            elog(EL_DBG, ("init_transaction_ids_table done"));

            //init checkpoint resources
            init_checkpoint_sems();
            elog(EL_DBG, ("init_checkpoint_sems done"));

            elog(EL_LOG, ("Starting database recovery or hot-backup restoration..."));
            fprintf(res_os, "Starting database recovery or hot-backup restoration...\n");
            llInit(db_files_path, db_name, max_log_files, &sedna_db_version, &is_stopped_correctly, true);
            elog(EL_DBG, ("logical log is started"));

            if (sedna_db_version != SEDNA_DATA_STRUCTURES_VER)
            {
                release_checkpoint_sems();
                release_transaction_ids_table();
                if (sedna_db_version != SEDNA_DATA_STRUCTURES_VER)
                    throw USER_EXCEPTION2(SE4212, "Possibly your Sedna installation is newer than database files. You should use export utility (se_exp) to convert database into the latest format. See documentation for details.");
            }

            d_printf1("logical log has been started successfully\n");

            //create checkpoint thread
            start_chekpoint_thread();
            elog(EL_DBG, ("start_chekpoint_thread done"));

            // check for tmp file (may be absent in hot-backup copy)
            string tmp_file_name = string(db_files_path) + string(db_name) + ".setmp";
            if (!uIsFileExist(tmp_file_name.c_str(), __sys_call_error))
            {
                USECURITY_ATTRIBUTES *sa;
                UFile tmp_file_handle;

                if (uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error) !=0 ) throw USER_EXCEPTION(SE3060);

                if ((tmp_file_handle = uCreateFile(tmp_file_name.c_str(), U_SHARE_READ, U_READ_WRITE, U_NO_BUFFERING, sa, __sys_call_error)) == U_INVALID_FD)
                    throw USER_EXCEPTION(SE4301);

                if (uCloseFile(tmp_file_handle, __sys_call_error) == 0)
                    throw USER_EXCEPTION(SE4305);

                if (uReleaseSA(sa, __sys_call_error) !=0 ) throw USER_EXCEPTION(SE3063);
            }

            //start buffer manager
            bm_startup(databaseOptions->bufferCount, sm_globals::db_files_path, sm_globals::db_name);
            elog(EL_LOG, ("Buffer manager is started"));

            //recover data base by physical log
            LSN last_checkpoint_lsn = LFS_INVALID_LSN;
            if (!is_stopped_correctly)
            {
                last_checkpoint_lsn = llRecoverPhysicalState();
                elog(EL_LOG, ("Database has been recovered by physical log successfully"));
            }

            /*
             * recover tmp file
             * we recreate it on usual start also since we want always to reset
             * its initial size
             */
            recreate_tmp_file();

            //disable checkpoints
            llDisableCheckpoints();
            elog(EL_LOG, ("Checkpoints are disabled"));

            lm_table.init_lock_table();
            elog(EL_DBG, ("lm_table.init_lock_table done"));

            // Starting SSMMsg server
            d_printf1("Starting SSMMsg...");

            smMessageServer = new SSMMsg(SSMMsg::Server,
                sizeof (sm_msg_struct),
                SEDNA_SSMMSG_SM_ID(db_id, buf, 1024),
                SM_NUMBER_OF_SERVER_THREADS,
                U_INFINITE);
            if (smMessageServer->init() != 0)
                throw USER_EXCEPTION(SE3030);

            if (smMessageServer->serve_clients(sm_server_handler) != 0)
                throw USER_EXCEPTION(SE3031);

            d_printf1("OK\n");

            WuSetTimestamp(llGetPersTimestamp() + 1);
            WuInitExn(0,0,llGetPersTimestamp()); // turn on versioning mechanism on recovery
            elog(EL_LOG, ("Wu is initialized"));

            //recover database by logical log
            if (!is_stopped_correctly)
            {
                execute_recovery_by_logical_log_process(last_checkpoint_lsn);
                elog(EL_LOG, ("Database has been recovered by logical log successfully"));
            }

            //enable checkpoints
            llEnableCheckpoints();
            elog(EL_LOG, ("Checkpoints are enabled"));

            if (smMessageServer->stop_serve_clients() != 0)
                throw USER_EXCEPTION(SE3032);

            if (smMessageServer->shutdown() != 0)
                throw USER_EXCEPTION(SE3033);

            //shutdown checkpoint thread (it also makes checkpoint)
            shutdown_chekpoint_thread(); // checkpont is created here!
            elog(EL_LOG, ("Shutdown checkpoint thread done"));

            WuReleaseExn();
            elog(EL_LOG, ("Wu is released"));

            // shutdown bm
            bm_shutdown(databaseOptions->bufferCount);
            elog(EL_LOG, ("Buffer manager is stopped"));

            //shutdown logical log
            llRelease();
            elog(EL_DBG, ("Logical log is stopped"));

            //release checkpoint resources
            release_checkpoint_sems();
            elog(EL_DBG, ("release_checkpoint_sems done"));

            release_transaction_ids_table();
            elog(EL_DBG, ("release_transaction_ids_table done"));

            lm_table.release_lock_table();
            elog(EL_DBG, ("lm_table.release_lock_table done"));

            elog(EL_LOG, ("Recovery procedure has been finished successfully"));
            event_logger_release();

            is_recovery_mode = false;
        }
    } catch (SednaUserException &e) {
        fprintf(stderr, "%s\n", e.what());
        throw USER_EXCEPTION(SE4205);
    } catch (SednaException&) {
        throw;
    } catch (ANY_SE_EXCEPTION) {
        throw;
    }

}
