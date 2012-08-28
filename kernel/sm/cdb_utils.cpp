#include "sm/smtypes.h"
#include "sm/bufmgr/blk_mngmt.h"
#include "sm/bufmgr/bm_functions.h"

#include "auxiliary/commutil.h"
#include "u/uutils.h"
#include "wu/wu.h"

#include <string>

using namespace std;

class SharedMemoryWarden
{
    UShMem shmem;
    global_name gname;
    char * _data;
public:
    SharedMemoryWarden(global_name gname, size_t sz)
    {
        if (uCreateShMem(&shmem, gname, sz, NULL, __sys_call_error) != 0) {
            throw USER_EXCEPTION(SE4016);
        };
        
        if (NULL == (_data = (char *) uAttachShMem(&gname, NULL, 0, __sys_call_error))) {
            throw USER_EXCEPTION(SE4023);
        }
    };

    ~SharedMemoryWarden() {
        if (uDettachShMem(&shmem, gname, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE4024);

        if (uReleaseShMem(&shmem, gname, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE4020);
    }

    char * data() { return _data; };
};

class ExecuteProcess
{
#define MAX_CMD_LINE (U_MAX_PATH * 3)
    UPID pid;
    UPHANDLE processHandle;

    char commandLine[MAX_CMD_LINE  + 1];
    bool processStarted;
public:
    ExecuteProcess(const char * basePath, const char * executable, const char * args)
      : processStarted(false)
    {
        if (basePath == NULL) {
            basePath = base_path;
        };

        if (args == NULL) {
            snprintf(commandLine, MAX_CMD_LINE, "%s"U_PATH_DELIMITER"%s", basePath, SESSION_EXE);
        } else {
            snprintf(commandLine, MAX_CMD_LINE, "%s"U_PATH_DELIMITER"%s %s", basePath, SESSION_EXE, args);
        };
    };

    ~ExecuteProcess()
    {
        release();
    };

    void execute(UFlag flags, bool inheritHandles)
    {
        if (0 != uCreateProcess(commandLine, inheritHandles, NULL,
            U_DETACHED_PROCESS, &processHandle, NULL,
                &pid, NULL, NULL, __sys_call_error))
        {
            throw USER_ENV_EXCEPTION("Cannot create process", false);
        }

        processStarted = true;
    };

    int wait4()
    {
        if (!processStarted) {
            throw USER_ENV_EXCEPTION("Process not started", false);
        };

        int statusCode = 0;

        if (0 != uWaitForChildProcess(pid, processHandle, &statusCode, __sys_call_error)) {
            throw USER_ENV_EXCEPTION("Cannot obtain process result", false);
        }

        return statusCode;
    };

    void release()
    {
        if (processStarted) {
            uCloseProcessHandle(processHandle, __sys_call_error);
            processStarted = false;
        }
    };
};

lsize_t determineLayerSize()
{
    /* SEDNA_DATA_ENVIRONMENT should be set */
    /* SEDNA_DB_ID_ENVIRONMENT should be set */

    // Result from transaction will be returned to this shared memory segment
    // So, we should create and attach it
    SharedMemoryWarden layerAddressMemory(LAYER_SIZE_RETURN_NAME, sizeof(lsize_t));

    /* setting hint size from command line */
    /* This is a safe cast since attached shared mem is always aligned */
    * (lsize_t *) layerAddressMemory.data() = (lsize_t) databaseOptions->layerSize * 1024 * 1024;

    try {
        ExecuteProcess determineVMM(NULL, SESSION_EXE, "--vmm-region");

        determineVMM.execute(U_DETACHED_PROCESS, false);

        if (determineVMM.wait4() != 0) {
            throw USER_ENV_EXCEPTION("Cannot create process to determine VMM region", false);
        };

        determineVMM.release();
    } catch (SednaUserEnvException & envE) {
        throw USER_ENV_EXCEPTION("Cannot create process to determine VMM region", false);
    };

    /* size of the layer should be right there */
    databaseOptions->layerSize = * (lsize_t *) layerAddressMemory.data();

    if (databaseOptions->layerSize < VMM_REGION_MIN_SIZE)
        throw USER_EXCEPTION2(SE1031, (std::string("Determined layer size: ") + int2string(LAYER_ADDRESS_SPACE_SIZE)).c_str());

    elog(EL_INFO, ("Layer address space size = 0x%x", databaseOptions->layerSize));

    return databaseOptions->layerSize;
}

void createDataDirectory()
{
   USECURITY_ATTRIBUTES *dir_sa;

   if (uCreateSA(&dir_sa, U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error) != 0)
   {
      throw USER_EXCEPTION(SE3060);
   }

   if (uMkDir(databaseOptions->dataFilePath.c_str(), dir_sa, __sys_call_error) == 0)
   {
      throw USER_EXCEPTION2(SE4300, databaseOptions->dataFilePath.c_str());
   }

   uReleaseSA(dir_sa, __sys_call_error);
}

void createInitialDbData()
{
    USECURITY_ATTRIBUTES *sa;

    if(uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error) != 0) {
        throw USER_EXCEPTION(SE3060);
    }

    /* Just create and close created files
     * QUESTION: Why do we need this?!
     */

    data_file_handle = uCreateFile(databaseOptions->dataFileName.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, sa, __sys_call_error);

    if (data_file_handle == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    tmp_file_handle = uCreateFile(databaseOptions->tmpFileName.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, sa, __sys_call_error);

    if (tmp_file_handle == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    if (uCloseFile(data_file_handle, __sys_call_error) == 0)
        throw USER_EXCEPTION(SE4301);

    if (uCloseFile(tmp_file_handle, __sys_call_error) == 0)
        throw USER_EXCEPTION(SE4301);

    /* Initialize masterblock */
    
    /* WTF ?? Allcoate master block */
    mb = (bm_masterblock*)(((uintptr_t) bm_master_block_buf + MASTER_BLOCK_SIZE) / MASTER_BLOCK_SIZE * MASTER_BLOCK_SIZE);

    memset(mb, MASTER_BLOCK_SIZE, 0);

    /* Set master block initial values */
    mb->data_file_cur_size = PAGE_SIZE;
    mb->data_file_max_size = MBS2PAGES(databaseOptions->dataFileSize.max);
    mb->tmp_file_max_size  = MBS2PAGES(databaseOptions->tmpFileSize.max);
    mb->data_file_extending_portion = (int) MBS2PAGES(databaseOptions->dataFileSize.extension);
    mb->tmp_file_extending_portion  = (int) MBS2PAGES(databaseOptions->tmpFileSize.extension);

    if (databaseOptions->securityOptions == se_security_authorization) {
        SET_FLAG(mb->transaction_flags, TR_AUTHORIZATION_FLAG);
        SET_FLAG(mb->transaction_flags, TR_AUTHENTICATION_FLAG);
    } else if (databaseOptions->securityOptions == se_security_authentication) {
        SET_FLAG(mb->transaction_flags, TR_AUTHENTICATION_FLAG);
    }

    mb->layer_size = LAYER_ADDRESS_SPACE_SIZE;

    /* Open data and tmp files */
    
    data_file_handle = uOpenFile(databaseOptions->dataFileName.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, __sys_call_error);

    if (data_file_handle == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    tmp_file_handle = uOpenFile(databaseOptions->tmpFileName.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, __sys_call_error);

    if (tmp_file_handle == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    /* write first page to data file, which serves as master block */
    char tmp[PAGE_SIZE];

    unsigned int number_of_bytes_written = 0;
    int res = uWriteFile(data_file_handle, tmp, PAGE_SIZE, &number_of_bytes_written, __sys_call_error);

    if (res == 0 || number_of_bytes_written != PAGE_SIZE) {
        throw USER_EXCEPTION2(SE4045, "data file");
    }

    // flush master block to disk
    flush_master_block();

    // close opened files
    if (uCloseFile(data_file_handle, __sys_call_error) == 0) {
        throw USER_EXCEPTION(SE4305);
    }

    if (uCloseFile(tmp_file_handle, __sys_call_error) == 0) {
        throw USER_EXCEPTION(SE4305);
    }

    if (uReleaseSA(sa, __sys_call_error) != 0) {
        throw USER_EXCEPTION(SE3063);
    }
}

void load_metadata()
{
    if(databaseOptions->securityOptions == se_security_off) {
        uSetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, "1", NULL, __sys_call_error);
    } else {
        uSetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, "2", NULL,  __sys_call_error);  
    }

    try {
        ExecuteProcess determineVMM(NULL, SESSION_EXE, "--load-metadata");

        /* WARNING: Why should inheritHandles be true here? */
        determineVMM.execute(U_DETACHED_PROCESS, true);

        if (determineVMM.wait4() != 0) {
            throw USER_ENV_EXCEPTION("Can't load metadata", false);
        };

        determineVMM.release();
    } catch (SednaUserEnvException & envE) {
        throw USER_ENV_EXCEPTION("Can't load metadata", false);
    };
}

void createDb() {
    LAYER_ADDRESS_SPACE_SIZE = determineLayerSize();
    elog(EL_INFO, ("Layer size determined successfully"));

    createDataDirectory();
    createInitialDbData();

    elog(EL_INFO, ("Create initial data call successful"));
    
    const char * db_files_path = databaseOptions->dataFilePath.c_str();

    llCreateNew(databaseOptions->dataFilePath.c_str(),
      databaseOptions->databaseName.c_str(),
      databaseOptions->logFileSize * 0x100000ULL);

    bool is_stopped_correctly;
    int sedna_db_version = 0;
 
    llInit(databaseOptions->dataFilePath.c_str(),
      databaseOptions->databaseName.c_str(),
      databaseOptions->maxLogFiles,
       &sedna_db_version,
       &is_stopped_correctly,
       false);

    d_printf1("logical_log_startup call successful\n");

    bm_startup(cdbParams->bufs_num, cdbParams->db_files_path, string(cdbParams->db_name));
//     is_bm_started = true;
    d_printf1("bm_startup call successful\n");

    WuSetTimestamp(0x10000);

    extend_data_file((int)MBS2PAGES(cdbParams->data_file_initial_size));
    d_printf1("extend_data_file call successful\n");

    extend_tmp_file ((int)MBS2PAGES(cdbParams->tmp_file_initial_size));
    d_printf1("extend_tmp_file call successful\n");

//              is_bm_started = false;
    bm_shutdown(cdbParams->bufs_num);
    d_printf1("bm_shutdown call successful\n");

    llRelease();
    d_printf1("logical_log_shutdown call successful\n");

    release_checkpoint_sems();

//     if(load_metadata_in_database(sm_globals::db_name, db_security, cfg) != 0)
//                  throw USER_EXCEPTION2(SE4211, sm_globals::db_name);


}
