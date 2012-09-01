#include "cdb_utils.h"

#include "sm/bufmgr/blk_mngmt.h"
#include "sm/bufmgr/bm_functions.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/wu/wu.h"
#include "sm/sm_globals.h"

#include "auxiliary/commutil.h"
#include "auxiliary/cppcast.h"
#include "auxiliary/shmwarden.h"
#include "auxiliary/processwarden.h"

#include "u/uutils.h"
#include "u/uhdd.h"

#include "common/llcommon/llMain.h"
#include "common/structures/config_data.h"

#include <sp_defs.h>
#include <string>

using namespace std;

lsize_t determineLayerSize()
{
    /* SEDNA_DATA_ENVIRONMENT should be set */
    /* SEDNA_DB_ID_ENVIRONMENT should be set */

    // Result from transaction will be returned to this shared memory segment
    // So, we should create and attach it
    SharedMemoryWarden layerAddressMemory(layerSizeReturn, sizeof(lsize_t));

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
        throw USER_EXCEPTION2(SE1031, (std::string("Determined layer size: ") + cast_to_string(layerAddressSpaceSize)).c_str());

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

/** @brief Create files (data and tmp), write master block there
 */
void createInitialDbData()
{
    USECURITY_ATTRIBUTES *sa;

    if(uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error) != 0) {
        throw USER_EXCEPTION(SE3060);
    }

    /* Just create and close created files
     * WARNING: Why do we need this?!
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
    
    /* WARNING: WTF ?? Allcoate master block */
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

    mb->layer_size = layerAddressSpaceSize;

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

void loadMetadata()
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

    elog(EL_INFO, ("Load metadata successful"));
}

void initializeDatabase()
{
    layerAddressSpaceSize = determineLayerSize();
    elog(EL_INFO, ("Layer size determined successfully"));

    createDataDirectory();
    createInitialDbData();

    elog(EL_INFO, ("Create initial data call successful"));
    
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

    bm_startup();
    
    WuSetTimestamp(0x10000);

    extend_data_file((int)MBS2PAGES(databaseOptions->dataFileSize.initial));
    extend_tmp_file ((int)MBS2PAGES(databaseOptions->tmpFileSize.initial));
    
    bm_shutdown();

    llRelease();
}
