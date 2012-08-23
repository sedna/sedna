#include "gov/cpool.h"
#include "gov/clients.h"

#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/blk_mngmt.h"
#include "sm/bufmgr/bm_functions.h"

#include "sm/sm_globals.h"
#include "sp_defs.h"

#include "common/errdbg/d_printf.h"
#include "common/llcommon/llMain.h"
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

lsize_t determineLayerSize()
{
    static char db_id[32];
    static char trn_command_line[U_MAX_PATH] = {};

    UPID pid;
    UPHANDLE process_handle;

    snprintf(db_id, 32, "%d", databaseOptions->databaseId);

    uSetEnvironmentVariable(SEDNA_DETERMINE_VMM_REGION, db_id, NULL, __sys_call_error);
    uSetEnvironmentVariable(SEDNA_OS_PRIMITIVES_ID_MIN_BOUND, SEDNA_DATA, NULL, __sys_call_error);
    
    // Result from transaction will be returned to this shared memory segment
    // So, we should create and attach it
    SharedMemoryWarden layerAddressMemory("LAYER_ADDRESS_RETURN", sizeof(lsize_t));

    snprintf(trn_command_line, U_MAX_PATH, "%s"U_PATH_DELIMITER"%s", base_path, SESSION_EXE);

    /* setting hint size from command line */
    /* This is a safe cast since attached shared mem is always aligned */
    * (lsize_t *) layerAddressMemory.data() = (lsize_t) databaseOptions->layerSize * 1024 * 1024;

    if (uCreateProcess(path_buf, false, NULL, U_DETACHED_PROCESS, &process_handle, NULL,
            &pid, NULL, NULL, __sys_call_error) != 0)
    {
        throw USER_ENV_EXCEPTION("Cannot create process to determine VMM region", false);
    }

    int status = 0;
    int res = 0;

    res = uWaitForChildProcess(pid, process_handle, &status, __sys_call_error);
    if (0 != res || status)
        throw USER_ENV_EXCEPTION((std::string("Cannot determine VMM region, status: ") + int2string(status) + ", result: " + int2string(res)).c_str(), false);

    uCloseProcessHandle(process_handle, __sys_call_error);

    uSetEnvironmentVariable(SEDNA_DETERMINE_VMM_REGION, "-1", NULL, __sys_call_error);

    /* size of the layer should be right there */
    databaseOptions->layerSize = * (lsize_t *) layerAddressMemory.data();

    if (databaseOptions->layerSize < VMM_REGION_MIN_SIZE)
        throw USER_EXCEPTION2(SE1031, (std::string("Determined layer size: ") + int2string(LAYER_ADDRESS_SPACE_SIZE)).c_str());

    elog(EL_INFO,  ("Layer address space size = 0x%x", databaseOptions->layerSize));

    return databaseOptions->layerSize;
}

void createCfgFile()
{
   char buf[100];
   unsigned int nbytes_written = 0;
   string cfg_file_content;
   UFile cfg_file_handle;
   USECURITY_ATTRIBUTES *def_sa, *dir_sa;

   if(uCreateSA(&dir_sa, U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) throw USER_EXCEPTION(SE3060);
   if (uMkDir(cdbParams->cfg_files_path.c_str(), dir_sa, __sys_call_error) == 0)
      throw USER_EXCEPTION2(SE4300, cdbParams->cfg_files_path.c_str());

   if(uCreateSA(&def_sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) throw USER_EXCEPTION(SE3060);
   cfg_file_handle = uCreateFile(cdbParams->cfg_file_name.c_str(),
                                    0,
                                    U_READ_WRITE,
                                    U_WRITE_THROUGH,
                                    def_sa, __sys_call_error);
   if (cfg_file_handle == U_INVALID_FD)
      throw USER_EXCEPTION2(SE4040, "database configuration file");

   uReleaseSA(def_sa, __sys_call_error);
   uReleaseSA(dir_sa, __sys_call_error);

   string db_name_str = replaceAll(string(cdbParams->db_name), "&", "&amp;");

   cfg_file_content =  "<?xml version=\"1.0\" standalone=\"yes\"?>\n";
   cfg_file_content += "<db>\n";
   cfg_file_content += "   <name>" + db_name_str + string("</name>\n");
   cfg_file_content += "   <bufs_num>" + int2string(cdbParams->bufs_num) + string("</bufs_num>\n");
   cfg_file_content += "   <max_log_files>" + int2string(cdbParams->max_log_files) + string("</max_log_files>\n");
   cfg_file_content += "   <tmp_file_initial_size>" + int2string(cdbParams->tmp_file_initial_size) + string("</tmp_file_initial_size>\n");

   sprintf(buf, "%.2f", cdbParams->upd_crt);

   cfg_file_content += "   <upd_crt>" + string(buf) + string("</upd_crt>\n");
   cfg_file_content += "</db>\n";

   int res = uWriteFile(cfg_file_handle,
                        cfg_file_content.c_str(),
                        (unsigned)cfg_file_content.size(),
                        &nbytes_written,
                        __sys_call_error);

   if ( res == 0 || nbytes_written != cfg_file_content.size())
      throw USER_EXCEPTION2(SE4045, cdbParams->cfg_file_name.c_str());

   if (uCloseFile(cfg_file_handle, __sys_call_error) == 0)
      throw USER_EXCEPTION2(SE4043, "configuration file");
}

void createDataDirectory(CdbParameters * cdbParams)
{
   USECURITY_ATTRIBUTES *dir_sa;

   if(uCreateSA(&dir_sa, U_SEDNA_DIRECTORY_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) throw USER_EXCEPTION(SE3060);

   if (uMkDir(cdbParams->data_files_path.c_str(), dir_sa, __sys_call_error) == 0)
      throw USER_EXCEPTION2(SE4300, cdbParams->data_files_path.c_str());

   if (uMkDir(cdbParams->db_files_path.c_str(), dir_sa, __sys_call_error) == 0)
      throw USER_EXCEPTION2(SE4300, cdbParams->db_files_path.c_str());

   uReleaseSA(dir_sa, __sys_call_error);
}

void createInitialDbData(CdbParameters * cdbParams)
{
//     create files
    USECURITY_ATTRIBUTES *sa;
    if(uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) throw USER_EXCEPTION(SE3060);

    data_file_handler = uCreateFile(cdbParams->data_file_name.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, sa, __sys_call_error);
    if (data_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    tmp_file_handler = uCreateFile(cdbParams->tmp_file_name.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, sa, __sys_call_error);
    if (tmp_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    //  close created files
    if (uCloseFile(data_file_handler, __sys_call_error) == 0)
        throw USER_EXCEPTION(SE4301);

    if (uCloseFile(tmp_file_handler, __sys_call_error) == 0)
        throw USER_EXCEPTION(SE4301);

//     event_logger_init(EL_CDB, cdbParams->db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
//     elog(EL_LOG, ("Request for database creation"));

    /* This call is to debug transactions */
    CREATE_DEBUG_LOG(cdbParams->db_name);

    /* Allcoate master block */

    mb = (bm_masterblock*)(((uintptr_t)bm_master_block_buf + MASTER_BLOCK_SIZE) / MASTER_BLOCK_SIZE * MASTER_BLOCK_SIZE);

    /* Set master block initial values */
    mb->free_data_blocks = XNULL;
    mb->free_tmp_blocks  = XNULL;
    mb->data_file_cur_size = PAGE_SIZE;
    mb->tmp_file_cur_size  = 0;
    mb->data_file_max_size = MBS2PAGES(cdbParams->data_file_max_size);
    mb->tmp_file_max_size  = MBS2PAGES(cdbParams->tmp_file_max_size);
    mb->data_file_extending_portion = (int)MBS2PAGES(cdbParams->data_file_extending_portion);
    mb->tmp_file_extending_portion  = (int)MBS2PAGES(cdbParams->tmp_file_extending_portion);

    mb->transaction_flags = 0;
    if (cdbParams->db_security == se_security_authorization) {
        SET_FLAG(mb->transaction_flags, TR_AUTHORIZATION_FLAG);
        SET_FLAG(mb->transaction_flags, TR_AUTHENTICATION_FLAG);
    } else if (cdbParams->db_security == se_security_authentication) {
        SET_FLAG(mb->transaction_flags, TR_AUTHENTICATION_FLAG);
    }

    mb->catalog_masterdata_block = XNULL;
    mb->layer_size = LAYER_ADDRESS_SPACE_SIZE;

    // open files
    data_file_handler = uOpenFile(cdbParams->data_file_name.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, __sys_call_error);
    if (data_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    tmp_file_handler = uOpenFile(cdbParams->tmp_file_name.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, __sys_call_error);
    if (tmp_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    //  write first page to data file, which serves as master block
    char tmp[PAGE_SIZE];

    unsigned int number_of_bytes_written = 0;
    int res = uWriteFile(data_file_handler, tmp, PAGE_SIZE, &number_of_bytes_written, __sys_call_error);
    if (res == 0 || number_of_bytes_written != PAGE_SIZE)
        throw USER_EXCEPTION2(SE4045, "data file");

    // flush master block to disk
    flush_master_block();

    // close opened files
    if (uCloseFile(data_file_handler, __sys_call_error) == 0)
        throw USER_EXCEPTION(SE4305);

    if (uCloseFile(tmp_file_handler, __sys_call_error) == 0)
        throw USER_EXCEPTION(SE4305);

    if(uReleaseSA(sa, __sys_call_error)!=0) throw USER_EXCEPTION(SE3063);
}

void load_metadata (CdbParameters * cdbParams, gov_header_struct * cfg) {
    int ret_status;
    int res;
    char buf[U_MAX_PATH + SE_MAX_DB_NAME_LENGTH + 16];
    UPID pid;
    UPHANDLE proc_h;
    string command_line;
    
    if(cdbParams->db_security == se_security_off) {
        uSetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, "1", NULL, __sys_call_error);
    } else {
        uSetEnvironmentVariable(SEDNA_LOAD_METADATA_TRANSACTION, "2", NULL,  __sys_call_error);  
    }
    
    command_line = constructClForTrn(cfg, cdbParams->db_name, cdbParams->db_id);

    strcpy(buf, command_line.c_str());
    if (0 != uCreateProcess(buf,
         true, // inherit handles
         NULL,
         0,     //U_DETACHED_PROCESS,
         &proc_h,
         NULL,
         &pid,
         NULL,
         NULL,
         __sys_call_error
         ))
         throw SYSTEM_EXCEPTION("Can't load metadata");

    res = uWaitForChildProcess(pid, proc_h, &ret_status, __sys_call_error);

    if (res != 0)
        throw SYSTEM_EXCEPTION("Can't load metadata");
    
    if (ret_status!=0)
        throw SYSTEM_EXCEPTION("Can't load metadata");

    return;
}


void createDb(CdbParameters * cdbParams, gov_header_struct * cfg) {
//     fill_database_cell_in_gov_shm(worker->cfg, cdbParams.db_id, cdbParams.db_name, cdbParams.bufs_num, cdbParams.upd_crt, cdbParams.max_log_files, cdbParams.tmp_file_initial_size);
    SetGlobalNamesDB(cdbParams->db_id);
    if (uSetEnvironmentVariable(SEDNA_SERVER_MODE, "0", NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4072, "SEDNA_SERVER_MODE");
    cdb_ugc(cdbParams->db_id, cfg->os_primitives_id_min_bound);
    d_printf1("determining size of the layer...");
    LAYER_ADDRESS_SPACE_SIZE = determineLayerSize(cdbParams, cfg);
    d_printf1("layer size determined successfully\n");
    createCfgFile(cdbParams);
    createDataDirectory(cdbParams);
    createInitialDbData(cdbParams);

    d_printf1("create initial data call successful\n");
    
    const char * db_files_path = cdbParams->db_files_path.c_str();
    llCreateNew(db_files_path, cdbParams->db_name, (uint64_t)(cdbParams->log_file_size * 0x100000));

    init_checkpoint_sems();
    bool is_stopped_correctly;
    int sedna_db_version = 0;
 
    llInit(db_files_path,
           cdbParams->db_name,
           cdbParams->max_log_files,
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
