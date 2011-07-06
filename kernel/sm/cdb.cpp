/*
 * File:  cdb.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <iostream>

#include "common/sedna.h"

#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/blk_mngmt.h"
#include "sm/bufmgr/bm_functions.h"
#include "sm/cdb_globals.h"
#include "sm/sm_functions.h"
#include "common/llcommon/llMain.h"
#include "sm/db_utils.h"
#include "common/errdbg/d_printf.h"
#include "common/u/uhdd.h"
#include "common/version.h"
#include "common/tr_debug.h"
#include "sm/trmgr.h"
#include "common/ugc.h"
#include "common/gmm.h"
#include "common/pping.h"
#include "common/ipc_ops.h"
#include "sm/wu/wu.h"
#include "common/commutil.h"

using namespace std;
using namespace cdb_globals;


void create_db()
{
    string db_common_path = string(sm_globals::db_files_path) + string(sm_globals::db_name);

    // create files
    USECURITY_ATTRIBUTES *sa;
    if(uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) throw USER_EXCEPTION(SE3060);

    string data_file_name = db_common_path + ".sedata";
    data_file_handler = uCreateFile(data_file_name.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, sa, __sys_call_error);
    if (data_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    string tmp_file_name = db_common_path + ".setmp";
    tmp_file_handler = uCreateFile(tmp_file_name.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, sa, __sys_call_error);
    if (tmp_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    // close created files
    if (uCloseFile(data_file_handler, __sys_call_error) == 0)
        throw USER_EXCEPTION(SE4301);

    if (uCloseFile(tmp_file_handler, __sys_call_error) == 0)
        throw USER_EXCEPTION(SE4301);

    event_logger_init(EL_CDB, sm_globals::db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
    elog(EL_LOG, ("Request for database creation"));

    /* This call is to debug transactions */
    CREATE_DEBUG_LOG(sm_globals::db_name);

    /* Allcoate master block */

    mb = (bm_masterblock*)(((uintptr_t)bm_master_block_buf + MASTER_BLOCK_SIZE) / MASTER_BLOCK_SIZE * MASTER_BLOCK_SIZE);

    /* Set master block initial values */
    mb->free_data_blocks = XNULL;
    mb->free_tmp_blocks  = XNULL;
    mb->data_file_cur_size = PAGE_SIZE;
    mb->tmp_file_cur_size  = 0;
    mb->data_file_max_size = MBS2PAGES(data_file_max_size);
    mb->tmp_file_max_size  = MBS2PAGES(tmp_file_max_size);
    mb->data_file_extending_portion = (int)MBS2PAGES(data_file_extending_portion);
    mb->tmp_file_extending_portion  = (int)MBS2PAGES(tmp_file_extending_portion);

    mb->transaction_flags = 0;
    if (strcmp(db_security, "authorization") == 0) {
        SET_FLAG(mb->transaction_flags, TR_AUTHORIZATION_FLAG);
        SET_FLAG(mb->transaction_flags, TR_AUTHENTICATION_FLAG);
    } else if (strcmp(db_security,"authentication") == 0) {
        SET_FLAG(mb->transaction_flags, TR_AUTHENTICATION_FLAG);
    }

    mb->catalog_masterdata_block = XNULL;
    mb->layer_size = LAYER_ADDRESS_SPACE_SIZE;

    // open files
    data_file_handler = uOpenFile(data_file_name.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, __sys_call_error);
    if (data_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    tmp_file_handler = uOpenFile(tmp_file_name.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, __sys_call_error);
    if (tmp_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION(SE4301);

    // write first page to data file, which serves as master block
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

/* If we can't perform cleanup the maximum we can do is to give user an advice. */
static inline void
cleanup_db_and_check_result()
{
    if(cleanup_db(sm_globals::db_name) == 2) {
        fprintf(stderr, "Cannot complete cleanup. Please stop Sedna and remove the following:\n");
        fprintf(stderr, "file   -  /cfg/%s_cfg.xml\n", sm_globals::db_name);
        fprintf(stderr, "folder -  /data/%s_files\n", sm_globals::db_name);
        fprintf(stderr, "Sorry for inconvenience.\n");
    }
}


int main(int argc, char **argv)
{
    program_name_argv_0 = argv[0];              // used in uGetImageProcPath
    pping_client *ppc = NULL;
    SednaUserException ppc_ex = USER_EXCEPTION(SE4400);
    bool is_bm_started = false;
    int db_id = -1;
    gov_header_struct cfg;

    /*Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
      so we must block SIGPIPE with sigignore.*/
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

    try {

#ifdef SE_MEMORY_MNG
        SafeMemoryContextInit();
#endif
        parse_cdb_command_line(argc, argv);

        get_sednaconf_values(&cfg);
        InitGlobalNames(cfg.os_primitives_id_min_bound, INT_MAX);
        SetGlobalNames();
        open_gov_shm();
        SEDNA_DATA = GOV_HEADER_GLOBAL_PTR -> SEDNA_DATA;

        setup_cdb_globals( GOV_CONFIG_GLOBAL_PTR );

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif

        /* Check if database already exists */
        string cfg_file_name = string(SEDNA_DATA) + string("/cfg/") +
                               string(sm_globals::db_name) + "_cfg.xml";

        if (uIsFileExist(sm_globals::db_files_path, __sys_call_error) ||
            uIsFileExist(cfg_file_name.c_str(), __sys_call_error))
        {
            string reason = "A database with the name '";
            reason +=  string(sm_globals::db_name);
            reason += "' already exists";
            throw USER_EXCEPTION2(SE4211, reason.c_str());
        }

        fprintf(res_os, "Creating a database (this can take a few minutes)...\n");

        db_id = get_next_free_db_id( GOV_CONFIG_GLOBAL_PTR );

        /*  There are no free cells? */
        if (db_id == -1)
            throw USER_EXCEPTION2(SE4211, "The maximum number of databases hosted by one server is exceeded");

        fill_database_cell_in_gov_shm(GOV_CONFIG_GLOBAL_PTR,
                                      db_id,
                                      sm_globals::db_name,
                                      sm_globals::bufs_num,
                                      sm_globals::upd_crt,
                                      sm_globals::max_log_files,
                                      (int)MBS2PAGES(sm_globals::tmp_file_initial_size));

        SetGlobalNamesDB(db_id);

        cdb_ugc(db_id, (GOV_HEADER_GLOBAL_PTR -> os_primitives_id_min_bound));


        try {
             if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3001);
             ppc = new pping_client(GOV_HEADER_GLOBAL_PTR -> ping_port_number, EL_CDB);
             ppc->startup(ppc_ex);

             d_printf1("determining size of the layer...");
             set_layer_parameters(determine_layer_size(db_id, cfg));
             d_printf1("ok\n");

             create_cfg_file();
             create_data_directory();
             create_db();

             d_printf1("create_db call successful\n");

             llCreateNew(sm_globals::db_files_path, sm_globals::db_name, (uint64_t)(log_file_size * 0x100000));

             init_checkpoint_sems();

             bool is_stopped_correctly;
             int sedna_db_version = 0;

             llInit(sm_globals::db_files_path,
                    sm_globals::db_name,
                    sm_globals::max_log_files,
                    &sedna_db_version,
                    &is_stopped_correctly,
                    false);

             d_printf1("logical_log_startup call successful\n");

             bm_startup();
             is_bm_started = true;
             d_printf1("bm_startup call successful\n");

             WuSetTimestamp(0x10000);

             extend_data_file((int)MBS2PAGES(cdb_globals::data_file_initial_size));
             d_printf1("extend_data_file call successful\n");

             extend_tmp_file ((int)MBS2PAGES(sm_globals::tmp_file_initial_size));
             d_printf1("extend_tmp_file call successful\n");

             is_bm_started = false;
             bm_shutdown();
             d_printf1("bm_shutdown call successful\n");

	         llRelease();
             d_printf1("logical_log_shutdown call successful\n");

             release_checkpoint_sems();

             if(load_metadata_in_database(sm_globals::db_name, db_security, cfg) != 0)
                 throw USER_EXCEPTION2(SE4211, sm_globals::db_name);

             elog(EL_LOG, ("Request for database creation satisfied"));
             event_logger_release();

             ppc->shutdown();
             delete ppc;
             ppc = NULL;

             close_gov_shm();

             if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3000);

             fprintf(res_os, "The database '%s' has been created successfully\n", sm_globals::db_name);
             fflush(res_os);

        } catch (SednaUserException &e) {
             fprintf(stderr, "%s\n", e.what());
             event_logger_release();
             if (ppc) { ppc->shutdown(); ppc = NULL; delete ppc; }
             if (is_bm_started) bm_shutdown();
             cleanup_db_and_check_result();
             uSocketCleanup(__sys_call_error);
             erase_database_cell_in_gov_shm(db_id, GOV_CONFIG_GLOBAL_PTR);
             close_gov_shm();
             return 1;
        } catch (SednaException &e) {
             if (is_bm_started) bm_shutdown();
             cleanup_db_and_check_result();
             sedna_soft_fault(e, EL_CDB);
        } catch (ANY_SE_EXCEPTION) {
             if (is_bm_started) bm_shutdown();
             cleanup_db_and_check_result();
             sedna_soft_fault(EL_CDB);
        }


    } catch (SednaUserException &e) {
        fprintf(stderr, "%s\n", e.what());
        erase_database_cell_in_gov_shm(db_id, GOV_CONFIG_GLOBAL_PTR);
        close_gov_shm();
        return 1;
    } catch (SednaException &e) {
        sedna_soft_fault(e, EL_CDB);
    } catch (ANY_SE_EXCEPTION) {
        sedna_soft_fault(EL_CDB);
    }


    return 0;
}

