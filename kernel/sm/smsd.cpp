/*
 * File:  smsd.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include <iostream>
#include <string.h>
#include "common/base.h"
#include "common/errdbg/d_printf.h"
#include "common/argtable.h"
#include "common/version.h"
#include "common/pping.h"
#include "common/u/usecurity.h"
#include "common/ipc_ops.h"

using namespace std;


const size_t narg = 4;
int smsd_help;
int smsd_version;
char db_name[1000];

arg_rec smsd_argtable[] =
{
{"--help",            NULL,       arg_lit,   &smsd_help,                 "0",    "\t\t   display this help and exit"},
{"-help",             NULL,       arg_lit,   &smsd_help,                 "0",    "\t\t\t   display this help and exit"},
{"-version",          NULL,       arg_lit,   &smsd_version,              "0",    "\t\t   display product version and exit"},
{NULL,               " db-name",  arg_str,   db_name,                    "???",  "\t\t   The name of the database "}
};


void print_smsd_usage()
{
   throw USER_SOFT_EXCEPTION((string("Usage: se_smsd [options] dbname \n\n") +
                              string("options:\n") + string(arg_glossary(smsd_argtable, narg, "  ")) + string("\n")).c_str());
}


#define SMSD_TIMEOUT		30000

int main(int argc, char **argv)
{
    program_name_argv_0 = argv[0];
    UShMem gov_mem_dsc;
    void* gov_shm_pointer = NULL;
    UPID sm_pid;
    int i=0;
    int port_number;
    bool exist_db = false;
    int command = STOP;
    int db_id;


    pping_client ppc(5151, EL_SMSD);

 
    try {

        SednaUserException e = USER_EXCEPTION(SE4400);

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif

        int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
        char errmsg[1000];
        arg_scan_ret_val = arg_scanargv(argc, argv, smsd_argtable, narg, NULL, errmsg, NULL);
        if (smsd_help == 1 ) print_smsd_usage();
        if (smsd_version == 1) { print_version_and_copyright("Sedna Shutdown Data Base Utility"); throw USER_SOFT_EXCEPTION(""); }
        if (arg_scan_ret_val == 0)
           throw USER_EXCEPTION2(SE4601, errmsg);


        if (string(db_name) == "???")
           throw USER_EXCEPTION2(SE4601, "The name of the database must be specified");
     
        d_printf1("Shutdowning SM... ");
        gov_header_struct cfg;
        get_gov_config_parameters_from_sednaconf(&cfg);//get config parameters from sednaconf
     
        set_global_names(cfg.os_primitives_id_min_bound);

        gov_shm_pointer = open_gov_shm(&gov_mem_dsc);

        SEDNA_DATA = ((gov_header_struct*)gov_shm_pointer)->SEDNA_DATA;
   
        db_id = get_db_id_by_name((gov_config_struct*)gov_shm_pointer, db_name);

        port_number = ((gov_header_struct*)gov_shm_pointer)->lstnr_port_number;


        if (db_id == -1)//there is no such database
        {
           close_gov_shm(gov_mem_dsc, gov_shm_pointer);
           goto end;
        }

        set_global_names(cfg.os_primitives_id_min_bound, db_id);

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3001);

        ppc.startup(e);

        event_logger_init(EL_SMSD, db_name, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
        elog(EL_LOG, ("Request for SM shutdown issued"));

        port_number = ((gov_header_struct*)gov_shm_pointer)->lstnr_port_number;

        int res;
        UPHANDLE proc_handle;

        sm_pid = ((gov_config_struct*)gov_shm_pointer)->db_vars[db_id].sm_pid;

        res = uOpenProcess(sm_pid, &proc_handle, __sys_call_error);
        if (res !=0) goto end;//sm already stopped

        ((gov_config_struct*)gov_shm_pointer)->db_vars[db_id].is_stop = 1;

        send_command_to_gov(port_number, command);
        uWaitForProcess(sm_pid, proc_handle, __sys_call_error);
        uCloseProcess(proc_handle, __sys_call_error);


        elog(EL_LOG, ("Request for SM shutdown satisfied"));

end:
        close_gov_shm(gov_mem_dsc, gov_shm_pointer);
        ppc.shutdown();

        if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3000);

        if (db_id != -1)
           fprintf(res_os, "The database '%s' has been successfully shut down\n", db_name);
        else
           fprintf(res_os, "There is no database with name '%s'\n", db_name);

        fflush(res_os);

        
    } catch (SednaUserException &e) { 
        fprintf(stderr, "%s\n", e.getMsg().c_str());
        event_logger_release();
        ppc.shutdown();
        return 1;
    } catch (SednaException &e) {
        sedna_soft_fault(e, EL_SMSD);
    } catch (...){
        sedna_soft_fault(EL_SMSD);
    }

    return 0;
}
