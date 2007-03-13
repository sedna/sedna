/*
 * File:  stop_server.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "common/base.h"
#include "common/errdbg/d_printf.h"
#include "common/argtable.h"
#include "common/version.h"
#include "common/pping.h"
#include "common/ipc_ops.h"
#include "common/u/uprocess.h"
#include "common/config.h"

using namespace std;


const size_t narg = 3;
int ss_help;
int ss_version;

arg_rec ss_argtable[] =
{
{"--help",            NULL,       arg_lit,   &ss_help,                 "0",    "\t\t   display this help and exit"},
{"-help",             NULL,       arg_lit,   &ss_help,                 "0",    "\t\t\t   display this help and exit"},
{"-version",          NULL,       arg_lit,   &ss_version,              "0",    "\t\t   display product version and exit"}
};


void print_ss_usage()
{
   throw USER_SOFT_EXCEPTION((string("Usage: se_stop [options]\n\n") +
                              string("options:\n") + string(arg_glossary(ss_argtable, narg, "  ")) + string("\n")).c_str());
}


int main(int argc, char** argv)
{
    USOCKET sock;
    UShMem gov_mem_dsc;
    void* gov_shm_pointer = NULL;
    UPID gov_pid;
    int port_number;
    UPHANDLE proc_handle;
    int res;
    program_name_argv_0 = argv[0];


    pping_client* ppc = NULL;


    try {

        SednaUserException e = USER_EXCEPTION(SE4400);
        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3001);

        int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
        char errmsg[1000];
        arg_scan_ret_val = arg_scanargv(argc, argv, ss_argtable, narg, NULL, errmsg, NULL);
        if (ss_help == 1 ) print_ss_usage();
        if (ss_version == 1) { print_version_and_copyright("Sedna Stop Server Utility"); throw USER_SOFT_EXCEPTION("");}

        if (arg_scan_ret_val == 0)
           throw USER_EXCEPTION2(SE4601, errmsg);

        gov_header_struct cfg;
        get_gov_config_parameters_from_sednaconf(&cfg);//get config parameters from sednaconf
     
        set_global_names(cfg.os_primitives_id_min_bound);

        gov_shm_pointer = open_gov_shm(&gov_mem_dsc);

        SEDNA_DATA = ((gov_header_struct*)gov_shm_pointer)->SEDNA_DATA;
   


#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif
        ppc = new pping_client(cfg.ping_port_number, EL_STOP);
        ppc->startup(e);

        event_logger_init(EL_STOP, NULL, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
        elog(EL_LOG, ("Request for GOVERNOR shutdown issued"));
        event_logger_release();

        ppc->shutdown();
        delete ppc;

        ((gov_config_struct*)gov_shm_pointer)->gov_vars.is_server_stop = 1;
        gov_pid = ((gov_config_struct*)gov_shm_pointer)->gov_vars.gov_pid;
        port_number = ((gov_config_struct*)gov_shm_pointer)->gov_vars.lstnr_port_number;

        
        res = uOpenProcess(gov_pid, &proc_handle, __sys_call_error);
        if (res  != 0) goto end;

        send_command_to_gov(port_number, STOP);

        uWaitForProcess(gov_pid, proc_handle, __sys_call_error);
        uCloseProcess(proc_handle, __sys_call_error);
        if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3000);


end:
        close_gov_shm(gov_mem_dsc, gov_shm_pointer);
        fprintf(res_os, "SEDNA server has been shut down successfully\n");
        fflush(res_os);
        return 0;

      } catch(SednaUserException &e) {
          fprintf(stderr, "%s\n", e.getMsg().c_str());
          close_gov_shm(gov_mem_dsc, gov_shm_pointer);
          return -1;
      } catch(SednaException &e) {
          sedna_soft_fault(e, EL_STOP);
      } catch (...) {
          sedna_soft_fault(EL_STOP);
      }
}

