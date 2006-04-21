/*
 * File:  stop_server.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>
#include "SSMMsg.h"
#include "base.h"
#include "exceptions.h"
#include "usem.h"
#include "usocket.h"
#include "d_printf.h"
#include "argtable.h"
#include "version.h"
#include "pping.h"
#include "usecurity.h"
#include "ipc_ops.h"
#include "listener.h"

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
   throw USER_SOFT_EXCEPTION(string("Usage: se_stop [options]\n\n") +
                               string("options:\n") + string(arg_glossary(ss_argtable, narg, "  ")) + string("\n"));
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


    pping_client ppc(5151);


    try {

        SednaUserException e = USER_EXCEPTION(SE4400);
        if (uSocketInit() == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3001);


        int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
        char errmsg[1000];
        arg_scan_ret_val = arg_scanargv(argc, argv, ss_argtable, narg, NULL, errmsg, NULL);
        if (ss_help == 1 ) print_ss_usage();
        if (ss_version == 1) { print_version_and_copyright("Sedna Stop Server Utility"); throw USER_SOFT_EXCEPTION("");}

        if (arg_scan_ret_val == 0)
           throw USER_EXCEPTION2(SE4601, errmsg);


        set_global_names();

#ifdef REQUIRE_ROOT
        if (!uIsAdmin()) throw USER_EXCEPTION(SE3064);
#endif
        ppc.startup(e);
        ppc.shutdown();



        gov_shm_pointer = open_gov_shm(&gov_mem_dsc);

        ((gov_header_struct*)gov_shm_pointer)->is_server_stop = 1;
        gov_pid = ((gov_header_struct*)gov_shm_pointer)->gov_pid;
        port_number = ((gov_header_struct*)gov_shm_pointer)->lstnr_port_number;

        
        res = uOpenProcess(gov_pid, &proc_handle);
        if (res  != 0) goto end;

        send_command_to_gov(port_number, STOP);

        uWaitForProcess(gov_pid, proc_handle);
        uCloseProcess(proc_handle);
        if (uSocketCleanup() == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3000);


end:
        close_gov_shm(gov_mem_dsc, gov_shm_pointer);
        fprintf(res_os, "SEDNA server has been shut down successfully\n");
        fflush(res_os);
        return 0;

      } catch(SednaUserException &e) {
          fprintf(stderr, "%s\n", e.getMsg().c_str());
          return -1;
      } catch(SednaException &e) {
          sedna_soft_fault(e);
      } catch (...) {
          sedna_soft_fault();
      }
}

