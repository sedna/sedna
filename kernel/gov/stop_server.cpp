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

static const size_t narg = 4;
static int ss_help;
static int ss_version;
static int ss_hard;

static arg_rec ss_argtable[] =
{
  {"--help",            NULL,       arg_lit,   &ss_help,                 "0",    "\t\t   display this help and exit"},
  {"-help",             NULL,       arg_lit,   &ss_help,                 "0",    "\t\t\t   display this help and exit"},
  {"-version",          NULL,       arg_lit,   &ss_version,              "0",    "\t\t   display product version and exit"},
  {"-hard",             NULL,       arg_lit,   &ss_hard,                 "0",    "\t\t\t   attempt to roll back transactions immediately"}
};


static void print_ss_usage()
{
    fprintf(stdout, "Usage: se_stop [options]\n\n");
    fprintf(stdout, "options:\n%s\n", arg_glossary(ss_argtable, narg, "  ")); 
}

int main(int argc, char** argv)
{
    UPID gov_pid;
    int port_number;
    char gov_address[U_MAX_HOSTNAME];
    UPHANDLE proc_handle;
    int res;
    program_name_argv_0 = argv[0];

    pping_client* ppc = NULL;

    /* Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
     * so we must block SIGPIPE with sigignore.
     */
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

    try {

        SednaUserException e = USER_EXCEPTION(SE4400);
        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3001);

        int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
        char errmsg[1000];
        arg_scan_ret_val = arg_scanargv(argc, argv, ss_argtable, narg, NULL, errmsg, NULL);

        if (ss_help == 1 ) {
            print_ss_usage();
            return 0;
        }
        if (ss_version == 1) { 
            print_version_and_copyright("Sedna Stop Server Utility"); 
            return 0;
        }

        if (arg_scan_ret_val == 0)
           throw USER_EXCEPTION2(SE4601, errmsg);

        gov_header_struct cfg;
        get_sednaconf_values(&cfg);
     
                InitGlobalNames(cfg.os_primitives_id_min_bound, INT_MAX);
        SetGlobalNames();

        open_gov_shm();

        SEDNA_DATA = GOV_HEADER_GLOBAL_PTR -> SEDNA_DATA;

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif
        ppc = new pping_client(GOV_HEADER_GLOBAL_PTR -> ping_port_number, EL_STOP);
        ppc->startup(e);

        event_logger_init(EL_STOP, NULL, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME);
        elog(EL_LOG, ("Request for GOVERNOR shutdown issued"));
        event_logger_release();

        ppc->shutdown();
        delete ppc; 
        ppc = NULL;

        /* Set flag in gov shared memory that we want to stop Sedna. 
         * SE_STOP_SOFT (default) means that se_gov will wait for
         *              transactions are completed.
         * SE_STOP_HARD - se_gov attempts to immediately rollback all
         *                running transactions.
         */

        GOV_HEADER_GLOBAL_PTR -> is_server_stop = (ss_hard ? 
                                                   SE_STOP_HARD : 
                                                   SE_STOP_SOFT);

        gov_pid     = GOV_HEADER_GLOBAL_PTR -> gov_pid;
        port_number = GOV_HEADER_GLOBAL_PTR -> lstnr_port_number;
        strcpy(gov_address, GOV_HEADER_GLOBAL_PTR -> lstnr_addr);
        
        res = uOpenProcess(gov_pid, &proc_handle, __sys_call_error);
        if (res != 0) 
            throw USER_ENV_EXCEPTION("An error occurred while trying to open Sedna server process", false);
        
        send_command_to_gov(port_number, gov_address, STOP);

        uWaitForProcess(gov_pid, proc_handle, __sys_call_error);
        uCloseProcessHandle(proc_handle, __sys_call_error);
        if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION (SE3000);

        close_gov_shm();
        fprintf(res_os, "SEDNA server has been shut down successfully\n");
        fflush(res_os);
        return 0;

      } catch(SednaUserException &e) {
          fprintf(stderr, "%s\n", e.what());
          close_gov_shm();
          return -1;
      } catch(SednaException &e) {
          sedna_soft_fault(e, EL_STOP);
      } catch(ANY_SE_EXCEPTION) {
          sedna_soft_fault(EL_STOP);
      }
}

