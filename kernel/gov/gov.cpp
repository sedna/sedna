/*
 * File:  gov.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <stdio.h>
#include <iostream>
#include <string>

#include "common/sedna.h"

#include "common/u/usem.h"
#include "common/u/uprocess.h"
#include "common/SSMMsg.h"
#include "common/base.h"
#include "common/errdbg/d_printf.h"
#include "common/pping.h"
#include "common/version.h"
#include "common/ugc.h"
#include "common/gmm.h"
#include "common/ipc_ops.h"
#include "common/config.h"

#include "gov/gov_globals.h"
#include "gov/gov_functions.h"
#include "gov/gov_table.h"
#include "gov/listener.h"
#include "gov/config_utils.h"

#define GOV_BACKGROUND_MODE_TIMEOUT					15000
#define GOV_BACKGROUND_OFF_FROM_BACKGROUND_ON		"SEDNA_GOV_BACKGROUND_OFF_FROM_BACKGROUND_ON"

using namespace std;


static void print_gov_usage()
{
    fprintf(stdout, "Usage: se_gov [options]\n\n");
    fprintf(stdout, "options:\n%s\n", arg_glossary(gov_argtable, narg, "  "));
}

#ifdef _WIN32
BOOL GOVCtrlHandler(DWORD fdwCtrlType)
{
    switch (fdwCtrlType)
    {
        case CTRL_C_EVENT		: // Handle the CTRL+C signal.
        case CTRL_CLOSE_EVENT	: // CTRL+CLOSE: confirm that the user wants to exit.
        case CTRL_BREAK_EVENT	:
        case CTRL_LOGOFF_EVENT	:
        case CTRL_SHUTDOWN_EVENT:
        {
             // Beep(1000, 1000);
             open_gov_shm();
             GOV_HEADER_GLOBAL_PTR -> is_server_stop = SE_STOP_SOFT;
             send_command_to_gov(GOV_HEADER_GLOBAL_PTR -> lstnr_port_number, STOP);
             close_gov_shm();

             return TRUE;
        }
        default	: return FALSE;
    }
}

#else /* !_WIN32 */
void GOVCtrlHandler(int signo)
{

    if (   signo == SIGINT
        || signo == SIGQUIT
        || signo == SIGTERM)
     {
         // beep();
         open_gov_shm();
         GOV_HEADER_GLOBAL_PTR -> is_server_stop = SE_STOP_SOFT;
         send_command_to_gov(GOV_HEADER_GLOBAL_PTR -> lstnr_port_number, STOP);
         close_gov_shm();
     }
}
#endif /* _WIN32 */


int main(int argc, char** argv)
{
    program_name_argv_0 = argv[0];
    pping_server *pps = NULL;
    gov_config_struct cfg;
    bool is_pps_close = true;
    int arg_scan_ret_val = 0;
    char buf[1024];

    /* Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
     * so we must block SIGPIPE with sigignore.
     */
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

    try {
        /* Parse command line */
        arg_scan_ret_val = arg_scanargv(argc, argv, gov_argtable, narg, NULL, buf, NULL);

        if (arg_scan_ret_val == 0)
            throw USER_EXCEPTION2(SE4601, buf);

        /* Process technical arguments */
        if (gov_help_l == 1 || gov_help_s == 1)
        {
            print_gov_usage();
            return 0;
        }

        if (gov_version == 1)
        {
           print_version_and_copyright("Sedna Governor");
           return 0;
        }

        fulfill_config_parameters(&cfg);

        SEDNA_DATA = cfg.gov_vars.SEDNA_DATA;

        check_data_folder_existence();

        RenameLastSoftFaultDir();

        pps = new pping_server(cfg.gov_vars.ping_port_number, EL_GOV);

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR)
            throw SYSTEM_EXCEPTION("Failed to initialize socket library");

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif

		InitGlobalNames(cfg.gov_vars.os_primitives_id_min_bound,INT_MAX);
        SetGlobalNames();

        if (!is_first_start_of_gov(cfg.gov_vars.ping_port_number))
           throw USER_EXCEPTION(SE4408);

        bool background_off_from_background_on;
        if (uGetEnvironmentVariable(GOV_BACKGROUND_OFF_FROM_BACKGROUND_ON, buf, 1024, __sys_call_error) == 0)
            // we were started by command "gov -background-mode off" from "gov -background-mode on"
            background_off_from_background_on = true;
        else
            // we were started by command "gov -background-mode on" or
            // "gov -background-mode off" directly
            background_off_from_background_on = false;

        clean_resources(cfg, background_off_from_background_on);

        if (background_off_from_background_on)
        {
            // we were started by command "gov -background-mode off" from "gov -background-mode on"
#ifndef _WIN32
            // perform standard routines to run the process in the background mode
            setsid();
            //chdir(cfg.gov_vars.SEDNA_DATA);
            umask(0);
#endif
        }


        /////////////// BACKGROUND MODE ////////////////////////////////////////
        char *command_line_str = NULL;
        if (background_mode == 1)
        {
        try {
            string command_line = argv[0];

            command_line += " -background-mode off";
            command_line += " -port-number " + int2string(cfg.gov_vars.lstnr_port_number);
            command_line += " -ping-port-number " + int2string(cfg.gov_vars.ping_port_number);
            command_line += " -el-level " + int2string(cfg.gov_vars.el_level);
            command_line += " -alive-timeout " + int2string(cfg.gov_vars.ka_timeout);
            command_line += " -stack-depth "   + int2string(cfg.gov_vars.pp_stack_depth);

            command_line_str = new char[command_line.length() + 1];
            strcpy(command_line_str, command_line.c_str());

            if (uSetEnvironmentVariable(GOV_BACKGROUND_OFF_FROM_BACKGROUND_ON, "1", NULL, __sys_call_error) != 0)
                throw USER_EXCEPTION2(SE4072, "GOV_BACKGROUND_OFF_FROM_BACKGROUND_ON");

            USemaphore started_sem;

            if (0 != USemaphoreCreate(&started_sem, 0, 1, CHARISMA_GOVERNOR_IS_READY, NULL, __sys_call_error))
                throw USER_EXCEPTION(SE4401);

#ifdef _WIN32
            if (uCreateProcess(command_line_str, false, NULL, CREATE_NEW_CONSOLE, NULL, NULL, NULL, NULL, NULL, __sys_call_error) != 0)
#else
            if (uCreateProcess(command_line_str, false, NULL, U_DETACHED_PROCESS, NULL, NULL, NULL, NULL, NULL, __sys_call_error) != 0)
#endif
                throw USER_EXCEPTION(SE4401);

            int res = USemaphoreDownTimeout(started_sem, GOV_BACKGROUND_MODE_TIMEOUT, __sys_call_error);


            USemaphoreRelease(started_sem, __sys_call_error);

            delete [] command_line_str;

            if (res != 0)
                throw USER_EXCEPTION(SE4401);


            fprintf(res_os, "GOVERNOR has been started in the background mode\n");
            fflush(res_os);

            return 0;

        } catch (SednaUserException &e) {
            fprintf(stderr, "%s\n", e.what());
            return 1;
        } catch (SednaException &e) {
            sedna_soft_fault(e, EL_GOV);
        } catch (ANY_SE_EXCEPTION) {
            sedna_soft_fault(EL_GOV);
        }
        }
        /////////////// BACKGROUND MODE ////////////////////////////////////////

      if (event_logger_start_daemon(el_convert_log_level(cfg.gov_vars.el_level), SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME))
          throw SYSTEM_EXCEPTION("Failed to initialize event log");

      gov_table = new info_table();
      gov_table->init(&cfg);

      log_out_system_information();

      create_global_memory_mapping(gov_table->get_config_struct()->gov_vars.os_primitives_id_min_bound);

      pps->startup();
      is_pps_close = false;

      d_printf1("Process ping server has been started\n");
      elog(EL_LOG, ("Process ping server is ready"));

#ifdef _WIN32
      BOOL fSuccess;
      SetProcessShutdownParameters(0x3FF, 0);
      fSuccess = SetConsoleCtrlHandler((PHANDLER_ROUTINE) GOVCtrlHandler, TRUE);                           // add to list
      if (!fSuccess) throw USER_EXCEPTION(SE4403);
#else
        if (signal(SIGINT, GOVCtrlHandler) == SIG_ERR)
           throw USER_EXCEPTION(SE4403);
		// For Control-backslash
        if (signal(SIGQUIT, GOVCtrlHandler) == SIG_ERR)
           throw USER_EXCEPTION(SE4403);
		//For reboot or halt
        if (signal(SIGTERM, GOVCtrlHandler) == SIG_ERR)
           throw USER_EXCEPTION(SE4403);
#endif

      set_session_common_environment();

      client_listener(gov_table->get_config_struct(), background_off_from_background_on);

      gov_table->wait_all_notregistered_sess();

      pps->shutdown();
      delete pps;
      pps = NULL;
      is_pps_close = true;

      if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION("Failed to clean up socket library");

      gov_table->release();
      delete gov_table;

      release_global_memory_mapping();

      elog(EL_LOG, ("SEDNA event log is down"));
      event_logger_shutdown_daemon(SE_EVENT_LOG_SHARED_MEMORY_NAME);


      fprintf(res_os, "GOVERNOR has been shut down successfully\n");
      fflush(res_os);
      return 0;

    } catch (SednaUserException &e) {
        fprintf(stderr, "%s\n", e.what());
        event_logger_release();
        if (!is_pps_close) { if (pps) pps->shutdown();}
        return 1;
    } catch (SednaException &e) {
        sedna_soft_fault(e, EL_GOV);
    } catch (ANY_SE_EXCEPTION) {
        sedna_soft_fault(EL_GOV);
    }

    return 0;
}
