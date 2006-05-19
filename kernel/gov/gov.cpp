/*
 * File:  gov.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <stdio.h>
#include <iostream>
#include <string>

#include "sedna.h"

#include "SSMMsg.h"
#include "usem.h"
#include "base.h"
#include "gov_globals.h"
#include "gov_functions.h"
#include "d_printf.h"
#include "pping.h"
#include "version.h"
#include "ugc.h"
#include "gov_table.h"
#include "listener.h"
#include "uprocess.h"
#include "gmm.h"
#include "ipc_ops.h"
#include "memutils.h"


#define GOV_BACKGROUND_MODE_TIMEOUT					15000
#define GOV_BACKGROUND_OFF_FROM_BACKGROUND_ON		"SEDNA_GOV_BACKGROUND_OFF_FROM_BACKGROUND_ON"

using namespace std;


void print_gov_usage()
{
    throw USER_SOFT_EXCEPTION((string("Usage: se_gov [options]\n\n") +
                               string("options:\n") + string(arg_glossary(gov_argtable, narg, "  ")) + string("\n")).c_str());
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
             Beep(1000, 1000);

             UShMem gov_mem_dsc;
             void* gov_shm_pointer = NULL;

             gov_shm_pointer = open_gov_shm(&gov_mem_dsc);
             ((gov_header_struct*)gov_shm_pointer)->is_server_stop = 1;
             close_gov_shm(gov_mem_dsc, gov_shm_pointer);

             send_command_to_gov(socket_port, STOP);

             return TRUE; 
        }
        default	: return FALSE; 
    } 
} 

#else

void GOVCtrlHandler(int signo)
{

  if (   signo == SIGINT 
      || signo == SIGQUIT
      || signo == SIGTERM) 
   {
       //beep();
       UShMem gov_mem_dsc;
       void* gov_shm_pointer = NULL;

       gov_shm_pointer = open_gov_shm(&gov_mem_dsc);
       ((gov_header_struct*)gov_shm_pointer)->is_server_stop = 1;
       close_gov_shm(gov_mem_dsc, gov_shm_pointer);

             send_command_to_gov(socket_port, STOP);

   }

}
#endif


int main(int argc, char** argv)
{
    program_name_argv_0 = argv[0];
    pping_server pps(5151);

    bool is_pps_close = true;

    try {
        SafeMemoryContextInit();

        int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
        char buf[1024];

        arg_scan_ret_val = arg_scanargv(argc, argv, gov_argtable, narg, NULL, buf, NULL);

        if (arg_scan_ret_val == 0)
            throw USER_EXCEPTION2(SE4601, buf);

        if (gov_help_l == 1 || gov_help_s == 1)
            print_gov_usage();

        if (gov_version == 1)
        {
           print_version_and_copyright("Sedna Governor");
           throw USER_SOFT_EXCEPTION("");
        }

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) 
            throw SYSTEM_EXCEPTION("Failed to initialize socket library");

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif

        set_global_names();

        if (!is_first_start_of_gov())
           throw USER_EXCEPTION(SE4408);

        bool background_off_from_background_on;
        if (uGetEnvironmentVariable(GOV_BACKGROUND_OFF_FROM_BACKGROUND_ON, buf, 1024, __sys_call_error) == 0)
            // we were started by command "gov -background-mode off" from "gov -background-mode on"
            background_off_from_background_on = true;
        else 
            // we were started by command "gov -background-mode on" or
            // "gov -background-mode off" directly
            background_off_from_background_on = false;


        clean_resources(background_off_from_background_on);

        if (background_off_from_background_on)
        {
            // we were started by command "gov -background-mode off" from "gov -background-mode on"
#ifdef _WIN32
#else
            // perform standard routines to run the process in the background mode
            setsid();
            chdir(SEDNA_DATA);
            //umask(0);
#endif
        }


        /////////////// BACKGROUND MODE ////////////////////////////////////////
        char *command_line_str = NULL;
        if (background_mode == 1)
        try {
            string command_line = argv[0];
            command_line += " -background-mode off";

            command_line_str = new char[command_line.length() + 1];
            strcpy(command_line_str, command_line.c_str());

            if (uSetEnvironmentVariable(GOV_BACKGROUND_OFF_FROM_BACKGROUND_ON, "1", __sys_call_error) != 0)
                throw USER_EXCEPTION2(SE4073, "GOV_BACKGROUND_OFF_FROM_BACKGROUND_ON");

            USemaphore started_sem;

            if (0 != USemaphoreCreate(&started_sem, 0, 1, CHARISMA_GOVERNOR_IS_READY, NULL, __sys_call_error))
                throw USER_EXCEPTION(SE4401);

            if (uCreateProcess(command_line_str, false, NULL, U_DETACHED_PROCESS, NULL, NULL, NULL, NULL, NULL, __sys_call_error) != 0)
                throw USER_EXCEPTION(SE4401);

            int res;
            res = USemaphoreDownTimeout(started_sem, GOV_BACKGROUND_MODE_TIMEOUT, __sys_call_error);


            USemaphoreRelease(started_sem, __sys_call_error);

            delete [] command_line_str;

            if (res != 0)
                throw USER_EXCEPTION(SE4401);


            fprintf(res_os, "GOVERNOR has been started in the background mode\n");
            fflush(res_os);

            return 0;

        } catch (SednaUserException &e) {
            fprintf(stderr, "%s\n", e.getMsg().c_str());
            return 1;            
        } catch (SednaException &e) {
            sedna_soft_fault(e);
        } catch (...) {
            sedna_soft_fault();
        }
        /////////////// BACKGROUND MODE ////////////////////////////////////////


        if (event_logger_start_daemon(EL_LOG, SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME))
            throw SYSTEM_EXCEPTION("Failed to initialize event log");

        elog(EL_LOG, ("SEDNA event log is ready"));


      gov_table = new info_table();
      gov_table->init(socket_port);

      create_global_memory_mapping();

      pps.startup();
      is_pps_close = false;
      d_printf1("ping started\n");


#ifdef _WIN32
      BOOL fSuccess; 
      fSuccess = SetConsoleCtrlHandler((PHANDLER_ROUTINE) GOVCtrlHandler, TRUE);                           // add to list 
      if (!fSuccess) throw USER_EXCEPTION(SE4403);
#else
        if ((int)signal(SIGINT, GOVCtrlHandler) == -1)
           throw USER_EXCEPTION(SE4403);
		// For Control-backslash
        if ((int)signal(SIGQUIT, GOVCtrlHandler) == -1)
           throw USER_EXCEPTION(SE4403);
		//For reboot or halt
        if ((int)signal(SIGTERM, GOVCtrlHandler) == -1)
           throw USER_EXCEPTION(SE4403);
#endif


      client_listener(background_off_from_background_on);

      gov_table->wait_all_notregistered_sess();

      pps.shutdown();
      is_pps_close = true;

      if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION("Failed to clean up socket library");

      gov_table->release();
      delete gov_table;


      release_global_memory_mapping();

      elog(EL_LOG, ("SEDNA event log is down"));
      event_logger_shutdown_daemon();


      fprintf(res_os, "GOVERNOR has been shut down successfully\n");
      fflush(res_os);
      return 0;

    } catch (SednaUserException &e) {
        fprintf(stderr, "%s\n", e.getMsg().c_str());
        event_logger_release();
        if (!is_pps_close) { pps.shutdown();}
        return 1;
    } catch (SednaException &e) {
        sedna_soft_fault(e);
    } catch (...) {
        sedna_soft_fault();
    }
    
    return 0;
}
