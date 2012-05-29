/*
 * File:  gov.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/errdbg/d_printf.h"

#include "gov/cpool.h"
#include "gov/gov_globals.h"
#include "gov/gov_functions.h"
#include "common/procutils/version.h"

static void print_gov_usage()
{
    fprintf(stdout, "Usage: se_gov [options]\n\n");
    arg_print_syntax(stdout, gov_argtable, "\n");
    arg_print_glossary(stdout, gov_argtable, "  %-25s $s\n");
}

#ifdef _WIN32
BOOL GOVCtrlHandler(DWORD fdwCtrlType)
{
    switch (fdwCtrlType)
    {
        case CTRL_C_EVENT               : // Handle the CTRL+C signal.
        case CTRL_CLOSE_EVENT   : // CTRL+CLOSE: confirm that the user wants to exit.
        case CTRL_BREAK_EVENT   :
        case CTRL_LOGOFF_EVENT  :
        case CTRL_SHUTDOWN_EVENT:
        {
             return TRUE;
        }
        default : return FALSE;
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
     }
}
#endif /* _WIN32 */


int main(int argc, char** argv)
{
    program_name_argv_0 = argv[0];
    bool is_pps_close = true;
    char buf[1024];

    /* Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
     * so we must block SIGPIPE with sigignore.
     */
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

    try {
        GlobalParameters sednaGlobalOptions;
        if ( parseSednaOptions(argc, argv, &sednaGlobalOptions) ) { return 1; }

// !FIXME: do we really need SEDNA_DATA in this form?
//  SEDNA_DATA = ..... = sednaGlobalOptions.global.dataDirectory;

        check_data_folder_existence();

        RenameLastSoftFaultDir();

//         pps = new pping_server(cfg.gov_vars.ping_port_number, EL_GOV);

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR)
            throw SYSTEM_EXCEPTION("Failed to initialize socket library");

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif

// TODO:        InitGlobalNames(os_primitives_id_min_bound,INT_MAX);
//              SetGlobalNames();

      if (event_logger_start_daemon(el_convert_log_level(cfg.gov_vars.el_level), SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME))
          throw SYSTEM_EXCEPTION("Failed to initialize event log");

      log_out_system_information();

      create_global_memory_mapping(sednaGlobalOptions.global.osObjectsOffset);


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
     
      Worker * govWorker = new Worker(&cfg);
      govWorker->createListener();
      govWorker->run();


      if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION("Failed to clean up socket library");

      release_global_memory_mapping();

      elog(EL_LOG, ("SEDNA event log is down"));
      event_logger_shutdown_daemon(SE_EVENT_LOG_SHARED_MEMORY_NAME);


      fprintf(res_os, "GOVERNOR has been shut down successfully\n");
      fflush(res_os);
      return 0;

    } catch (SednaUserException &e) {
        fprintf(stderr, "%s\n", e.what());
        event_logger_release();

        return 1;
    } catch (SednaException &e) {
        sedna_soft_fault(e, EL_GOV);
    } catch (ANY_SE_EXCEPTION) {
        sedna_soft_fault(EL_GOV);
    }

    return 0;
}
