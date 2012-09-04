/*
 * File:  gov.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/base.h"
#include "common/globalobjects/globalnames.h"
#include "common/procutils/version.h"
#include "common/errdbg/event_log.h"
#include "common/errdbg/d_printf.h"

#include "gov/cpool.h"
#include "gov/gov_globals.h"
#include "gov/gov_functions.h"

#include "u/ugnames.h"
#include "u/uutils.h"
#include "u/uprocess.h"


#ifdef EL_DEBUG
#include <iostream>
#endif /* EL_DEBUG */

GlobalObjectsCollector gboCollector;

void cleanup()
{
    gboCollector.cleanup();
};

bool shutdownIssued = false;

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
            shutdownIssued = true;
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
        shutdownIssued = true;
     }
}
#endif /* _WIN32 */


int main(int argc, char** argv)
{
    /* Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
     * so we must block SIGPIPE with sigignore.
     */
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

    try {
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

        
        init_base_path(argv[0]);
        GlobalParameters sednaGlobalOptions;

        if (parseSednaOptions(argc, argv, &sednaGlobalOptions, program_name_argv_0) ) { return 1; }

#ifdef EL_DEBUG
        std::cout << "\n\nFinal Sedna parameters are: \n";
        sednaGlobalOptions.saveToStream(&std::cout);
        std::cout << "\n";
#endif /* EL_DEBUG */

        if (*sednaGlobalOptions.global.dataDirectory.rbegin() != U_PATH_DELIMITER_C) {
            sednaGlobalOptions.global.dataDirectory += U_PATH_DELIMITER;
        };

        SEDNA_DATA = sednaGlobalOptions.global.dataDirectory.c_str();

        uSetGlobalNameGeneratorBase(SEDNA_DATA);

        if (event_logger_start_daemon(SEDNA_DATA, el_convert_log_level(sednaGlobalOptions.global.logLevel),
                eventLogShmName, eventLogSemName)) {
            throw SYSTEM_EXCEPTION("Failed to initialize event log");
        }

        if (0 != uSetEnvironmentVariable(SEDNA_DATA_ENVIRONMENT, SEDNA_DATA, NULL, __sys_call_error)) {
            throw SYSTEM_EXCEPTION("Failed to set environment variable");
        };

        check_data_folder_existence(sednaGlobalOptions.global.dataDirectory.c_str());
        RenameLastSoftFaultDir();

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR)
            throw SYSTEM_EXCEPTION("Failed to initialize socket library");

        log_out_system_information();
        srand(uGetCurrentProcessId(__sys_call_error));

        ProcessManager processManager(sednaGlobalOptions);
        Worker * govWorker = new Worker(&processManager);
        govWorker->createListener();
        govWorker->run();

        if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION("Failed to clean up socket library");

        elog(EL_LOG, ("SEDNA event log is down"));
        event_logger_shutdown_daemon(eventLogShmName);
        
    } catch (SednaException &e) {
        sedna_soft_fault(e, EL_GOV);
    } catch (ANY_SE_EXCEPTION) {
        sedna_soft_fault(EL_GOV);
    }

    return 0;
}
