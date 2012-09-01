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

int main(int argc, char** argv)
{
    program_name_argv_0 = argv[0];
    char buf[1024];
    char instance[16];
    /* Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
     * so we must block SIGPIPE with sigignore.
     */
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

    try {
        GlobalParameters sednaGlobalOptions;
        if ( parseSednaOptions(argc, argv, &sednaGlobalOptions, program_name_argv_0) ) { return 1; }

#ifdef EL_DEBUG
        std::cout << "\n\nFinal Sedna parameters are: \n";
        sednaGlobalOptions.saveToStream(&std::cout);     
#endif /* EL_DEBUG */
        
// !FIXME: do we really need SEDNA_DATA in this form?

        SEDNA_DATA = sednaGlobalOptions.global.dataDirectory.c_str();

        if (0 != uSetEnvironmentVariable(SEDNA_DATA_ENVIRONMENT, SEDNA_DATA, NULL, __sys_call_error)) {
            throw SYSTEM_EXCEPTION("Failed to set environment variable");
        };

        check_data_folder_existence(sednaGlobalOptions.global.dataDirectory.c_str());
        RenameLastSoftFaultDir();

        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR)
            throw SYSTEM_EXCEPTION("Failed to initialize socket library");

        GlobalObjectsCollector collector(sednaGlobalOptions.global.dataDirectory.c_str());
        uSetGlobalNameGeneratorBase(sednaGlobalOptions.global.dataDirectory.c_str());

        if (event_logger_start_daemon(sednaGlobalOptions.global.dataDirectory.c_str(),
              el_convert_log_level(sednaGlobalOptions.global.logLevel),
                eventLogShmName, eventLogSemName)) {
            throw SYSTEM_EXCEPTION("Failed to initialize event log");
        }

        log_out_system_information();
        srand(uGetCurrentProcessId(__sys_call_error));
        
        ProcessManager processManager(sednaGlobalOptions);
        Worker * govWorker = new Worker(&processManager);
        govWorker->createListener();
        govWorker->run();

        if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw SYSTEM_EXCEPTION("Failed to clean up socket library");

        elog(EL_LOG, ("SEDNA event log is down"));
        event_logger_shutdown_daemon(eventLogShmName);

        return 0;
    } catch (SednaException &e) {
        sedna_soft_fault(e, EL_GOV);
    } catch (ANY_SE_EXCEPTION) {
        sedna_soft_fault(EL_GOV);
    }

    return 0;
}
