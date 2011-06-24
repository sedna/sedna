/*
 * File:  hot_backup.cpp - Main file for hot-backup utility
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <iostream>
#include <string.h>
#include "common/base.h"
#include "common/errdbg/d_printf.h"
#include "common/argtable.h"
#include "common/version.h"
#include "common/u/usecurity.h"
#include "common/u/usocket.h"
#include "common/ipc_ops.h"

#include "hb_aux.h"
#include "hb_main.h"
#include "hb_files.h"

using namespace std;

int main(int argc, char **argv)
{
    program_name_argv_0 = argv[0];

    /*Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
      so we must block SIGPIPE with sigignore.*/
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif
 
    try {

#ifdef REQUIRE_ROOT
        if (!uIsAdmin(__sys_call_error)) throw USER_EXCEPTION(SE3064);
#endif
                // get default parameters from sednaconf (port number)
                hbGetDefaultValues();

        // parsing parameters
                hbParseCommandLine(argc, argv);
   
        // init socket subsystem (needed for communication with gov)
        if (uSocketInit(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3001);

        // doing hot-backup
        hbMainProcedure(hb_dir_name, hb_db_name, hb_address, hb_port, hb_checkpoint);
        
        if (uSocketCleanup(__sys_call_error) == U_SOCKET_ERROR) throw USER_EXCEPTION(SE3000);

        printf("The database '%s' has been successfully backed up\n", hb_db_name);
    } 
    // in case of any exception we don't want to crash sedna since hot-backup is an external utility
    // so we must try to do proper deinitialization
    catch (SednaException &e) {        
        fprintf(stderr, "%s\n", e.what());

            // make backup directory cleanup
        hbMakeCleanup();

        return 1;
    } 
    catch (ANY_SE_EXCEPTION) {
        fprintf(stderr, "Unknown error during hot-backup process!\n");

            // make backup directory cleanup
        hbMakeCleanup();

        return 2;
    }

    return 0;
}
