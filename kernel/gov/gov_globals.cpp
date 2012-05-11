/*
 * File:  gov_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "gov/gov_globals.h"
#include "aux/argtable2/argtable2.h"


/* !TODO: tune linux_install.sh (or make some more clear procedure to install sedna */

int background_mode = 0;
int gov_help_s = 0;
int gov_help_l = 0;

/*
 * -1 means that parameter was not defined through command line
 * In ths case it will be set futher in fulfill_config_parameters().
 */
namespace gov_globals
{
    /* TODO: make gov able to listen multiple addresses and define a max ifaces constant */
    struct arg_lit * help               = arg_lit0("h","help",                    "print this help and exit");
    struct arg_str * cl_data_dir     	= arg_file0("i","data directory", "<path>", "Path to the directory containing databases and cfg directories");
    struct arg_str * cl_lstnr_addr      = arg_strn("l","listen-addresses","\"address1\"<,\"address2\",..",0,10, 
                                                   "local address (or addresses) Sedna listens for client connections (default localhost)" ); // Governor listen address (or addresses)
    struct arg_int * cl_lstnr_port      = arg_int0("p","port-to-listen", "<int>", "socket listening port");
    struct arg_int * cl_el_level        = arg_int0("d","event-logging-verbosity-level", NULL, "event log verbosity level: 0 - event logging is off, 1 - log only fatal errors, 2 - log all errors, 3 - system operational messages");
    struct arg_int * cl_ka_timeout      = arg_int0("a","keep-alive-timeout", "<int>", "session keep alive timeout (default 0 - infinite timeout)");
    struct arg_int * cl_pp_stack_depth  = arg_int0("d","stack-depth", "<int>", "session keep alive timeout (default 0 - infinite timeout)");
}