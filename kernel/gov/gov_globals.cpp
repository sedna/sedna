/*
 * File:  gov_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "gov/gov_globals.h"
#include "aux/argtable2/argtable2.h"

/*
 * NOTE! If you want to add some new sednaconf.xml global parameter
 * make sure that you also change linux_install.sh script appropriately.
 */


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
    struct arg_str * cl_install_dir     = arg_file0("i","installation-directory", "<path>", "Path to the directory containing data and cfg directories");
    struct arg_str * cl_lstnr_addr      = arg_strn("l","listen-addresses","\"address1\"<,\"address2\",..",0,10, 
                                                   "local address (or addresses) Sedna listens for client connections (default localhost)" ); // Governor listen address (or addresses)
    struct arg_int * cl_lstnr_port      = arg_int0("p","port-to-listen", "<int>", "socket listening port");
    struct arg_int * cl_el_level        = arg_int0("d","event-logging-verbosity-level", NULL, "event log verbosity level: 0 - event logging is off, 1 - log only fatal errors, 2 - log all errors, 3 - system operational messages");
    struct arg_int * cl_ka_timeout      = arg_int0("a","keep-alive-timeout", "<int>", "session keep alive timeout (default 0 - infinite timeout)");
    struct arg_int * cl_pp_stack_depth  = arg_int0("d","stack-depth", "<int>", "session keep alive timeout (default 0 - infinite timeout)");
}

/*
arg_rec gov_argtable[] =
{
{"--help",              NULL,        arg_lit,  &gov_help_l,                    "0",   "\t\t\t display this help and exit"},
{"-help",               NULL,        arg_lit,  &gov_help_s,                    "0",   "\t\t\t\t display this help and exit"},
{"-version",            NULL,        arg_lit,  &gov_version,                   "0",   "\t\t\t display product version and exit"},
{"-background-mode",    " on/off",   arg_bool, &background_mode,               "on",  "\t start in the background mode (default on)"},
{"-listen-address",  " address",     arg_str,  &gov_globals::cl_lstnr_addr,    "localhost", "\t local address Sedna listens for client connections (default localhost)"},
{"-port-number",        " port",     arg_int,  &gov_globals::cl_lstnr_port,    "-1",  "\t\t socket listening port (default 5050)"},
{"-ping-port-number",   " port",     arg_int,  &gov_globals::cl_ping_port,     "-1",  "\t ping listening port (default 5151)"},
{"-el-level",           " level",    arg_int,  &gov_globals::cl_el_level,      "-1",  "\t\t event logging level (default 3):\n\t\t\t\t    0 - event logging is off\
\n\t\t\t\t    1 - log only fatal errors\n\t\t\t\t    2 - log all errors/warnings\n\t\t\t\t    3 - system operational messages\
\n\t\t\t\t    4 - log everything (+debug messages)"},
{"-alive-timeout",      " timeout",  arg_int,  &gov_globals::cl_ka_timeout,    "-1",  "\t session keep alive timeout\n\t\t\t\t (default 0 - infinite timeout)"},
{"-stack-depth",        " depth",    arg_int,  &gov_globals::cl_pp_stack_depth,"-1",  "\t\t maximum executor stack depth\n\t\t\t\t (default 4000)"}
};
*/

void * gov_argtable[] = {gov_globals::cl_install_dir, gov_globals::cl_lstnr_addr, gov_globals::cl_lstnr_port, gov_globals::cl_el_level,
                         gov_globals::cl_ka_timeout, gov_globals::cl_pp_stack_depth};