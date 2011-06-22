/*
 * File:  gov_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "gov/gov_globals.h"

/*
 * NOTE! If you want to add some new sednaconf.xml global parameter
 * make sure that you also change linux_install.sh script appropriately.
 */

info_table *gov_table;

int background_mode = 0;
int gov_help_s = 0;
int gov_help_l = 0;
int gov_version = 0;

const int narg = 10;

/*
 * -1 means that parameter was not defined through command line
 * In ths case it will be set futher in fulfill_config_parameters().
 */
namespace gov_globals
{
    int  cl_el_level                     = -1;      // Event log severity level
    char cl_lstnr_addr[U_MAX_HOSTNAME]   = {"\0",}; // Governor listen address
    int  cl_lstnr_port                   = -1;      // Governor listen port
    int  cl_ping_port                    = -1;      // Process ping port
    int  cl_ka_timeout                   = -1;      // Session keep alive timeout
    int  cl_pp_stack_depth               = -1;
}


arg_rec gov_argtable[] =
{
{"--help",              NULL,        arg_lit,  &gov_help_l,                    "0",   "\t\t\t display this help and exit"},
{"-help",               NULL,        arg_lit,  &gov_help_s,                    "0",   "\t\t\t\t display this help and exit"},
{"-version",            NULL,        arg_lit,  &gov_version,                   "0",   "\t\t\t display product version and exit"},
{"-background-mode",    " on/off",   arg_bool, &background_mode,               "on",  "\t start in the background mode (default on)"},
{"-address-to-listen",  " interface",     arg_str,  &gov_globals::cl_lstnr_addr,    "localhost", "\t socket listening address or interface name (default localhost)"},
{"-port-number",        " port",     arg_int,  &gov_globals::cl_lstnr_port,    "-1",  "\t\t socket listening port (default 5050)"},
{"-ping-port-number",   " port",     arg_int,  &gov_globals::cl_ping_port,     "-1",  "\t ping listening port (default 5151)"},
{"-el-level",           " level",    arg_int,  &gov_globals::cl_el_level,      "-1",  "\t\t event logging level (default 3):\n\t\t\t\t    0 - event logging is off\
\n\t\t\t\t    1 - log only fatal errors\n\t\t\t\t    2 - log all errors/warnings\n\t\t\t\t    3 - system operational messages\
\n\t\t\t\t    4 - log everything (+debug messages)"},
{"-alive-timeout",      " timeout",  arg_int,  &gov_globals::cl_ka_timeout,    "-1",  "\t session keep alive timeout\n\t\t\t\t (default 0 - infinite timeout)"},
{"-stack-depth",        " depth",    arg_int,  &gov_globals::cl_pp_stack_depth,"-1",  "\t\t maximum executor stack depth\n\t\t\t\t (default 4000)"}
};

