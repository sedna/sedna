/*
 * File:  tr_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "term_globals.h"
#include "common/base.h"

// variables for parsing command line 

int term_s_help = 0;
int term_l_help = 0;
int term_version = 0;

int show_time = 0;
int socket_port = 0;
int echo = 1;
int debug_output = 1;
int debug_mode = 0;
int interactive_mode = 1;

char host[TERM_ARGSTRLEN+1];
char db_name[TERM_ARGSTRLEN+1];
char filename[TERM_ARGSTRLEN+1];
char query[TERM_ARGSTRLEN+1];
char login[TERM_ARGSTRLEN+1];
char password[TERM_ARGSTRLEN+1];
char output_file[TERM_ARGSTRLEN+1];
char echo_str[TERM_ARGSTRLEN+1];
char debug_indent[TERM_ARGSTRLEN+1];
char prompt[16]="> ";
char micro_prompt[16]="> ";

const int narg = 14;

arg_rec term_argtable[] =
{
{"-help",            NULL,       arg_lit,   &term_s_help,                 "0",       "\t\t\tdisplay this help and exit"},
{"--help",           NULL,       arg_lit,   &term_l_help,                 "0",       "\t\tdisplay this help and exit"},
{"-version",         NULL,       arg_lit,   &term_version,                "0",       "\t\tdisplay product version and exit"},
{"-file",           " filename", arg_str,   filename,                     "???",     "\tfile with an XQuery query"},
{"-output",         " filename", arg_str,   output_file,                  "STDOUT",  "\toutput file (default stdout)"},
{"-query",          " \"query\"",arg_str,   query,                        "???",     "\tXQuery query to execute\n"},
{"-echo",           " on/off",   arg_str,   echo_str,                     "???",     "\t\tdisplay se_term output\n\t\t\t(default: on for interactive mode,\n\t\t\t\t  off for batch mode)\n"},
{"-show-time",      " on/off",   arg_bool,  &show_time,                   "off",     "\tshow time of the latest query execution\n\t\t\t(default off)\n"},
{"-debug",          " on/off",   arg_bool,  &debug_mode,                  "off",     "\texecute statements in debug mode\n\t\t\t(default off)\n"},
{"-host",           " host",     arg_str,   host,                         "???",     "\t\thostname of the machine with Sedna running\n\t\t\t(default localhost)\n"},
{"-port-number",    " port",     arg_int,   &socket_port,                 "5050",    "\tsocket listening port (default 5050)"},
{"-name",           " name",     arg_str,   login,                        "???",     "\t\tuser name "},
{"-pswd",           " password", arg_str,   password,                     "???",     "\tuser password "},
{NULL,              " db-name",  arg_str,   db_name,                      "???",     "\t\tdatabase name"}
};

bool on_error_stop = false;

char session_dir[U_MAX_PATH+1];

SednaConnection conn = SEDNA_CONNECTION_INITIALIZER;
