/*
 * File:  tr_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "term_globals.h"
#include "base.h"

// variables for parsing command line 

int term_s_help = 0;
int term_l_help = 0;
int term_version = 0;

int show_time = 0;
int socket_port = 0;
int echo = 1;

char host[TERM_ARGSTRLEN];
char db_name[TERM_ARGSTRLEN];
char filename[TERM_ARGSTRLEN];
char query[TERM_ARGSTRLEN];
//char q_type[1000];
//QueryType query_type = TL_XQuery;
char login[TERM_ARGSTRLEN];
char password[TERM_ARGSTRLEN];
FILE* res_os; //otput stream of term results (result of the user's queres)

const size_t narg = 13;

arg_rec term_argtable[] =
{
{"-help",            NULL,       arg_lit,   &term_s_help,                 "0",       "\t\t\t  display this help and exit"},
{"--help",           NULL,       arg_lit,   &term_l_help,                 "0",       "\t\t  display this help and exit"},
{"-version",         NULL,       arg_lit,   &term_version,                "0",       "\t\t  display product version and exit"},
{"-file",           " filename", arg_str,   &filename,                  "???",     "\t  file with an XQuery query\t\t\t  "},
{"-output",         " filename",     arg_str,   &output_file,               "STDOUT",  "\t\t  outputfile (default stdout)"},
{"-query",          " \"query\"",    arg_str,   &query,                   "???",     "\t\t  XQuery query to execute\t\t"},
{"-echo",           " on/off",   arg_bool,  &echo,                      "on",      "\t\t  display se_term output  (default on)"},
{"-showtime",       " on/off",   arg_bool,  &show_time,                 "off",     "\t  show time of the latest query execution (default off)"},
{"-host",           " host",     arg_str,   &host,                      "???",     "\t\t  hostname of the machine with Sedna running (default localhost)\n\t\t"},
{"-port-number",    " port",     arg_int,   &socket_port,               "5050",    "\t  socket listening port  (default 5050)"},
{"-name",           " name",     arg_str,   &login,                     "???",  "\t\t  user name "},
{"-pswd",           " password", arg_str,   &password,                  "???", "\t  user password "},
{NULL,              " db-name",  arg_str,   &db_name,                   "???",     "\t\t  database name"}
};

bool in_transaction = false; // in the scope of transaction
SednaConnection conn;
