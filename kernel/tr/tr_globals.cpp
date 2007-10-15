/*
 * File:  tr_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/tr_globals.h"
#include "tr/tr_functions.h"
#include "common/base.h"
#include "tr/client_core.h"
#include "common/SSMMsg.h"

void *gov_shm_pointer = NULL;
// variables for parsing command line 
int tr_s_help = 0;
int tr_l_help = 0;
int tr_version = 0;

int run_rewriter = 1;			// run rewriter
int run_popt = 0; // whether turn on or turn off physical optimizer
int show_time = 0;
int print_intermed = 0;
int server_mode = 0;
int debug_mode = 0;
int socket_port = 0;
int write_phys_log = 1;
int user_id = 0;
int auth = AUTH_SWITCH;

char db_name[SE_MAX_DB_NAME_LENGTH+1];
char filename[TR_ARG_MAX_LENGTH+1];
char q_type[TR_ARG_MAX_LENGTH+1];
QueryType query_type = TL_XQuery;
char login[SE_MAX_LOGIN_LENGTH+1];
char password[SE_MAX_PASSWORD_LENGTH+1];
char output_file[TR_ARG_MAX_LENGTH+1];

const size_t narg = 13;

arg_rec tr_argtable[] =
{
{"-help",            NULL,       arg_lit,   &tr_s_help,                 "0",       "\t\t\t  display this help and exit"},
{"--help",           NULL,       arg_lit,   &tr_l_help,                 "0",       "\t\t  display this help and exit"},
{"-version",         NULL,       arg_lit,   &tr_version,                "0",       "\t\t  display product version and exit"},
{"-output",         " file",     arg_str,   output_file,                "STDOUT",  "\t\t  outputfile (default STDOUT)"},
{"-show-time",      " on/off",   arg_bool,  &show_time,                 "off",     "\t  show time of query execution (default off)"},
/* {"-rewriter",       " on/off",   arg_bool,  &run_rewriter,              "on",      "\t  run rewriter (default on)"}, */
{"-popt",  			" on/off",	 arg_bool,  &run_popt, 					"off",	   "\t\t  run physical optimizer (default off)"},
{"-print-intermed", " on/off",   arg_bool,  &print_intermed,            "off",     "  print intermediate results for debug purposes\n\t\t\t  (default off)"},
/* {"-server-mode",    " on/off",   arg_bool,  &server_mode,               "off",     "\t  work in server mode (output result to pipe)\n\t\t\t  (default off)"}, */
{"-query-type",     " type",     arg_str,   q_type,                     "XQuery",  "\t  type of the query to execute: XQuery, POR, Scheme, LR\n\t\t\t  (default XQuery)"},
{"-debug",          " on/off",   arg_bool,  &debug_mode,                "off",     "\t\t  execute statements in debug mode (default off)\t"},
{"-name",           " name",     arg_str,   login,                      "SYSTEM",  "\t\t  user name (default SYSTEM)"},
{"-pswd",           " password", arg_str,   password,                   "MANAGER", "\t  user password (default MANAGER)"},
{NULL,              " db-name",  arg_str,   db_name,                    "???",     "\t\t  database name"},
{NULL,              " filename", arg_str,   filename,                   "???",     "\t\t  file with an XQuery query\n\t\t\t  "}
};

client_core* client = NULL;
transaction_id trid = -1;
session_id sid = -1;
bool is_init_gov_shm = false;

UShMem gov_shm_dsc;
void* gov_shared_mem;

persistent_db_data* entry_point;

msg_struct sp_msg;

bool is_need_checkpoint_on_transaction_commit = false;

bool is_ro_mode = false, need_ph_reinit, is_ft_disabled;

int db_id;


