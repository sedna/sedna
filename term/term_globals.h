/*
 * File:  tr_globals.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TERM_GLOBALS_H
#define _TERM_GLOBALS_H

#include "common/sedna.h"
#include "libsedna.h"
#include "common/argtable.h"
	
	
#define TERM_ARGSTRLEN 511
#define RESULT_MSG_SIZE 10235

extern int term_s_help;
extern int term_l_help;
extern int term_version;

extern int show_time;
extern int socket_port;
extern int echo;
extern int debug_output;
extern int debug_mode;
extern int interactive_mode;

extern char host[];
extern char db_name[];
extern char filename[];
extern char query[];
extern char login[];
extern char password[];
extern char output_file[];
extern char echo_str[];
extern char debug_indent[];
extern char prompt[];
extern char micro_prompt[];

extern FILE* res_os; //output stream of term results (result of the user's queres)

extern const int narg;
extern arg_rec term_argtable[];

extern bool on_error_stop;
extern char session_dir[];
extern SednaConnection conn;

#define EXIT_SUCCESS                       0
#define EXIT_TERM_FAILED                   1
#define EXIT_CONNECTION_BROKEN             2
#define EXIT_STATEMENT_OR_COMMAND_FAILED   3
#define EXIT_GOT_QUERY                     3
#define EXIT_GOT_COMMAND                   4
#define EXIT_EOF                           5
#define EXIT_GOT_LONG_QUERY                6
#define EXIT_ERROR                         7
#define EXIT_USER                          8

#endif
