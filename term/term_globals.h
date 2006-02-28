/*
 * File:  tr_globals.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TERM_GLOBALS_H
#define _TERM_GLOBALS_H

#include <stdio.h>

#include "base.h"
#include "argtable.h"
	
#include "libsedna.h"
	
#define TERM_ARGSTRLEN 511
#define RESULT_MSG_SIZE 10235

extern int term_s_help;
extern int term_l_help;
extern int term_version;

extern int show_time;
extern int socket_port;
extern int echo;

extern char host[];
extern char db_name[];
extern char filename[];
extern char query[];
extern char login[];
extern char password[];
extern char output_file[];
extern FILE* res_os; //output stream of term results (result of the user's queres)

extern const size_t narg;
extern arg_rec term_argtable[];

extern bool in_transaction;

extern SednaConnection conn;

#define EXIT_SUCCESS                    0
#define EXIT_GOT_QUERY                  1
#define EXIT_GOT_COMMAND                2
#define EXIT_EOF                        3
#define EXIT_GOT_LONG_QUERY             4
#define EXIT_ERROR                      5
#define EXIT_USER                       6
#define EXIT_COMMIT_FAILED              7
#define EXIT_ROLLBACK_FAILED            8
#define EXIT_NOT_COMMAND                9
#define EXIT                           10

#endif
