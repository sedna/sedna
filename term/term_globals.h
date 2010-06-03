/*
 * File:  term_globals.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TERM_GLOBALS_H
#define _TERM_GLOBALS_H

#include "common/sedna.h"
#include "libsedna.h"
#include "common/argtable.h"
	

#define TERM_ARGSTRLEN     511
#define RESULT_MSG_SIZE    10235


namespace term_globals 
{
    extern bool show_time;
    extern bool interactive_mode;
    extern bool debug_mode;
    extern bool on_error_stop;
    extern bool echo;
    extern bool debug_output;
    
    extern int socket_port;

    extern char host[];
    extern char db_name[];
    extern char filename[];
    extern char login[];
    extern char password[];
    extern char session_dir[];
    extern char query[];
    extern char prompt[];
    extern char micro_prompt[];
    
    extern SednaConnection conn;
}

/* Output stream of term results */
extern FILE* res_os;

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

#endif /*  _TERM_GLOBALS_H */
