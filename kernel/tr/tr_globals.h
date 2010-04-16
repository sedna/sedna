/*
 * File:  tr_globals.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _TR_GLOBALS_H
#define _TR_GLOBALS_H

#include "common/sedna.h"
#include "common/base.h"
#include "common/pping.h"
#include "tr/client_core.h"
#include "tr/tr_base.h"

#define ENV_BUF_SIZE 1000

void parse_trn_command_line(int argc, char** argv);

namespace tr_globals
{
    extern int run_rewriter;
    extern int show_time;
    extern int socket_port;
    extern int print_intermed;
    extern int debug_mode;
    extern int authentication;
    extern int authorization;
    extern int query_timeout;
    extern int max_stack_depth;
    
    /* Special transactions */
    extern bool run_recovery;
    extern bool first_transaction;

    extern char db_name[];
    extern char filename[];
    extern char password[];
    extern char login[];
    extern char output_file[];

    extern QueryType query_type;

    extern transaction_id trid;
    extern session_id     sid;

    extern bool is_need_checkpoint_on_transaction_commit;
    extern bool is_ro_mode;       // may change during transaction execution!
    extern bool is_ft_disabled;
    extern bool is_log_less_mode; // true, if we write only one record on every bulkload

    extern pping_client* ppc;
    extern client_core*  client;

    extern int internal_auth_switch;
}


#endif /* _TR_GLOBALS_H */
