/*
 * File:  tr_functions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TR_FUNCTIONS
#define _TR_FUNCTIONS

#include <string>
#include "upipe.h"
#include "exec_output.h"
#include "SSMMsg.h"
#include "base.h"
#include "crmutils.h"
#include "PPBase.h"
#include "d_printf.h"
#include "ipc_ops.h"
#include "exceptions.h"
#include "por2qep.h"
#include "indirection.h"
#include "tr_globals.h"
#include "XQuerytoLR.h"

#define ENV_BUF_SIZE 1000

void on_session_begin(SSMMsg* &sm_server);
void on_session_end(SSMMsg* &sm_server);
void on_transaction_begin(SSMMsg* &sm_server);
void on_transaction_end(SSMMsg* &sm_server, bool is_commit);
void on_user_statement_begin(QueryType query_type,
                             t_print output_type,
                             crmostream* s,
                             const char* query_str,
                             PPQueryEssence* &qep_tree, 
                             StmntsArray* &st);
void on_user_statement_end(PPQueryEssence* &qep_tree, StmntsArray* &st);



bool is_stop_session();
void set_session_finished();

transaction_id get_transaction_id(SSMMsg* sm_server);
void release_transaction_id(SSMMsg* sm_server);

void clear_state(StmntsArray* &st, PPQueryEssence* &qep_tree);
void exec_implicit_stmnt_part(StmntsArray *st);
void execute(PPQueryEssence* qep_tree);
bool next(PPQueryEssence* qep_tree);

bool is_command_line_args_length_overflow(int argc, char ** argv);
void print_tr_usage();
void authentication();
void register_session_on_gov();

bool check_database_existence(const char* db_name);

#endif
