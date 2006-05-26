/*
 * File:  tr_functions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TR_FUNCTIONS
#define _TR_FUNCTIONS

#include "sedna.h"
#include <string>
#include "upipe.h"
#include "exec_output.h"
#include "SSMMsg.h"
#include "base.h"
#include "crmutils.h"
#include "PPBase.h"
#include "d_printf.h"
#include "ipc_ops.h"
#include "por2qep.h"
#include "indirection.h"
#include "tr_globals.h"
#include "XQuerytoLR.h"
#include "tr_common_funcs.h"

#define ENV_BUF_SIZE 1000

void on_user_statement_begin(QueryType query_type,
                             t_print output_type,
                             se_ostream* s,
                             const char* query_str,
                             PPQueryEssence* &qep_tree, 
                             StmntsArray* &st);
void on_user_statement_end(PPQueryEssence* &qep_tree, StmntsArray* &st);

void set_session_finished();

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
