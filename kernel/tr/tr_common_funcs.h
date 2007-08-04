/*
 * File:  tr_common_funcs.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TR_COMMON_FUNCTIONS
#define _TR_COMMON_FUNCTIONS

#include "common/sedna.h"

extern bool is_sm_server_inited;
extern bool is_ph_inited;

extern bool is_trid_obtained;
extern bool is_qep_built;
extern bool is_qep_opened;
extern bool is_stmt_built;


void on_session_begin(SSMMsg* &sm_server, bool rcv_active= false);
void on_session_end(SSMMsg* &sm_server);
void on_transaction_begin(SSMMsg* &sm_server, bool rcv_active = false, bool is_query = false);
void on_transaction_end(SSMMsg* &sm_server, bool is_commit, bool rcv_active = false);

void on_kernel_recovery_statement_begin();
void on_kernel_recovery_statement_end();

bool is_stop_session();
transaction_id get_transaction_id(SSMMsg* sm_server);
void release_transaction_id(SSMMsg* sm_server);


#endif
