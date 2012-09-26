/*
 * File:  tr_common_funcs.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TR_COMMON_FUNCTIONS
#define _TR_COMMON_FUNCTIONS

#include "common/sedna.h"
// #include "common/pping.h"
#include "common/ssmmsg/SSMMsg.h"

void on_session_begin(SSMMsg* &sm_server, bool rcv_active= false);
void on_session_end(SSMMsg* &sm_server);
void on_transaction_begin(SSMMsg* &sm_server, /*pping_client* ppc,*/ bool rcv_active = false);
void on_transaction_end(SSMMsg* &sm_server, bool is_commit, /*pping_client* ppc,*/ bool rcv_active = false);

void on_kernel_recovery_statement_begin();
void on_kernel_recovery_statement_end();

bool is_stop_session();

void SwitchSessionToRO (bool flag);
void SwitchLogMode     (int log_less_mode);

#endif /* _TR_COMMON_FUNCTIONS */
