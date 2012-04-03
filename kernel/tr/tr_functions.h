/*
 * File:  tr_functions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TR_FUNCTIONS
#define _TR_FUNCTIONS

#include "common/sedna.h"

#include "tr/tr_base.h"
#include "tr/crmutils/crmbase.h"
#include "tr/executor/base/PPBase.h"
#include "tr/xqp/XQuerytoLR.h"
#include "tr/socket_client.h"


void on_user_statement_begin(QueryType queryType,
                             const char* query_str,
                             PPQueryEssence* &qep_tree, 
                             StmntsArray* &st);
void on_user_statement_end(PPQueryEssence* &qep_tree, StmntsArray* &st);

void set_session_finished();

qepNextAnswer execute (PPQueryEssence* qep_tree);
qepNextAnswer next    (PPQueryEssence* qep_tree);

void do_authentication();

/* Sets handlers for keyboard events like Ctrl+C */
void set_trn_ctrl_handler();

#endif /* _TR_FUNCTIONS */
