/*
 * File:  tr_functions.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TR_FUNCTIONS
#define _TR_FUNCTIONS

#include "common/sedna.h"

#include "common/base.h"
#include "tr/crmutils/crmbase.h"
#include "tr/executor/base/PPBase.h"
#include "tr/xqp/XQuerytoLR.h"

void on_user_statement_begin(QueryType queryType,
                             t_print output_type,
                             se_ostream* s,
                             const char* query_str,
                             PPQueryEssence* &qep_tree, 
                             StmntsArray* &st);
void on_user_statement_end(PPQueryEssence* &qep_tree, StmntsArray* &st);

void set_session_finished();

qepNextAnswer execute (PPQueryEssence* qep_tree);
qepNextAnswer next    (PPQueryEssence* qep_tree);

void do_authentication();
void register_session_on_gov();

bool check_database_existence(const char* name);

#endif /* _TR_FUNCTIONS */
