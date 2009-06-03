/*
 * File:  indirection.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _INDIRECTION_H
#define _INDIRECTION_H

#include "common/sedna.h"

#include "common/xptr.h"
#include "tr/structures/schema.h"
#include "common/sm_vmm_data.h"
#define MIN_CLUSTER_SIZE 1
#define MODE_NORMAL 0
#define MODE_REDO 1
#define MODE_UNDO 2

xptr add_record_to_indirection_table(xptr p);

void del_record_from_indirection_table(xptr p);

void sync_indirection_table();

void indirection_table_on_session_begin();
void indirection_table_on_transaction_begin();
void indirection_table_on_statement_begin();
void indirection_table_on_session_end();
void indirection_table_on_transaction_end();
void indirection_table_on_statement_end();


// functions for rollback
void switch_to_rollback_mode(int type);
void set_rollback_record(xptr p);
extern int indir_node_count;
extern bool delete_mode ;
bool is_rolled_back();

void add_predeleted_block(xptr block);

xptr get_last_indir();

bool check_indirection_consistency(xptr p, bool recourse = false);
#endif


