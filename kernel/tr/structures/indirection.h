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
struct indir_blk_hdr 
{
	vmm_sm_blk_hdr sm_vmm;	/* sm/vmm parameters */
    xptr nblk;				/* next block */
	static xptr init(xptr p);
};
xptr add_record_to_data_indirection_table(xptr p);
void del_record_from_data_indirection_table(xptr p);

xptr add_record_to_tmp_indirection_table(xptr p);
void del_record_from_tmp_indirection_table(xptr p);

inline xptr add_record_to_indirection_table(xptr p)
{
    return IS_DATA_BLOCK(p) ? add_record_to_data_indirection_table(p)
                            : add_record_to_tmp_indirection_table(p);
}

inline void del_record_from_indirection_table(xptr p)
{
    IS_DATA_BLOCK(p) ? del_record_from_data_indirection_table(p)
                     : del_record_from_tmp_indirection_table(p);
}

void sync_indirection_table();

void indirection_table_on_session_begin();
void indirection_table_on_transaction_begin();
void indirection_table_on_statement_begin();
void indirection_table_on_session_end();
void indirection_table_on_transaction_end();
void indirection_table_on_statement_end();


// functions for rollback
void switch_to_rollback_mode(int type);
void start_delete_mode(doc_schema_node* doc);
void stop_delete_mode();
void set_rollback_record(xptr p);
void set_redo_hint(int cl_hint,std::vector<xptr>* blocks);
extern int indir_block_count;
extern int indir_node_count;
extern bool delete_mode ;
bool is_rolled_back();

xptr get_last_indir();
#endif


