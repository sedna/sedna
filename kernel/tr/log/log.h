/*
 * File:  log.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _LOG_H
#define _LOG_H

#include <string>

#include "common/sedna.h"
#include "common/xptr.h"

#include "tr/structures/nodes.h"
#include "tr/mo/mo.h"
#include "tr/log/logiclog.h"
#include "tr/executor/base/PPBase.h"

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#endif

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif

/* Logical journal records */
void hl_logical_log_on_session_begin(std::string logical_log_path, bool rcv_active);
void hl_logical_log_on_transaction_begin(bool rcv_active, bool tr_ro_mode);
void hl_logical_log_on_session_end();
void hl_logical_log_on_transaction_end(bool is_commit, bool rcv_active);

void up_transaction_block_sems();    // this functions
void down_transaction_block_sems();  // serve to block starting of all transactions before checkpoint

void down_concurrent_micro_ops_number();
void up_concurrent_micro_ops_number();
void wait_for_checkpoint_finished();

// force == false, if we want to ignore this checkpoint if any another checkpoint is already being processed
// force == true, if we want to make checkpoint in any case
void activate_and_wait_for_end_checkpoint(bool force);

LSN get_lsn_of_first_record_in_logical_log();

void hl_logical_log_element(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const char* uri,const char* prefix,bool inserted);
void hl_logical_log_attribute(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const  char* value,int data_size,const char* uri,const char* prefix,bool inserted);
void hl_logical_log_text(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int data_size,bool inserted); 
void hl_logical_log_text(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,xptr& value,bool inserted );
void hl_logical_log_text_edit(const xptr &self,const  char* value,int data_size,bool begin,bool inserted); 
void hl_logical_log_text_edit(const xptr &self,int data_size,bool begin,bool inserted); 
#ifdef SE_ENABLE_FTSEARCH
void hl_logical_log_ft_index(PathExpr *object_path, ft_index_type itconst, const char * index_title, const char* doc_name,bool is_doc, ft_custom_tree_t * custom_tree,bool inserted);
ft_index_template_t* ft_rebuild_cust_tree(const char *custom_tree_buf, int custom_tree_size);
#endif
#ifdef SE_ENABLE_TRIGGERS
void hl_logical_log_trigger(trigger_time tr_time, trigger_event tr_event, PathExpr *trigger_path, trigger_granularity tr_gran, trigger_action_cell* trac, inserting_node insnode, PathExpr *path_to_parent, const char* trigger_title, const char* doc_name, bool is_doc, bool inserted); 
#endif
void hl_logical_log_pi(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int total_size,shft target_size,bool inserted); 
void hl_logical_log_comment(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int data_size,bool inserted); 
void hl_logical_log_document(const xptr &self,const  char* name,const  char* collection,bool inserted);
void hl_logical_log_collection(const  char* name,bool inserted);
void hl_logical_log_index(PathExpr *object_path, PathExpr *key_path, xmlscm_type key_type,const char * index_title, const char* doc_name,bool is_doc,bool inserted);
void hl_logical_log_namespace(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* uri,const char* prefix,bool inserted); 
void hl_logical_log_commit(transaction_id);
void hl_logical_log_rollback(transaction_id);

void hl_enable_log();
void hl_disable_log();
void hl_logical_log_rename_collection(const  char* old_name,const char* new_name);

#endif

