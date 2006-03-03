/*
 * File:  log.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _LOG_H
#define _LOG_H

#include "xptr.h"
#include "nodes.h"
#include "micro.h"
#include "llmgr_core.h"
#include <string>
#include "PPBase.h"
#ifdef SE_ENABLE_FTSEARCH
#include "ft_index_data.h"
#endif

void hl_phys_log_on_session_begin(std::string phys_log_path);
void hl_phys_log_on_transaction_begin();
void hl_phys_log_on_session_end();
void hl_phys_log_on_transaction_end();

void hl_phys_log_change(const /*xptr &*/void *p, shft size);
void hl_phys_log_change_blk(const /*xptr &*/void *p);
void hl_phys_log_create_node_blk(const void* p);
int get_phys_record_block_parts(const void * p, int size);

/* Logical journal records */
void hl_logical_log_on_session_begin(std::string logical_log_path, bool rcv_active = false);
void hl_logical_log_on_transaction_begin(bool rcv_active = false);
void hl_logical_log_on_session_end();
void hl_logical_log_on_transaction_end(bool is_commit);


void down_concurrent_micro_ops_number();
void up_concurrent_micro_ops_number();

void hl_logical_log_element(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const char* uri,const char* prefix,bool inserted);
void hl_logical_log_attribute(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const  char* value,int data_size,const char* uri,const char* prefix,bool inserted);
void hl_logical_log_text(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int data_size,bool inserted); 
void hl_logical_log_text(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,xptr& value,int data_size,bool inserted ); 
void hl_logical_log_text_edit(const xptr &self,const  char* value,int data_size,bool begin,bool inserted); 
void hl_logical_log_text_edit(const xptr &self,int data_size,bool begin,bool inserted); 
//void hl_logical_log_text_edit(const xptr &self,xptr source,bool begin); 
//void hl_logical_log_text(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,bool inserted); 
#ifdef SE_ENABLE_FTSEARCH
 void hl_logical_log_ft_index(PathExpr *object_path, ft_index_type itconst, char * index_title, const char* doc_name,bool is_doc,pers_sset<ft_custom_cell,unsigned short> * custom_tree,bool inserted); 
#endif
void hl_logical_log_pi(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int total_size,shft target_size,bool inserted); 
void hl_logical_log_comment(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int data_size,bool inserted); 
void hl_logical_log_document(const xptr &self,const  char* name,const  char* collection,bool inserted);
void hl_logical_log_collection(const  char* name,bool inserted);
void hl_logical_log_index(PathExpr *object_path, PathExpr *key_path, xmlscm_type key_type,const char * index_title, const char* doc_name,bool is_doc,bool inserted);
void hl_logical_log_indirection(int cl_hint, std::vector<xptr>* blocks);
void hl_logical_log_namespace(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* uri,const char* prefix,bool inserted); 
void hl_logical_log_commit(transaction_id);
void hl_logical_log_rollback(transaction_id);

void hl_enable_log();
void hl_disable_log();

extern llmgr_core* tr_llmgr;
#endif

