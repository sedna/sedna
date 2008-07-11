/*
 * File:  logiclog.h - Logical logging on tr
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Only the user interface is specified here. For further information refer to logicallog.cpp file.
 *
 */

#ifndef _LL_LOGICAL_LOG_
#define _LL_LOGICAL_LOG_

#include "common/base.h"
#include "common/xptr.h"
#include "common/lfsGlobals.h"

// Record element
void llLogElement(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, 
    			  const xptr *parent, const char* name, const char* uri, const char* prefix, xmlscm_type type, bool inserted);

// Record attribute
void llLogAttribute(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, const xptr *parent,
						const char* name, xmlscm_type type, const char* value, int value_size, const char* uri,
						const char* prefix, bool inserted);


// Text log record
void llLogText(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, const xptr *parent,
				const char* value, int value_size, bool inserted);

// Text edit log record
void llLogTextEdit(transaction_id trid, const xptr *self, const char* value, int data_size, bool begin, 
						bool inserted);

// Document log record
void llLogDocument(transaction_id trid, const xptr *self, const char *name, const char *collection, bool inserted);

// PI log record
void llLogPI(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, const xptr *parent,
				const char* value, int total_size, shft target_size, bool inserted);

// Comment log record
void llLogComment(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, const xptr *parent, 
					const char *value, int value_size, bool inserted);

// Collection log record
void llLogCollection(transaction_id trid, const char* name, bool inserted);

// Namespace log record
void llLogNS(transaction_id trid, const xptr *self, const xptr *left, const xptr *right, const xptr *parent,
				const char *uri, const char *prefix, bool inserted);

// Index log record
void llLogIndex(transaction_id trid, const char *object_path, const char *key_path, xmlscm_type key_type, 
					const char *index_title, const char *doc_name, bool is_doc, bool inserted);

// Full-text index log record
void llLogFtIndex(transaction_id trid, const char *object_path, int itconst, const char *index_title, 
					const char *doc_name, bool is_doc, char *custom_tree_buf, int custom_tree_size, bool inserted);

// Trigger log record
void llLogTrigger(transaction_id trid, int tr_time, int tr_event, const char *trigger_path, int tr_gran,
						char *tr_action_buf, int tr_action_size, const char *in_name, int in_type, 
						const char *path_to_parent, const char *trigger_title, const char *doc_name, 
						bool is_doc, bool inserted);

// Commit log record
void llLogCommit(transaction_id trid);

// Rollback log record
void llLogRollback(transaction_id trid);

#endif
