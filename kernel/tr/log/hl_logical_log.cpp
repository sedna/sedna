/*
 * File:  hl_logical_log.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <vector>
#include <sstream>

#include "common/sedna.h"

#include "tr/log/log.h"
#include "tr/log/logiclog.h"
#include "sm/llsm/llMain.h"
#include "tr/tr_globals.h"
#include "common/errdbg/d_printf.h"
#include "common/tr_debug.h"
#include "sm/trmgr.h"
#include "tr/pstr/pstr_long.h"
#include "tr/rcv/rcv_funcs.h"
#include "tr/mo/indirection.h"
#include "tr/mo/boundaries.h"

#ifdef SE_ENABLE_DTSEARCH
#include "tr/ft/FTindex.h"
#endif



using namespace std;

static bool is_ll_on_session_initialized = false;
static bool is_ll_on_transaction_initialized = false;

// concurrent semaphore already was released by commit function
// because of checkpoint
static bool sem_released = false;

USemaphore checkpoint_sem;
USemaphore concurrent_trns_sem;

///// FOR DEBUG ////
int number_of_records = 0;
int down_nums = 0;
int up_nums = 0;
int down_up_counter = 0;
////////////////////


bool enable_log = true;

void hl_logical_log_on_session_begin(string logical_log_path, bool rcv_active)
{
	if ( 0 != USemaphoreOpen(&concurrent_trns_sem, SEDNA_TRNS_FINISHED, __sys_call_error))
		throw USER_EXCEPTION2(SE4012, "SEDNA_TRNS_FINISHED");

	llOpen(logical_log_path.c_str(), tr_globals::db_name, rcv_active);

	is_ll_on_session_initialized = true;
}

void hl_logical_log_on_transaction_begin(bool rcv_active, bool tr_ro_mode)
{
	//Here trid is a global variable inited before
	if (tr_ro_mode) // we don't need log in RO-mode
	{
		enable_log = false;
		tr_globals::is_need_checkpoint_on_transaction_commit = false;
		is_ll_on_transaction_initialized = false;
		return;
	}

	llOnTransBegin(tr_globals::trid);
	tr_globals::is_need_checkpoint_on_transaction_commit = tr_globals::is_log_less_mode;
	is_ll_on_transaction_initialized = true;
#ifdef LOG_TRACE
	elog(EL_LOG, ("LOG_TRACE: Transaction is started: trid=%d", tr_globals::trid));
#endif
}

void hl_logical_log_on_session_end()
{
	if (is_ll_on_session_initialized)
	{
		if (USemaphoreClose(concurrent_trns_sem, __sys_call_error) != 0)
			throw USER_EXCEPTION2(SE4013, "CHARISMA_LOGICAL_OPERATION_ATOMICITY");
	}

	if (is_ll_on_session_initialized)
		llClose();

	is_ll_on_session_initialized = false;
}

void hl_logical_log_on_transaction_end(bool is_commit, bool rcv_active)
{
	if (is_ll_on_transaction_initialized)
	{
		if (!rcv_active)
		{
			if (is_commit)
			{
#ifdef SE_ENABLE_DTSEARCH
				SednaIndexJob::start_commit();
#endif
				hl_logical_log_commit(tr_globals::trid);
#ifdef SE_ENABLE_DTSEARCH
				SednaIndexJob::fix_commit();
#endif
       		}
        	else
	        {
#ifdef LOG_TRACE
				elog(EL_LOG, ("LOG_TRACE: Transaction starts rolling back: trid=%d", tr_globals::trid));
#endif
				rollback_tr_by_logical_log(tr_globals::trid);
#ifdef SE_ENABLE_DTSEARCH
				SednaIndexJob::rollback();
#endif
				hl_logical_log_rollback(tr_globals::trid);
			}
		}

		//Here trid is a global variable inited before
		llOnTransEnd(tr_globals::trid);
	}

	enable_log = true;  // log might have been disabled for RO-mode
	is_ll_on_transaction_initialized = false;
}

// do not call this now during transaction processing!!!
void activate_and_wait_for_end_checkpoint(bool force)
{
    int res;

    while ((res = llActivateCheckpoint()) == -2 && force)
        uSleep(1, __sys_call_error);

    if (res == -2 && !force)
        return;
    else if (res != 0)
        throw SYSTEM_EXCEPTION("logic error in waiting on checkpoint!");

    U_ASSERT(res == 0);

    // wait for checkpoint to finish
    // checkpoint thread should awake this transaction
    llOnCheckpointWait();
}

void down_concurrent_micro_ops_number()
{
	down_up_counter++;
}

void up_concurrent_micro_ops_number()
{
	down_up_counter--;
}

void up_transaction_block_sems()
{
    if (sem_released)
    {
        sem_released = false;
        return;
    }

	if (USemaphoreUp(concurrent_trns_sem, __sys_call_error) != 0)
		throw SYSTEM_EXCEPTION("Can't up semaphore: CHARISMA_LOGICAL_OPERATION_ATOMICITY");
}

void down_transaction_block_sems()
{
	if (USemaphoreDown(concurrent_trns_sem, __sys_call_error) != 0)
		throw SYSTEM_EXCEPTION("Can't down semaphore: CHARISMA_LOGICAL_OPERATION_ATOMICITY");
}

LSN get_lsn_of_first_record_in_logical_log()
{
    return llGetFirstTranLsn(tr_globals::trid);
}

void hl_logical_log_element(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const char* uri,const char* prefix,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;
	llLogElement(tr_globals::trid, &self, &left, &right, &parent, name, uri, prefix, type, inserted);
#ifdef LOG_TRACE
	if (inserted)
		elog(EL_LOG, ("LOG_TRACE: Element is inserted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
	else
		elog(EL_LOG, ("LOG_TRACE: Element is deleted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
#endif
}

void hl_logical_log_attribute(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const  char* value,int data_size,const char* uri,const char* prefix,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;
	llLogAttribute(tr_globals::trid, &self, &left, &right, &parent, name, type, value, data_size, uri, prefix, inserted);
#ifdef LOG_TRACE
	if (inserted)
		elog(EL_LOG, ("LOG_TRACE: Attribute is inserted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
	else
		elog(EL_LOG, ("LOG_TRACE: Attribute is deleted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
#endif
}

void hl_logical_log_text(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int data_size,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;
	llLogText(tr_globals::trid, &self, &left, &right, &parent, (char*)value, data_size, inserted);
#ifdef LOG_TRACE
	if (inserted)
		elog(EL_LOG, ("LOG_TRACE: Text is inserted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
	else
		elog(EL_LOG, ("LOG_TRACE: Text is deleted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
#endif
}

void hl_logical_log_text_edit(const xptr &self,const  char* value,int data_size,bool begin,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;
	llLogTextEdit(tr_globals::trid, &self, (char*)value, data_size, begin, inserted);
#ifdef LOG_TRACE
	elog(EL_LOG, ("LOG_TRACE:  Text Edit: trid=%d, self=%08x%08x", tr_globals::trid, self.layer, (int)self.addr));
#endif
}

void hl_logical_log_text(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,xptr& value,bool inserted )
{
	if (!enable_log) return;

	if (inserted)
	{
		pstr_long_cursor cur(value);
		char *ptr;
		int len = cur.get_blk(&ptr);
		ASSERT(len > 0);
		hl_logical_log_text(self, left, right, parent, ptr, len, true);
		while ((len = cur.get_blk(&ptr)) > 0)
		{
			hl_logical_log_text_edit(self, ptr, len, false, true);
		}
	}
	else
	{
		pstr_long_cursor cur(value, true);
		char *ptr1, *ptr2;
		int len1 = cur.get_blk_rev(&ptr1);
		int len2 = cur.get_blk_rev(&ptr2);
		ASSERT(len1 > 0);
		while (len2 > 0)
		{
			hl_logical_log_text_edit(self, ptr1, len1, false, false);
			len1 = len2;
			ptr1 = ptr2;
			len2 = cur.get_blk_rev(&ptr2);
		}
		hl_logical_log_text(self, left, right, parent, ptr1, len1, false);
	}
}

void hl_logical_log_text_edit(const xptr &self,int data_size,bool begin,bool inserted)
{
	if (!enable_log) return;
	ASSERT(inserted);
	xptr desc = indirectionDereferenceCP(self);
	CHECKP(desc);
	xptr str_ptr = ((t_dsc*)XADDR(desc))->data;
	int  str_len = ((t_dsc*)XADDR(desc))->size; //FIXME - int
	if (str_len <= PSTRMAXSIZE)
	{
		if (inserted)
		{
			CHECKP(str_ptr);
			if (begin)
				hl_logical_log_text_edit(self, (char*)XADDR(str_ptr), data_size, begin, inserted);
			else
				hl_logical_log_text_edit(self, (char*)XADDR(str_ptr) + str_len - data_size, data_size, begin, inserted);
		}
		return;
	}
	if (!begin)
	{
		if (inserted)
		{
			pstr_long_iterator cur(str_ptr, str_len);
			cur -= data_size;
			char *ptr;
			int len;
			while ( (len = cur.get_blk(&ptr)) > 0)
				hl_logical_log_text_edit(self, ptr, len, false, true);
		}
	}
	else
	{
		if (inserted)
		{
			pstr_long_iterator cur(str_ptr);
			char *ptr;
			int len;
			cur += data_size;
			while ( (len = cur.get_blk_rev(&ptr)) > 0)
				hl_logical_log_text_edit(self, ptr, len, true, true);
		}
	}
}

void hl_logical_log_comment(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int data_size,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;
	llLogComment(tr_globals::trid, &self, &left, &right, &parent, value, data_size, inserted);
#ifdef LOG_TRACE
	if (inserted)
		elog(EL_LOG, ("LOG_TRACE: Comment is inserted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
	else
		elog(EL_LOG, ("LOG_TRACE: Comment is deleted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
#endif
}

void hl_logical_log_document(const xptr &self,const  char* name,const  char* collection,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;
	llLogDocument(tr_globals::trid, &self, name, collection, inserted);
#ifdef LOG_TRACE
	if (inserted)
		elog(EL_LOG, ("LOG_TRACE: Document is inserted: trid=%d, self=%08x%08x, name=%s, coll=%s", tr_globals::trid,
			self.layer, (int)self.addr, name, collection));
	else
		elog(EL_LOG, ("LOG_TRACE: Document is deleted: trid=%d, self=%08x%08x, name=%s, coll=%s", tr_globals::trid,
			self.layer, (int)self.addr, name, collection));
#endif
}
void hl_logical_log_rename_collection(const char *old_name, const char *new_name)
{
	if (!enable_log) return;
	number_of_records++;

	llLogRenameCollection(tr_globals::trid, old_name, new_name);

#ifdef LOG_TRACE
	elog(EL_LOG, ("LOG_TRACE: Collection is renamed: trid=%d, old name=%s, new name=%s", tr_globals::trid,
		old_name, new_name));
#endif

}
void hl_logical_log_collection(const  char* name,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;
	llLogCollection(tr_globals::trid, name, inserted);
#ifdef LOG_TRACE
	if (inserted)
		elog(EL_LOG, ("LOG_TRACE: Collection is created: trid=%d, name=%s", tr_globals::trid, name));
	else
		elog(EL_LOG, ("LOG_TRACE: Text is deleted: trid=%d, name=%s", tr_globals::trid, name));
#endif
}

void hl_logical_log_namespace(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* uri,const char* prefix,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;
	llLogNS(tr_globals::trid, &self, &left, &right, &parent, uri, prefix, inserted);
#ifdef LOG_TRACE
	if (inserted)
		elog(EL_LOG, ("LOG_TRACE: Namespace is inserted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
	else
		elog(EL_LOG, ("LOG_TRACE: Namespace is deleted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
#endif
}

void hl_logical_log_pi(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int total_size,shft target_size,bool inserted)
{
	if (!enable_log) return;
	number_of_records++;
	llLogPI(tr_globals::trid, &self, &left, &right, &parent, value, total_size, target_size, inserted);
#ifdef LOG_TRACE
	if (inserted)
		elog(EL_LOG, ("LOG_TRACE: Pi is inserted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
	else
		elog(EL_LOG, ("LOG_TRACE: Pi is deleted: trid=%d, self=%08x%08x, left=%08x%08x, right=%08x%08x, parent=%08x%08x", tr_globals::trid,
			self.layer, (int)self.addr, left.layer, (int)left.addr, right.layer, (int)right.addr, parent.layer, (int)parent.addr));
#endif
}

// to report to wu
void reportToWu(bool rcv_active, bool is_commit);

void hl_logical_log_commit(transaction_id _trid)
{
	number_of_records++;

    if (tr_globals::is_need_checkpoint_on_transaction_commit)
    {
        up_transaction_block_sems();
        sem_released = true;
        catalog_on_transaction_end(true);
        storage_on_transaction_end();
        catalog_on_transaction_end(true);
        reportToWu(false, true);
        // dirty hack here!
        // we don't want this transaction to be redone ever
        // so we pretend it've been rolled back
        llLogRollback(_trid);
        activate_and_wait_for_end_checkpoint(true);
    }
    else
        llLogCommit(_trid);

#ifdef LOG_TRACE
	elog(EL_LOG, ("LOG_TRACE: Transaction is committed: trid=%d", tr_globals::trid));
#endif
}

void hl_logical_log_rollback(transaction_id _trid)
{
	llLogRollback(_trid);
#ifdef LOG_TRACE
	elog(EL_LOG, ("LOG_TRACE: Transaction is rolled back: trid=%d", tr_globals::trid));
#endif
}

void hl_enable_log()
{
	enable_log = true;
}

void hl_disable_log()
{
	enable_log = false;
}

void hl_logical_log_index(PathExpr *object_path, PathExpr *key_path, xmlscm_type key_type,const char * index_title, const char* doc_name,bool is_doc,bool inserted)
{
    if (!enable_log) return;
    number_of_records++;

    std::ostringstream obj_str(std::ios::out | std::ios::binary);
    std::ostringstream key_str(std::ios::out | std::ios::binary);

    PathExpr2lr(object_path, obj_str);
    PathExpr2lr(key_path, key_str);

    llLogIndex(tr_globals::trid, obj_str.str().c_str(), key_str.str().c_str(), key_type, index_title, doc_name, is_doc, inserted);
}
#ifdef SE_ENABLE_FTSEARCH
void hl_logical_log_ft_index(PathExpr *object_path, ft_index_type itconst, const char * index_title, const char* doc_name,bool is_doc,ft_custom_tree_t * custom_tree,bool inserted)
{
    if (!enable_log) return;
    number_of_records++;

    std::ostringstream obj_str(std::ios::out | std::ios::binary);
	PathExpr2lr(object_path, obj_str);

	int custom_tree_count = 0;
	int custom_tree_size = 0;
	char *custom_tree_buf = tr_globals::e_string_buf;

	if (custom_tree != NULL)
	{
		ft_custom_tree_t::sedna_rbtree_entry *tmp;
		tmp = custom_tree->rb_minimum(custom_tree->root);

		while (tmp != NULL)
		{
			int len;

			custom_tree_count++;

			U_ASSERT(custom_tree_size + sizeof(ft_index_type) <= sizeof(tr_globals::e_string_buf));
			memcpy(custom_tree_buf + custom_tree_size, &tmp->obj->cm, sizeof(ft_index_type));
			custom_tree_size += sizeof(ft_index_type);

#define PUT_STR(str) \
	if (str == NULL) {\
		U_ASSERT(custom_tree_size < sizeof(tr_globals::e_string_buf));\
		custom_tree_buf[custom_tree_size] = '\x0';\
		custom_tree_size++;\
	} else {\
		len = strlen(str);\
		U_ASSERT(custom_tree_size + len + 1 <= sizeof(tr_globals::e_string_buf));\
		memcpy(custom_tree_buf + custom_tree_size, str, len + 1);\
		custom_tree_size += len + 1;\
	}
			if (tmp->obj->get_xmlns() == NULL)
			{
				PUT_STR(NULL);
				PUT_STR(NULL);
			}
			else
			{
				PUT_STR(tmp->obj->get_xmlns()->uri);
				PUT_STR(tmp->obj->get_xmlns()->prefix);
			}
			PUT_STR(tmp->obj->local);
#undef PUT_STR

			tmp = custom_tree->rb_successor(tmp);
		}
	}

    llLogFtIndex(tr_globals::trid, obj_str.str().c_str(), itconst, index_title, doc_name, is_doc, custom_tree_buf, custom_tree_size, inserted);
}
ft_index_template_t* ft_rebuild_cust_tree(const char *custom_tree_buf, int custom_tree_size)
{
	return NULL;
}
#endif

#ifdef SE_ENABLE_TRIGGERS
void hl_logical_log_trigger(trigger_time tr_time, trigger_event tr_event, PathExpr *trigger_path, trigger_granularity tr_gran, trigger_action_cell* trac, inserting_node insnode, PathExpr *path_to_parent, const char* trigger_title, const char* doc_name, bool is_doc, bool inserted)
{
  if (!enable_log) return;
  number_of_records++;

  std::ostringstream tr_path(std::ios::out | std::ios::binary);
  if (trigger_path)
  	PathExpr2lr(trigger_path, tr_path);

  std::ostringstream path_to_par(std::ios::out | std::ios::binary);
  if (path_to_parent)
  	PathExpr2lr(path_to_parent, path_to_par);

  int trac_len = 0;

  for (trigger_action_cell *tr_act = trac; tr_act != NULL; tr_act = tr_act->next)
      trac_len += strlen(tr_act->statement) + 1 + sizeof(int);

  char *tr_action_buf = new char[trac_len];
  int tr_action_buf_size = 0;
  int str_len = 0;

  for (trigger_action_cell *tr_act = trac; tr_act != NULL; tr_act = tr_act->next)
  {
      str_len = strlen(tr_act->statement);

      U_ASSERT(tr_action_buf_size + str_len + 1 + sizeof(int) <= trac_len);

      if (str_len)
      {
          memcpy(tr_action_buf + tr_action_buf_size, tr_act->statement, str_len + 1);
          tr_action_buf_size += str_len + 1;
      }
      else
      {
          tr_action_buf[tr_action_buf_size] = '\x0';
          tr_action_buf_size++;
      }
      memcpy(tr_action_buf + tr_action_buf_size, &(tr_act->is_query), sizeof(bool));
      tr_action_buf_size += sizeof(int);
  }

  llLogTrigger(tr_globals::trid, tr_time, tr_event,  tr_path.str().c_str(), tr_gran, tr_action_buf, tr_action_buf_size,
  	  insnode.name, insnode.type, path_to_par.str().c_str(), trigger_title, doc_name, is_doc, inserted);

  delete[] tr_action_buf;
}
#endif
