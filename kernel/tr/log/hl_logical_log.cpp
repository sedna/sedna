/*
 * File:  hl_logical_log.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <vector>
#include "log.h"
#include "indirection.h"
#include "llmgr_core.h"
#include "tr_globals.h"
#include "d_printf.h"
#include "tr_debug.h"
#include "trmgr.h"
#include "pstr_long.h"
#include "rcv_funcs.h"
#ifdef SE_ENABLE_FTSEARCH
#include "FTindex.h"
#endif

#include <sstream>


using namespace std;

llmgr_core* tr_llmgr;
static bool is_ll_on_session_initialized = false;
static bool is_ll_on_transaction_initialized = false;

USemaphore checkpoint_sem;
USemaphore concurrent_ops_sem; 

///// FOR DEBUG ////
int number_of_records = 0;
int down_nums = 0;
int up_nums = 0;
////////////////////


bool enable_log = true;

void hl_logical_log_on_session_begin(string logical_log_path, bool rcv_active)
{
#ifdef CHECKPOINT_ON
  if (USemaphoreOpen(&checkpoint_sem, CHARISMA_CHECKPOINT_SEM) != 0)
     throw USER_EXCEPTION2(SE4012, "CHARISMA_CHECKPOINT_SEM");

  if ( 0 != USemaphoreOpen(&concurrent_ops_sem, CHARISMA_LOGICAL_OPERATION_ATOMICITY))
     throw USER_EXCEPTION2(SE4012, "CHARISMA_LOGICAL_OPERATION_ATOMICITY");
#endif

#ifdef LOGICAL_LOG
  tr_llmgr = new llmgr_core();
  tr_llmgr->ll_log_open(logical_log_path, rcv_active);
  tr_llmgr->ll_log_open_shared_mem();
  is_ll_on_session_initialized = true; 
#endif


}

void hl_logical_log_on_transaction_begin(bool rcv_active)
{
#ifdef LOGICAL_LOG
  //Here trid is a global variable inited before
  tr_llmgr->ll_log_on_transaction_begin(rcv_active, trid, true);
  is_ll_on_transaction_initialized = true; 
#endif
}

void hl_logical_log_on_session_end()
{
#ifdef CHECKPOINT_ON
 if (is_ll_on_session_initialized)
 {
   if (USemaphoreClose(checkpoint_sem) != 0)
      throw USER_EXCEPTION2(SE4013, "CHARISMA_CHECKPOINT_SEM");

   if (USemaphoreClose(concurrent_ops_sem) != 0)
      throw USER_EXCEPTION2(SE4013, "CHARISMA_LOGICAL_OPERATION_ATOMICITY");

//   d_printf2("down_nums=%d\n", down_nums);
//   d_printf2("up_nums=%d\n", up_nums);
 }
#endif

#ifdef LOGICAL_LOG
 if (is_ll_on_session_initialized)
 {
   tr_llmgr->ll_log_close_shared_mem();
   tr_llmgr->ll_log_close();
 }

 is_ll_on_session_initialized = false;
#endif 
}

void hl_logical_log_on_transaction_end(bool is_commit)
{
#ifdef LOGICAL_LOG
  if (is_ll_on_transaction_initialized)
  {
     if (is_commit)
     {
#ifdef SE_ENABLE_FTSEARCH
		 SednaIndexJob::start_commit();
#endif
		 hl_logical_log_commit(trid);
#ifdef SE_ENABLE_FTSEARCH
		 SednaIndexJob::fix_commit();
#endif
     }
     else
     {
        rollback_tr_by_logical_log(trid);
#ifdef SE_ENABLE_FTSEARCH
		SednaIndexJob::rollback();
#endif
        hl_logical_log_rollback(trid); 
     }

     //Here trid is a global variable inited before
     tr_llmgr->ll_log_on_transaction_end(trid, true);
  }

  is_ll_on_transaction_initialized = false;
#endif
}


/*
void hl_logical_log_register_tr(transaction_id _trid)
{
#ifdef LOGICAL_LOG
  tr_llmgr->register_trn(_trid, true);
  is_register_on_logical_log = true;

#endif
}


void hl_logical_log_register_tr_light(transaction_id _trid)
{
#ifdef LOGICAL_LOG
  tr_llmgr->register_trn(_trid, true);
  is_register_on_logical_log = true;

#endif
}


void hl_logical_log_unregister_tr(transaction_id _trid)
{
#ifdef LOGICAL_LOG
  if (is_register_on_logical_log == true)
  {
     if (_trid < 0 || _trid >= CHARISMA_MAX_TRNS_NUMBER) return;
     tr_llmgr->unregister_trn(_trid, true);
  }

  is_register_on_logical_log = false;
#endif
}

void hl_logical_log_unregister_tr_light(transaction_id _trid)
{
#ifdef LOGICAL_LOG
  if (is_register_on_logical_log == true)
  {
     if (_trid < 0 || _trid >= CHARISMA_MAX_TRNS_NUMBER) return;
     tr_llmgr->unregister_trn(_trid, true);
  }

  is_register_on_logical_log = false;

#endif

}
*/


void down_concurrent_micro_ops_number()
{
#ifdef CHECKPOINT_ON

//  d_printf1("down_concurrent_micro_ops_number() - begin\n");

  if (USemaphoreDown(checkpoint_sem) != 0)  
     throw SYSTEM_EXCEPTION("Can't down semaphore: CHARISMA_CHECKPOINT_SEM");

  if (USemaphoreUp(checkpoint_sem) != 0)
     throw SYSTEM_EXCEPTION("Can't up semaphore: CHARISMA_CHECKPOINT_SEM");

  if (USemaphoreDown(concurrent_ops_sem) != 0)
     throw SYSTEM_EXCEPTION("Can't down semaphore: CHARISMA_LOGICAL_OPERATION_ATOMICITY");

//  down_nums++;

//  d_printf1("down_concurrent_micro_ops_number() - end\n");

  //string str = string("down_concurrent_micro_ops_number\n");
  //WRITE_DEBUG_LOG(str.c_str());
#endif

}

void up_concurrent_micro_ops_number()
{

#ifdef CHECKPOINT_ON
//  d_printf1("up_concurrent_micro_ops_number() - begin\n");

  if (USemaphoreUp(concurrent_ops_sem) != 0)
     throw SYSTEM_EXCEPTION("Can't down semaphore: CHARISMA_LOGICAL_OPERATION_ATOMICITY"); 

//  up_nums++;

//  d_printf1("up_concurrent_micro_ops_number() - end\n");


  //string str = string("up_concurrent_micro_ops_number\n");
  //WRITE_DEBUG_LOG(str.c_str());
#endif
}




void hl_logical_log_element(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const char* uri,const char* prefix,bool inserted)
{
#ifdef LOGICAL_LOG
//  d_printf2("element %s\n", name);
//  self.print();
  if (!enable_log) return;
  number_of_records++;
  tr_llmgr->ll_log_element(trid, self, left, right, parent, name, uri, prefix, type, inserted, true);
  string str = string("hl_logical_log_element finished\n");
  WRITE_DEBUG_LOG(str.c_str());

#endif
}

void hl_logical_log_attribute(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* name, xmlscm_type type,const  char* value,int data_size,const char* uri,const char* prefix,bool inserted)
{
#ifdef LOGICAL_LOG
  if (!enable_log) return;
  number_of_records++;
  tr_llmgr->ll_log_attribute(trid, self, left, right, parent, name, type, value, data_size, uri, prefix, inserted, true);

  string str = string("hl_logical_log_attribute finished\n");
  WRITE_DEBUG_LOG(str.c_str());

#endif
}
void hl_logical_log_text(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int data_size,bool inserted)
{
#ifdef LOGICAL_LOG
  if (!enable_log) return;
  number_of_records++;
  tr_llmgr->ll_log_text(trid, self, left, right, parent, (char*)value, data_size, inserted, true);
  string str = string("hl_logical_log_text finished\n");
  WRITE_DEBUG_LOG(str.c_str());

#endif
}
void hl_logical_log_text_edit(const xptr &self,const  char* value,int data_size,bool begin,bool inserted)
{
#ifdef LOGICAL_LOG
  if (!enable_log) return;
  number_of_records++;
  tr_llmgr->ll_log_text_edit(trid, self, (char*)value, data_size, begin, inserted, true);
#endif
}
void hl_logical_log_text(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,xptr& value,int data_size,bool inserted ) 
{
#ifdef LOGICAL_LOG
	if (!enable_log) return;
	ASSERT(data_size > PSTRMAXSIZE);
	if (inserted)
	{
		pstr_long_cursor cur(value);
		char *ptr;
		int len = cur.get_blk(&ptr);
		ASSERT(len > 0);
		hl_logical_log_text(self, left, right, parent, ptr, len, true);
		while ( (len = cur.get_blk(&ptr)) > 0)
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
#endif
}
void hl_logical_log_text_edit(const xptr &self,int data_size,bool begin,bool inserted)
{
#ifdef LOGICAL_LOG
	if (!enable_log) return;
	ASSERT(inserted);
	xptr desc = removeIndirection(self);
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
#endif
}
void hl_logical_log_comment(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int data_size,bool inserted)
{
#ifdef LOGICAL_LOG
  if (!enable_log) return;
  number_of_records++;
  tr_llmgr->ll_log_comment(trid, self, left, right, parent, value, data_size, inserted, true); 
  string str = string("hl_logical_log_comment finished\n");
  WRITE_DEBUG_LOG(str.c_str());
#endif
}
void hl_logical_log_document(const xptr &self,const  char* name,const  char* collection,bool inserted)
{
#ifdef LOGICAL_LOG
  if (!enable_log) return;
  number_of_records++;
//  d_printf2("doc %s\n", name);
//  self.print();
  tr_llmgr->ll_log_document(trid, self, name, collection, inserted, true); 
  string str = string("hl_logical_log_document finished\n");
  WRITE_DEBUG_LOG(str.c_str());
#endif
}
void hl_logical_log_collection(const  char* name,bool inserted)
{
#ifdef LOGICAL_LOG
  if (!enable_log) return;
  number_of_records++;
  tr_llmgr->ll_log_collection(trid, name, inserted, true);
  string str = string("hl_logical_log_collection finished\n");
  WRITE_DEBUG_LOG(str.c_str());
#endif
}


void hl_logical_log_namespace(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const char* uri,const char* prefix,bool inserted)
{
#ifdef LOGICAL_LOG
  if (!enable_log) return;
  number_of_records++;
  tr_llmgr->ll_log_ns(trid, self, left, right, parent, uri, prefix, inserted, true);
  string str = string("hl_logical_log_namespace finished\n");
  WRITE_DEBUG_LOG(str.c_str());
#endif
}
void hl_logical_log_pi(const xptr &self,const xptr &left,const xptr &right,const xptr &parent,const  char* value,int total_size,shft target_size,bool inserted)
{
#ifdef LOGICAL_LOG
  if (!enable_log) return;
  number_of_records++;
  tr_llmgr->ll_log_pi(trid, self, left, right, parent, value, total_size, target_size, inserted, true);
  string str = string("hl_logical_log_pi finished\n");
  WRITE_DEBUG_LOG(str.c_str());
#endif
}
void hl_logical_log_indirection(int cl_hint, std::vector<xptr>* blocks)
{
#ifdef LOGICAL_LOG
  if (!enable_log) return;
  number_of_records++;
  tr_llmgr->ll_log_indirection(trid, cl_hint, blocks, true);
#endif
}
void hl_logical_log_commit(transaction_id _trid)
{
#ifdef LOGICAL_LOG
  number_of_records++;
  LONG_LSN commit_lsn;
  commit_lsn = tr_llmgr->ll_log_commit(_trid, true);
  tr_llmgr->ll_log_flush(_trid, true);
  tr_llmgr->flush_last_commit_lsn(commit_lsn);//writes to the logical log file header lsn of last commited function
  //d_printf3("num of records written by transaction id=%d, num=%d\n", trid, tr_llmgr->get_num_of_records_written_by_trn(trid)); 
  string str = string("hl_logical_log_comment finished\n");
  WRITE_DEBUG_LOG(str.c_str());
  //d_printf1("Commit record has been added and flushed\n");
#endif
}

void hl_logical_log_rollback(transaction_id _trid)
{
#ifdef LOGICAL_LOG
  tr_llmgr->ll_log_rollback(_trid, true);
  //d_printf3("num of records written by transaction id=%d, num=%d\n", trid, tr_llmgr->get_num_of_records_written_by_trn(trid)); 

  string str = string("hl_logical_log_rollback finished\n");
  WRITE_DEBUG_LOG(str.c_str());
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
#ifdef LOGICAL_LOG
//    d_printf1("log index operation\n");
    std::ostringstream obj_str(std::ios::out | std::ios::binary);
    std::ostringstream key_str(std::ios::out | std::ios::binary);
//    ostrstream obj_str, key_str;


    PathExpr2lr(object_path, obj_str);
//    d_printf2("obj_path=%s\n", obj_str.str().c_str());
    PathExpr2lr(key_path, key_str);
//    d_printf2("key_path=%s\n", key_str.str().c_str());

    tr_llmgr->ll_log_index(trid, obj_str.str().c_str(), key_str.str().c_str(), key_type, index_title, doc_name, is_doc, inserted, true);

#endif
}
#ifdef SE_ENABLE_FTSEARCH
void hl_logical_log_ft_index(PathExpr *object_path, ft_index_type itconst, char * index_title, const char* doc_name,bool is_doc,pers_sset<ft_custom_cell,unsigned short> * custom_tree,bool inserted)
{
	#ifdef LOGICAL_LOG
	std::ostringstream obj_str(std::ios::out | std::ios::binary);
	PathExpr2lr(object_path, obj_str);

	int custom_tree_count = 0;
	int custom_tree_size = 0;
	char *custom_tree_buf = tr_globals::e_string_buf;

	if (custom_tree != NULL)
	{
		pers_sset<ft_custom_cell,unsigned short>::pers_sset_entry *tmp;
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

			PUT_STR(tmp->obj->ns->uri);
			PUT_STR(tmp->obj->ns->prefix);
			PUT_STR(tmp->obj->local);
#undef PUT_STR

			tmp = custom_tree->rb_successor(tmp);
		}
	}

	//TODO: call something

	#endif
}
#endif