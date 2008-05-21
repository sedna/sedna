/*
 * File:  rcv_funcs.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */
#include "common/sedna.h"
#include <string>
#include <set>

#include "common/base.h"
#include "common/xptr.h"
#include "tr/mo/micro.h"
#include "tr/structures/metadata.h"
#include "tr/structures/indirection.h"
#include "sm/llmgr/llmgr_core.h"
#include "tr/rcv/rcv_funcs.h"
#include "tr/tr_globals.h"
#include "tr/log/log.h"
#include "common/errdbg/d_printf.h"
#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/FTindex.h"
#include "tr/ft/ft_index_data.h"
#endif
#include "common/errdbg/d_printf.h"
#include "common/tr_debug.h"
#include "tr/executor/base/XPath.h"
#include "tr/idx/indexes.h"
#include "tr/updates/updates.h"

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif

#include "common/XptrHash.h"

using namespace std;

static XptrHash <xptr, 16, 16> indir_map; // mapping for redo indirection purposes

int rcv_number_of_records =0;//for debug
int rcv_number_of_text = 0;

#ifdef SE_ENABLE_FTSEARCH
// we need this function since we modify xptrs
static void rcvRecoverFtIndexes()
{
	XptrHash <xptr, 16, 16>::iterator it;
	xptr new_x;
	schema_node *scm;
	schema_ft_ind_cell *sft;

	clear_ft_sequences();

	for (it = indir_map.begin(); it != indir_map.end(); ++it)
	{
		new_x = removeIndirection(*it);

		CHECKP(new_x);
		scm = (GETBLOCKBYNODE(new_x))->snode;
	
	    sft = scm->ft_index_object;

	    while (sft != NULL)
	    {
		    update_insert_sequence(*it, sft->index);
		    update_delete_sequence(it.getKey(), sft->index);
		    sft = sft->next;
		}
	}

	execute_modifications(true);
}
#endif

void rollback_tr_by_logical_log(transaction_id _trid)
{
#ifdef LOGICAL_LOG
  switch_to_rollback_mode(MODE_UNDO);
  tr_llmgr->rollback_trn(trid, exec_micro_op, true);
  string str = string("rollback_tr_by_logical_log finished\n");
  WRITE_DEBUG_LOG(str.c_str());
#endif
}

void recover_db_by_logical_log(const LONG_LSN& last_cp_lsn)
{
  bool isHB;
#ifdef LOGICAL_LOG
  switch_to_rollback_mode(MODE_REDO);
#ifdef SE_ENABLE_FTSEARCH
  isHB = tr_llmgr->recover_db_by_logical_log(SednaIndexJob::recover_db,exec_micro_op, last_cp_lsn, false);  
  if (!isHB) rcvRecoverFtIndexes();	
#else
  tr_llmgr->recover_db_by_logical_log(exec_micro_op, last_cp_lsn, false);	
#endif
  string str = string("recover+db_by_logical_log finished\n");
  WRITE_DEBUG_LOG(str.c_str());

#ifdef TEST_AFTER_RCV
  test_db_after_rcv();
#endif

#endif
}


void exec_micro_op(const char* rec, int len, bool isUNDO, bool isHB)
{
try{
  if (len < 1 )
	 throw SYSTEM_EXCEPTION("bad logical record given from logical log");

  char op = rec[0];

  RECOVERY_CRASH;

  if(op == LL_INSERT_ELEM || op == LL_DELETE_ELEM)
  {
     const char* name, *uri, *prefix;
     xmlscm_type type;
     xptr self, left, right, parent;
     int offs;

     offs = sizeof(char) + sizeof(transaction_id);

     name = rec + offs;
     offs+= strlen(name) + 1;  
     uri = rec +offs;
     offs += strlen(uri) + 1;
     prefix = rec + offs;
     offs += strlen(prefix) + 1;
     memcpy(&type, rec + offs, sizeof(xmlscm_type));
     offs += sizeof(xmlscm_type);
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if((isUNDO && op == LL_DELETE_ELEM) || (!isUNDO && op == LL_INSERT_ELEM))
     {
	   if (!isUNDO)
	   {
            indir_map.find(left, left);
            indir_map.find(right, right);
            indir_map.find(parent, parent);
	   }
	   else
	        set_rollback_record(self);
	   	 
       insert_element(removeIndirection(left),
                      removeIndirection(right),
                      removeIndirection(parent),
                      name,
                      type,
                      (strlen(uri) != 0) ? uri: NULL,
                      (strlen(prefix) != 0) ? prefix: NULL);

       xptr self_res = get_last_indir();
       if (self_res != self) indir_map.insert(self, self_res);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find_remove(self, self);
	   }

       delete_node(removeIndirection(self));
     } 
  }
  else
  if(op == LL_INSERT_ATTR || op == LL_DELETE_ATTR)
  {
     const char* name, *uri, *prefix, *value;
     int value_size;
     xmlscm_type type;
     xptr self, left, right, parent;
     int offs;

     offs = sizeof(char) + sizeof(transaction_id);

     name = rec + offs;
     offs += strlen(name) + 1;
     uri = rec + offs;
     offs += strlen(uri) + 1;
     prefix = rec + offs;
     offs += strlen(prefix) + 1;
     memcpy(&value_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     value = rec + offs;
     offs += value_size;
     memcpy(&type, rec + offs, sizeof(xmlscm_type));
     offs += sizeof(xmlscm_type);
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if ((isUNDO && op == LL_DELETE_ATTR) || (!isUNDO && op == LL_INSERT_ATTR)) 
     {
	   if (!isUNDO)
	   {
            indir_map.find(left, left);
            indir_map.find(right, right);
            indir_map.find(parent, parent);
	   }
	   else
	       set_rollback_record(self);

       insert_attribute(removeIndirection(left),
                        removeIndirection(right),
                        removeIndirection(parent),
                        name,
                        type,
                        value,
                        value_size,
                        (strlen(uri) != 0) ? uri : NULL,
                        (strlen(prefix) != 0) ? prefix: NULL);

       xptr self_res = get_last_indir();
       if (self_res != self) indir_map.insert(self, self_res);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find_remove(self, self);
	   }

       delete_node(removeIndirection(self));
     }  
  }
  else
  if (op == LL_INSERT_TEXT || op == LL_DELETE_TEXT)
  {
     const char* value;
     int value_size;
     xptr self, left, right, parent;
     int offs;

     offs = sizeof(char) + sizeof(transaction_id);

     memcpy(&value_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     value = rec + offs;
     offs += value_size;
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if ((isUNDO && op == LL_DELETE_TEXT) || (!isUNDO && op == LL_INSERT_TEXT))
     {
	   if (!isUNDO)
	   {
            indir_map.find(left, left);
            indir_map.find(right, right);
            indir_map.find(parent, parent);
	   }
	   else
           set_rollback_record(self);

       insert_text(removeIndirection(left),
                   removeIndirection(right),
                   removeIndirection(parent),
                   value,
                   value_size);

       xptr self_res = get_last_indir();
       if (self_res != self) indir_map.insert(self, self_res);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find_remove(self, self);
	   }

       delete_node(removeIndirection(self));
     }
        
  }
  else
  if ( op == LL_INSERT_LEFT_TEXT || op == LL_DELETE_LEFT_TEXT || op == LL_INSERT_RIGHT_TEXT || op == LL_DELETE_RIGHT_TEXT )
  {
     const char* value;
     int value_size;
     xptr self;
     int offs;


     offs = sizeof(char) + sizeof(transaction_id);

     memcpy(&value_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     value = rec + offs;
     offs += value_size;
     memcpy(&self, rec + offs, sizeof(xptr));

     if (   (isUNDO && (op == LL_DELETE_LEFT_TEXT || op == LL_DELETE_RIGHT_TEXT)) || (!isUNDO && (op == LL_INSERT_LEFT_TEXT || op == LL_INSERT_RIGHT_TEXT)))
     {//insert case
	   if (!isUNDO)
	   {
		   indir_map.find(self, self);
	   }

       if (op == LL_INSERT_RIGHT_TEXT || op == LL_DELETE_RIGHT_TEXT)
           appendTextValue(removeIndirection(self), value, value_size,text_mem);
       else
           insertTextValue(removeIndirection(self), value, value_size,text_mem);
     }
     else 
     {//delete case
	   if (!isUNDO)
	   {
	   		indir_map.find(self, self);
	   }

       if (op == LL_INSERT_RIGHT_TEXT || op == LL_DELETE_RIGHT_TEXT)
          delete_text_tail(removeIndirection(self), value_size);
       else
          delete_text_head(removeIndirection(self), value_size);
     }
        
  }
  else
  if (op == LL_INSERT_DOC || op == LL_DELETE_DOC)
  {
    const char* name, *collection;
    xptr self;
    int offs;

    offs = sizeof(char) + sizeof(transaction_id);

    name = rec + offs;
    offs += strlen(name) + 1;
    collection = rec + offs;
    offs += strlen(collection) + 1;
    memcpy(&self, rec + offs, sizeof(xptr));

    if ((isUNDO && op == LL_DELETE_DOC) || (!isUNDO && op == LL_INSERT_DOC))
    {
       if(strlen(collection) == 0)
	   {
          if (isUNDO) set_rollback_record(self);
          insert_document(name);

	      xptr self_res = get_last_indir();
	      if (self_res != self) indir_map.insert(self, self_res);
	   }
       else
       {
		  if (isUNDO) set_rollback_record(self);
          insert_document_in_collection(collection, name);    

	      xptr self_res = get_last_indir();
	      if (self_res != self) indir_map.insert(self, self_res);
       }
    }   
    else
    {
       if(strlen(collection) == 0)
       {
          delete_document(name);
       }
       else
          delete_document(collection, name);

       if (!isUNDO) indir_map.remove(self);
    }
  }
  else
  if (op == LL_INSERT_COMMENT || op == LL_DELETE_COMMENT)
  {
     const char* value;
     int value_size;
     xptr self, left, right, parent;
     int offs;

     offs = sizeof(char) + sizeof(transaction_id);

     memcpy(&value_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     value = rec + offs;
     offs += value_size;
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if((isUNDO && op == LL_DELETE_COMMENT) || (!isUNDO && op == LL_INSERT_COMMENT))
     {
	   if (!isUNDO)
	   {
            indir_map.find(left, left);
            indir_map.find(right, right);
            indir_map.find(parent, parent);
	   }
	   else
	       set_rollback_record(self);

       insert_comment(removeIndirection(left),
                      removeIndirection(right),
                      removeIndirection(parent),
                      value,
                      value_size);

       xptr self_res = get_last_indir();
       if (self_res != self) indir_map.insert(self, self_res);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find_remove(self, self);
	   }

       delete_node(removeIndirection(self));
     }
  }
  else
  if(op == LL_INSERT_PI || op == LL_DELETE_PI)
  {
     const char* value;
     int total_size;
     shft target_size;
     xptr self, left, right, parent;
     int offs;

     offs = sizeof(char) + sizeof(transaction_id);

     memcpy(&total_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     memcpy(&target_size, rec + offs, sizeof(shft));
     offs += sizeof(shft);
     value = rec + offs;
     offs += total_size;
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if ((isUNDO && op == LL_DELETE_PI) || (!isUNDO && op == LL_INSERT_PI))
     {
	   if (!isUNDO)
	   {
            indir_map.find(left, left);
            indir_map.find(right, right);
            indir_map.find(parent, parent);
	   }
	   else
	       set_rollback_record(self);

       insert_pi(removeIndirection(left),
                 removeIndirection(right),
                 removeIndirection(parent),
                 value,
                 target_size,
                 value+(target_size),
                (total_size)-(target_size));

       xptr self_res = get_last_indir();
       if (self_res != self) indir_map.insert(self, self_res);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find_remove(self, self);
	   }

       delete_node(removeIndirection(self));
     }
  }
  else
  if(op == LL_INSERT_COLLECTION || op == LL_DELETE_COLLECTION)
  {
     const char* name;
     int offs;

     offs = sizeof(char) + sizeof(transaction_id);

     name = rec + offs;

     if ((isUNDO && op == LL_DELETE_COLLECTION) || (!isUNDO && op == LL_INSERT_COLLECTION))
     {
        insert_collection(name);
     }
     else
     {
        delete_collection(name);
     }
  }
  else
  if(op == LL_INSERT_NS || op == LL_DELETE_NS)
  {
     const char *uri, *prefix;
     xptr self, left, right, parent;
     int offs;

     offs = sizeof(char) + sizeof(transaction_id);

     uri = rec + offs;
     offs += strlen(uri) + 1;
     prefix = rec + offs;
     offs += strlen(prefix) + 1;
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if((isUNDO && op == LL_DELETE_NS) || (!isUNDO && op == LL_INSERT_NS))
     {
	   if (!isUNDO)
	   {
            indir_map.find(left, left);
            indir_map.find(right, right);
            indir_map.find(parent, parent);
	   }
	   else
	       set_rollback_record(self);

       insert_namespace(removeIndirection(left),
                        removeIndirection(right),
                        removeIndirection(parent),
                        strlen(uri) ? uri : NULL,
                        strlen(prefix) ? prefix : NULL);

       xptr self_res = get_last_indir();
       if (self_res != self) indir_map.insert(self, self_res);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find_remove(self, self);
	   }

       delete_node(removeIndirection(self));
     }
     
  }
  else
  if(op == LL_INSERT_DOC_INDEX || op == LL_DELETE_DOC_INDEX || op == LL_INSERT_COL_INDEX || op == LL_DELETE_COL_INDEX)
  {
     const char *obj_path, *key_path, *ind_name, *doc_name;
     xmlscm_type key_type;
     int offs;


     offs = sizeof(char) + sizeof(transaction_id);

     obj_path = rec + offs;
     offs += strlen(obj_path) + 1;
     key_path = rec + offs;
     offs += strlen(key_path) + 1;
     memcpy(&key_type, rec + offs, sizeof(xmlscm_type));
     offs += sizeof(xmlscm_type);
     ind_name = rec + offs;
     offs += strlen(ind_name) + 1;
     doc_name = rec + offs;

     if((isUNDO && (op == LL_DELETE_DOC_INDEX || op == LL_DELETE_COL_INDEX) ) || (!isUNDO && (op == LL_INSERT_DOC_INDEX || op == LL_INSERT_COL_INDEX)))
     {//create index
//        d_printf1("rollback index operation begin\n");
        if (op == LL_DELETE_DOC_INDEX || op == LL_INSERT_DOC_INDEX)
        {
//           d_printf2("obj_path=%s\n", obj_path);
//           d_printf2("key_path=%s\n", key_path);
           schema_node *doc_node = find_document(doc_name);
//           d_printf1("doc found\n");
           if (doc_node->type == document || doc_node->type == virtual_root)
              create_index (lr2PathExpr(NULL, obj_path, true),
                            lr2PathExpr(NULL, key_path, true),
                            key_type,
                            (doc_schema_node*)doc_node,
                            ind_name,
                            doc_name,
                            true);
           else throw SYSTEM_EXCEPTION("Can't create index for document");
        } 
        else
        {
//           d_printf2("obj_path=%s\n", obj_path);
//           d_printf2("key_path=%s\n", key_path);
           schema_node *col_node = find_collection(doc_name);
           if (col_node->type == document || col_node->type == virtual_root)
               create_index (lr2PathExpr(NULL, obj_path, true),
                             lr2PathExpr(NULL, key_path, true),
                             key_type,
                             (doc_schema_node*)col_node,
                             ind_name,
                             doc_name,
                             false);
           else throw SYSTEM_EXCEPTION("Can't create index for collection");

        }
     }
     else
     {//delete index
//          d_printf2("ind_name=%s\n", ind_name);
          delete_index (ind_name);
     }
//     d_printf1("rollback index operation end\n");

  }
#ifdef SE_ENABLE_FTSEARCH
  else if(op == LL_INSERT_DOC_FTS_INDEX || op == LL_DELETE_DOC_FTS_INDEX || op == LL_INSERT_COL_FTS_INDEX || op == LL_DELETE_COL_FTS_INDEX)
  {
     const char *obj_path, *ind_name, *doc_name, *custom_tree_buf;
     int itconst;
     int custom_tree_size;
     int offs;


     offs = sizeof(char) + sizeof(transaction_id);

     obj_path = rec + offs;
     offs += strlen(obj_path) + 1;
     memcpy(&itconst, rec + offs, sizeof(int));
     offs += sizeof(int);
     ind_name = rec + offs;
     offs += strlen(ind_name) + 1;
     doc_name = rec + offs;
     offs += strlen(doc_name) + 1;
     memcpy(&custom_tree_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     custom_tree_buf = rec + offs;

     if((isUNDO && (op == LL_DELETE_DOC_FTS_INDEX || op == LL_DELETE_COL_FTS_INDEX) ) || (!isUNDO && (op == LL_INSERT_DOC_FTS_INDEX || op == LL_INSERT_COL_FTS_INDEX)))
     {//create index
//        d_printf1("rollback index operation begin\n");
        if (op == LL_DELETE_DOC_FTS_INDEX || op == LL_INSERT_DOC_FTS_INDEX)
        {;

           schema_node *doc_node = find_document(doc_name);
//           d_printf1("doc found\n");

           if (doc_node->type == document || doc_node->type == virtual_root)
              ft_index_cell::create_index (lr2PathExpr(NULL, obj_path, true),
                            (ft_index_type)itconst,
                            (doc_schema_node*)doc_node,
                            ind_name,
                            doc_name,
                            true,
                            ft_rebuild_cust_tree(custom_tree_buf, custom_tree_size),
							true);
           else throw SYSTEM_EXCEPTION("Can't create index for document");
        } 
        else
        {;

//           d_printf2("obj_path=%s\n", obj_path);
//           d_printf2("key_path=%s\n", key_path);
           schema_node *col_node = find_collection(doc_name);
           if (col_node->type == document || col_node->type == virtual_root)
              ft_index_cell::create_index (lr2PathExpr(NULL, obj_path, true),
                            (ft_index_type)itconst,
                            (doc_schema_node*)col_node,
                            ind_name,
                            doc_name,
                            false,
                            ft_rebuild_cust_tree(custom_tree_buf, custom_tree_size),
							true);
           else throw SYSTEM_EXCEPTION("Can't create index for collection");

        }
     }
     else
     {//delete index
//          d_printf2("ind_name=%s\n", ind_name);
         ;
          ft_index_cell::delete_index (ind_name, true);
     }
//     d_printf1("rollback index operation end\n");

  }
#endif
#ifdef SE_ENABLE_TRIGGERS
  else if (op == LL_INSERT_DOC_TRG || op == LL_DELETE_DOC_TRG || op == LL_INSERT_COL_TRG || op == LL_DELETE_COL_TRG)
  {
    const char *trigger_path, *path_to_parent, *trigger_title, *doc_name;
    trigger_time tr_time;
    trigger_event tr_event;
    trigger_granularity tr_gran;
    int tr_action_size;
    const char *tr_action_buf;
    inserting_node innode;

    int tmp;

    int offs = sizeof(char) + sizeof(transaction_id);
    
	memcpy(&tmp, rec + offs, sizeof(int));
    offs += sizeof(int);
    tr_time = (trigger_time)tmp;

    memcpy(&tmp, rec + offs, sizeof(int));
    offs += sizeof(int);
    tr_event = (trigger_event)tmp;

    trigger_path = rec + offs;
    offs += strlen(trigger_path) + 1;

    memcpy(&tmp, rec + offs, sizeof(int));
    offs += sizeof(int);
    tr_gran = (trigger_granularity)tmp;

    memcpy(&tr_action_size, rec + offs, sizeof(int));
    offs += sizeof(int);

    tr_action_buf = rec + offs;
    offs += tr_action_size;

    innode.name = const_cast<char *>(rec) + offs;
    offs += strlen(innode.name) + 1;

    memcpy(&tmp, rec + offs, sizeof(int));
    offs += sizeof(int);
    innode.type = (t_item)tmp;

    path_to_parent = rec + offs;
    offs += strlen(path_to_parent) + 1;

    trigger_title = rec + offs;
    offs += strlen(trigger_title) + 1;
    
    doc_name = rec + offs;

    // restore trigger_action_cell sequence
    int i = 0;
    trigger_action_cell *trac = (trigger_action_cell *)scm_malloc(sizeof(trigger_action_cell),true);
    rcv_tac = trac;
    
    while (i < tr_action_size)
    {
        trac->statement = (char *)scm_malloc(strlen(tr_action_buf + i) + 1, true);
        strcpy(trac->statement, tr_action_buf + i);
        i += strlen(tr_action_buf + i) + 1;

        memcpy(&(trac->cxt_size), tr_action_buf + i, sizeof(int));
        i += sizeof(int);

        if (i < tr_action_size) 
        	trac->next = (trigger_action_cell *)scm_malloc(sizeof(trigger_action_cell), true);
        else
        	trac->next = NULL;

        trac = trac->next;
    }

    U_ASSERT(i == tr_action_size);
        
    if ((isUNDO && (op == LL_DELETE_DOC_TRG || op == LL_DELETE_COL_TRG) ) || (!isUNDO && (op == LL_INSERT_DOC_TRG || op == LL_INSERT_COL_TRG)))
    {//create trigger
        if (op == LL_DELETE_DOC_TRG || op == LL_INSERT_DOC_TRG)
        {// is_doc - true
           schema_node *doc_node = find_document(doc_name);

           if (doc_node->type == document || doc_node->type == virtual_root)
               trigger_cell::create_trigger(tr_time, tr_event, 
               								(strlen(trigger_path)) ? lr2PathExpr(NULL, trigger_path, true) : NULL,
               								tr_gran,
                                            NULL,
                                            innode,
                                            (strlen(path_to_parent)) ? lr2PathExpr(NULL, path_to_parent, true) : NULL,
                                            (doc_schema_node *)doc_node,
                                            trigger_title, doc_name, true);
           else throw SYSTEM_EXCEPTION("Can't create trigger for document");
   		}
        else
        {      
           schema_node *coll_node = find_collection(doc_name);

           if (coll_node->type == document || coll_node->type == virtual_root)
               trigger_cell::create_trigger(tr_time, tr_event, 
               								(strlen(trigger_path)) ? lr2PathExpr(NULL, trigger_path, true) : NULL,
               								tr_gran,
                                            NULL,
                                            innode,
                                            (strlen(path_to_parent)) ? lr2PathExpr(NULL, path_to_parent, true) : NULL,
                                            (doc_schema_node *)coll_node,
                                            trigger_title, doc_name, false);
           else throw SYSTEM_EXCEPTION("Can't create trigger for collection");
        }      
    }
    else // delete trigger
    	trigger_cell::delete_trigger(trigger_title);
  }
#endif  
  else
    throw SYSTEM_EXCEPTION("bad logical record given from logical log");

  
  rcv_number_of_records++;
  //d_printf2("exec micro op completed num=%d\n", rcv_number_of_records);

  string str = string("exec_micro_op finished\n");
  WRITE_DEBUG_LOG(str.c_str());

} catch (ANY_SE_EXCEPTION) {
  d_printf1("\n\n!!!EXEC_MICRO_OP EXCEPTION!!!\n\n");
  throw;
}
}

void print_value(const char* value, int value_size)
{
   d_printf1("value=");
   for (int i=0; i<value_size; i++)
     d_printf2("%c", value[i]);

   d_printf1("#\n");
}
