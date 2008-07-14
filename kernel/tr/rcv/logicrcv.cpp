/*
 * File:  logicrcv.cpp - Logical recovery
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Logical recovery is a process of sequential scanning of records and calling corresponding microoperations.
 * To specify function for every type of operation use llRcvLogRecsInfo structure.
 *
 */

#include "tr/updates/updates.h"
#include "common/XptrHash.h"

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif

#include "common/base.h"
#include "common/xptr.h"
#include "tr/mo/micro.h"
#include "tr/structures/metadata.h"
#include "tr/structures/indirection.h"
#include "sm/llsm/llMain.h"
#include "tr/rcv/rcv_funcs.h"
#include "tr/tr_globals.h"
#include "tr/log/log.h"
#include "common/errdbg/d_printf.h"
#include "tr/rcv/logican.h"

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/FTindex.h"
#include "tr/ft/ft_index_data.h"
#endif

#include "common/tr_debug.h"
#include "tr/executor/base/XPath.h"
#include "tr/idx/indexes.h"

#include <assert.h>

static XptrHash <xptr, 16, 16> indir_map; // mapping for redo indirection purposes
static trn_cell_analysis_redo *rcv_list = NULL;
static LSN end_rcv_lsn;

// Returns previous lsn for rollback
static LSN llGetPrevRollbackLsn(LSN curr_lsn, void *RecBuf)
{
	return llGetPrevLsnFromRecord(RecBuf);
}

static LSN llGetNextRcvRec(LSN curr_lsn, void *RecBuf)
{
	LSN lsn = curr_lsn + llGetRecordSize(RecBuf, 0);

    if (lsn >= end_rcv_lsn) // we don't need synchronization here since this is one-thread access
    	return LFS_INVALID_LSN;
    
    return lsn;
}

// called when something bad happen
static void llProcessRcvError(LSN curr_lsn, void *Rec)
{
	char op = *((char *)Rec);
	
	if (op < LL_INSERT_ELEM || op >= LL_DEFAULT)
		throw SYSTEM_EXCEPTION("Bad things really happen :(");
}

static bool llRcvPrereqRedo(LSN lsn, void *RecBuf)
{
	char *rec = (char *)RecBuf;
	trn_cell_analysis_redo *redo_trn_cell;
	bool res;

	// this function tries to find transaction which start_lsn <=lsn <=end_lsn
	redo_trn_cell = llFindTrnCell(rcv_list, *((transaction_id *)(rec + sizeof(char))), lsn);

    return (redo_trn_cell != NULL && redo_trn_cell->finish_status == TRN_COMMIT_FINISHED);
}

// Recover element
static void llRcvElement(LSN curr_lsn, void *Rec)
{
	char *rec = (char *)Rec;
	const char *name, *uri, *prefix;
	xmlscm_type type;
	xptr self, left, right, parent;
	int offs;
	bool isUNDO = rollback_active;
	char op = rec[0];

    offs = sizeof(char) + sizeof(transaction_id);

    name = rec + offs;
    offs += strlen(name) + 1;  
    uri = rec + offs;
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

    if ((isUNDO && op == LL_DELETE_ELEM) || (!isUNDO && op == LL_INSERT_ELEM))
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

// Recover attribute
static void llRcvAttribute(LSN curr_lsn, void *Rec)
{
	 char *rec = (char *)Rec;
     const char* name, *uri, *prefix, *value;
     int value_size;
     xmlscm_type type;
     xptr self, left, right, parent;
     int offs;
	 bool isUNDO = rollback_active;
	 char op = rec[0];

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
                        (strlen(prefix) != 0) ? prefix : NULL);

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

// Recover text node
static void llRcvText(LSN curr_lsn, void *Rec)
{
	 char *rec = (char *)Rec;
     const char* value;
     int value_size;
     xptr self, left, right, parent;
     int offs;
	 bool isUNDO = rollback_active;
	 char op = rec[0];

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

// Recover text edit info
static void llRcvTextEdit(LSN curr_lsn, void *Rec)
{
	 char *rec = (char *)Rec;
     const char* value;
     int value_size;
     xptr self;
     int offs;
	 bool isUNDO = rollback_active;
	 char op = rec[0];

     offs = sizeof(char) + sizeof(transaction_id);

     memcpy(&value_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     value = rec + offs;
     offs += value_size;
     memcpy(&self, rec + offs, sizeof(xptr));

     if ((isUNDO && (op == LL_DELETE_LEFT_TEXT || op == LL_DELETE_RIGHT_TEXT)) || 
     	 (!isUNDO && (op == LL_INSERT_LEFT_TEXT || op == LL_INSERT_RIGHT_TEXT)))
     {
	   if (!isUNDO)
	   {
		   indir_map.find(self, self);
	   }

       if (op == LL_INSERT_RIGHT_TEXT || op == LL_DELETE_RIGHT_TEXT)
           appendTextValue(removeIndirection(self), value, value_size, text_mem);
       else
           insertTextValue(removeIndirection(self), value, value_size, text_mem);
     }
     else 
     {
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

// Recover document node
static void llRcvDoc(LSN curr_lsn, void *Rec)
{
    char *rec = (char *)Rec;
    const char* name, *collection;
    xptr self;
    int offs;
	bool isUNDO = rollback_active;
	char op = rec[0];

    offs = sizeof(char) + sizeof(transaction_id);

    name = rec + offs;
    offs += strlen(name) + 1;
    collection = rec + offs;
    offs += strlen(collection) + 1;
    memcpy(&self, rec + offs, sizeof(xptr));

    if ((isUNDO && op == LL_DELETE_DOC) || (!isUNDO && op == LL_INSERT_DOC))
    {
       if (strlen(collection) == 0)
	   {
          if (isUNDO) set_rollback_record(self);
          insert_document(name);
	   }
       else
       {
		  if (isUNDO) set_rollback_record(self);
          insert_document_in_collection(collection, name);    
       }

	   xptr self_res = get_last_indir();
	   if (self_res != self) indir_map.insert(self, self_res);
    }   
    else
    {
       if (strlen(collection) == 0)
       {
          delete_document(name);
       }
       else
          delete_document(collection, name);

       if (!isUNDO) indir_map.remove(self);
    }
}

// Recover comment node
static void llRcvComment(LSN curr_lsn, void *Rec)
{
	 char *rec = (char *)Rec;
     const char* value;
     int value_size;
     xptr self, left, right, parent;
     int offs;
	 bool isUNDO = rollback_active;
	 char op = rec[0];

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

// Recover pi node
static void llRcvPI(LSN curr_lsn, void *Rec)
{
	 char *rec = (char *)Rec;
     const char* value;
     int total_size;
     shft target_size;
     xptr self, left, right, parent;
     int offs;
	 bool isUNDO = rollback_active;
	 char op = rec[0];

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
                 value + target_size,
                 total_size - target_size);

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


// Recover collection
static void llRcvCollection(LSN curr_lsn, void *Rec)
{  
	 char *rec = (char *)Rec;
     const char* name;
     int offs;
	 bool isUNDO = rollback_active;
	 char op = rec[0];

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

// Recover namespace
static void llRcvNS(LSN curr_lsn, void *Rec)
{
	 char *rec = (char *)Rec;
     const char *uri, *prefix;
     xptr self, left, right, parent;
     int offs;
	 bool isUNDO = rollback_active;
	 char op = rec[0];

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

// Recover index
static void llRcvIndex(LSN curr_lsn, void *Rec)
{
	 char *rec = (char *)Rec;
     const char *obj_path, *key_path, *ind_name, *doc_name;
     xmlscm_type key_type;
     int offs;
	 bool isUNDO = rollback_active;
	 char op = rec[0];

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

     if ((isUNDO && (op == LL_DELETE_DOC_INDEX || op == LL_DELETE_COL_INDEX)) || (!isUNDO && (op == LL_INSERT_DOC_INDEX || op == LL_INSERT_COL_INDEX)))
     {
        if (op == LL_DELETE_DOC_INDEX || op == LL_INSERT_DOC_INDEX)
        {
           schema_node *doc_node = find_document(doc_name);
           if (doc_node->type == document || doc_node->type == virtual_root)
              create_index (lr2PathExpr(NULL, obj_path, true),
                            lr2PathExpr(NULL, key_path, true),
                            key_type,
                            (doc_schema_node *)doc_node,
                            ind_name,
                            doc_name,
                            true);
           else throw SYSTEM_EXCEPTION("Can't create index for document");
        } 
        else
        {
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
     {
          delete_index(ind_name);
     }
}

// Recover full-text index
static void llRcvFtIndex(LSN curr_lsn, void *Rec)
{
#ifdef SE_ENABLE_FTSEARCH
	 char *rec = (char *)Rec;
     const char *obj_path, *ind_name, *doc_name, *custom_tree_buf;
     int itconst;
     int custom_tree_size;
     int offs;
	 bool isUNDO = rollback_active;
	 char op = rec[0];

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
        if (op == LL_DELETE_DOC_FTS_INDEX || op == LL_INSERT_DOC_FTS_INDEX)
        {
           schema_node *doc_node = find_document(doc_name);

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
        {
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
          ft_index_cell::delete_index(ind_name, true);
     }
#endif
}

// Recover trigger
static void llRcvTrigger(LSN curr_lsn, void *Rec)
{
#ifdef SE_ENABLE_TRIGGERS
	char *rec = (char *)Rec;
    const char *trigger_path, *path_to_parent, *trigger_title, *doc_name;
    trigger_time tr_time;
    trigger_event tr_event;
    trigger_granularity tr_gran;
    int tr_action_size;
    const char *tr_action_buf;
    inserting_node innode;
	bool isUNDO = rollback_active;
	char op = rec[0];

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

    innode.name = rec + offs;
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
    trigger_action_cell *trac = (trigger_action_cell *)scm_malloc(sizeof(trigger_action_cell), true);
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
#endif  
}

// Main structure for logical recovery
static
struct llRecInfo llRcvLogRecsInfo[] = 
{
	{LL_INSERT_ELEM, llRcvElement},
	{LL_DELETE_ELEM, llRcvElement},
	{LL_INSERT_ATTR, llRcvAttribute},
	{LL_DELETE_ATTR, llRcvAttribute},
	{LL_INSERT_TEXT, llRcvText},
	{LL_DELETE_TEXT, llRcvText},
	{LL_INSERT_LEFT_TEXT, llRcvTextEdit},
	{LL_DELETE_LEFT_TEXT, llRcvTextEdit},
	{LL_INSERT_RIGHT_TEXT, llRcvTextEdit},
	{LL_DELETE_RIGHT_TEXT, llRcvTextEdit},
	{LL_INSERT_DOC, llRcvDoc},
	{LL_DELETE_DOC, llRcvDoc},
	{LL_INSERT_COMMENT, llRcvComment},
	{LL_DELETE_COMMENT, llRcvComment},
	{LL_INSERT_PI, llRcvPI},
	{LL_DELETE_PI, llRcvPI},
	{LL_INSERT_COLLECTION, llRcvCollection},
	{LL_DELETE_COLLECTION, llRcvCollection},
	{LL_INSERT_NS, llRcvNS},
	{LL_DELETE_NS, llRcvNS},
	{LL_INSERT_DOC_INDEX, llRcvIndex},
	{LL_DELETE_DOC_INDEX, llRcvIndex},
	{LL_INSERT_COL_INDEX, llRcvIndex},
	{LL_DELETE_COL_INDEX, llRcvIndex},
	{LL_INSERT_DOC_FTS_INDEX, llRcvFtIndex},
	{LL_DELETE_DOC_FTS_INDEX, llRcvFtIndex},
	{LL_INSERT_COL_FTS_INDEX, llRcvFtIndex},
	{LL_DELETE_COL_FTS_INDEX, llRcvFtIndex},
	{LL_INSERT_DOC_TRG, llRcvTrigger},
	{LL_DELETE_DOC_TRG, llRcvTrigger},
	{LL_INSERT_COL_TRG, llRcvTrigger},
	{LL_DELETE_COL_TRG, llRcvTrigger},
};
static int llRcvLogRecsInfoLen = sizeof(llRcvLogRecsInfo) / sizeof(llRecInfo);

void llLogRollbackTrn(transaction_id trid)
{
	RECOVERY_CRASH;

	rollback_active = true;

	llInfo->llTransInfoTable[trid].mode = ROLLBACK_MODE;

	if (llInfo->llTransInfoTable[trid].last_lsn == LFS_INVALID_LSN)
	{
		rollback_active = false;
		return;
	}

	// flush all its records (do we really need to do this?)
	llFlushTransRecs(trid);
    
	// rollback transaction by scaning all its records
	llScanRecords(llRcvLogRecsInfo, llRcvLogRecsInfoLen, llInfo->llTransInfoTable[trid].last_lsn, llGetPrevRollbackLsn, NULL);

	rollback_active = false;
}

static void llRcvRedoTrns(trn_cell_analysis_redo *rcv_list, LSN start_lsn, LSN end_lsn)
{
	if (rcv_list == NULL) return;

	if (start_lsn >= end_lsn )
		throw USER_EXCEPTION(SE4152);

	end_rcv_lsn = end_lsn;

	// redo transactions by scaning all its records
	llScanRecords(llRcvLogRecsInfo, llRcvLogRecsInfoLen, start_lsn, llGetNextRcvRec, llRcvPrereqRedo);
}

// this function is run from the special recovery process
void llLogicalRecover(const LSN start_lsn)
{
	LSN start_analysis_lsn; 

	assert(llInfo->checkpoint_lsn != LFS_INVALID_LSN || llInfo->last_lsn != LFS_INVALID_LSN);

	// determine starting lsn to analyze transactions
	if (llInfo->min_rcv_lsn != LFS_INVALID_LSN) 
		start_analysis_lsn = llInfo->min_rcv_lsn;
	else if (llInfo->checkpoint_lsn != LFS_INVALID_LSN)	
		start_analysis_lsn = llInfo->checkpoint_lsn;
	else
		return;

	// flush all physical records, if any
    llFlushAll();

	// determine transactions that need to be recovered
	rcv_list = llGetRedoList(start_analysis_lsn);

	//redo committed transactions
	llRcvRedoTrns(rcv_list, start_analysis_lsn, llInfo->last_lsn);
  
	RECOVERY_CRASH;
 
#ifdef SE_ENABLE_FTSEARCH
	SednaIndexJob::recover_db(rcv_list, (llInfo->hotbackup_needed) ? true : false);
#endif

	llDestroyRedoList(rcv_list);
}

#ifdef SE_ENABLE_FTSEARCH
// this function performs remapping on ft-indexes (needed since we use redo-remapping)
void rcvRecoverFtIndexes()
{
	XptrHash <xptr, 16, 16>::iterator it;
	xptr new_x;
	schema_node *scm;
	schema_ft_ind_cell *sft;

	if (llInfo->hotbackup_needed) return; // since we rebuild ft-indexes on hot-backup anyway

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

	execute_modifications();
}
#endif
