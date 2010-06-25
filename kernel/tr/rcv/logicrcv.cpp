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
#include "tr/triggers/triggers.h"
#endif

#include "common/base.h"
#include "common/xptr.h"
#include "tr/mo/mo.h"
#include "tr/structures/metadata.h"
#include "tr/mo/indirection.h"
#include "common/llcommon/llMain.h"
#include "tr/rcv/rcv_funcs.h"
#include "tr/tr_globals.h"
#include "tr/log/log.h"
#include "common/errdbg/d_printf.h"
#include "tr/rcv/logican.h"

#ifdef SE_ENABLE_DTSEARCH
#include "tr/ft/FTindex.h"
#endif
#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#endif

#include "common/tr_debug.h"
#include "tr/executor/base/XPath.h"
#include "tr/idx/indexes.h"

#include <assert.h>

#include "tr/mo/mo.h"
#include "tr/mo/microoperations.h"

static XptrHash <xptr, 16, 16> indir_map; // mapping for redo indirection purposes
static trn_cell_analysis_redo *rcv_list = NULL;
static LSN highRcvLSN = 0; // this is an upper boundary for redo recovery process

// Returns previous lsn for rollback
static LSN llGetPrevRollbackLsn(LSN curr_lsn, void *RecBuf)
{
	return llGetPrevLsnFromRecord(RecBuf);
}

/* Converts logical log namespace presentation into pointer the real object */
static inline xmlns_ptr
llGetNamespaceFromRecord(const char* prefix,
                         const char* uri)
{
    U_ASSERT(prefix != NULL && uri != NULL);
    if(strlen(prefix) == 0 && strlen(uri) == 0) return NULL_XMLNS;
    else return xmlns_touch(prefix, uri);
}

static LSN llGetNextRcvRec(LSN curr_lsn, void *RecBuf)
{
	LSN lsn = curr_lsn + llGetRecordSize(RecBuf, 0);

    // if we walk out upper bound then we should stop redo right away
    if (lsn > highRcvLSN) return LFS_INVALID_LSN;

    // we don't need to check lsn validity since lfsGetRecord in llScan will do it for us
    return lsn;
}

static bool llRcvPrereqRedo(LSN lsn, void *RecBuf)
{
	char *rec = (char *)RecBuf;
	trn_cell_analysis_redo *redo_trn_cell;

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
	size_t offs;
	bool isUNDO = (rollback_active != 0);
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
           indir_map.find(left, &left);
           indir_map.find(right, &right);
           indir_map.find(parent, &parent);
	  }
	  else
	       indirectionSetRollbackRecord(self);

      xmlns_ptr ns = llGetNamespaceFromRecord(prefix, uri);

      insert_element(removeIndirection(left),
                     removeIndirection(right),
                     removeIndirection(parent),
                     name,
                     type,
                     ns);

      xptr self_res = indirectionGetLastRecord();
      if (self_res != self) indir_map.insert(self, self_res);
    }
    else
    {
	  if (!isUNDO)
	  {
	  	  indir_map.find_remove(self, &self);
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
     size_t offs;
	 bool isUNDO = (rollback_active != 0);
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
            indir_map.find(left, &left);
            indir_map.find(right, &right);
            indir_map.find(parent, &parent);
	   }
	   else
	        indirectionSetRollbackRecord(self);

       xmlns_ptr ns = llGetNamespaceFromRecord(prefix, uri);

       insert_attribute(removeIndirection(left),
                        removeIndirection(right),
                        removeIndirection(parent),
                        name,
                        type,
                        value,
                        value_size,
                        ns);

       xptr self_res = indirectionGetLastRecord();
       if (self_res != self) indir_map.insert(self, self_res);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find_remove(self, &self);
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
     size_t offs;
	 bool isUNDO = (rollback_active != 0);
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
            indir_map.find(left, &left);
            indir_map.find(right, &right);
            indir_map.find(parent, &parent);
	   }
	   else
          indirectionSetRollbackRecord(self);

       insert_text(removeIndirection(left),
                   removeIndirection(right),
                   removeIndirection(parent),
                   value,
                   value_size);

       xptr self_res = indirectionGetLastRecord();
       if (self_res != self) indir_map.insert(self, self_res);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find_remove(self, &self);
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
     size_t offs;
	 bool isUNDO = (rollback_active != 0);
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
		   indir_map.find(self, &self);
	   }

       if (op == LL_INSERT_RIGHT_TEXT || op == LL_DELETE_RIGHT_TEXT)
           insertTextValue(ip_tail, removeIndirection(self), value, value_size, text_mem);
       else
           insertTextValue(ip_head, removeIndirection(self), value, value_size, text_mem);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find(self, &self);
	   }

       if (op == LL_INSERT_RIGHT_TEXT || op == LL_DELETE_RIGHT_TEXT)
          deleteTextValue(ip_tail, removeIndirection(self), value_size);
       else
          deleteTextValue(ip_head, removeIndirection(self), value_size);
     }
}

// Recover document node
static void llRcvRenameColl(LSN curr_lsn, void *Rec)
{
    char *rec = (char *)Rec;
    const char *old_name, *new_name;
    size_t offs;
	bool isUNDO = (rollback_active != 0);
	char op = rec[0];

    assert(op == LL_RENAME_COLLECTION);

    offs = sizeof(char) + sizeof(transaction_id);

    old_name = rec + offs;
    offs += strlen(old_name) + 1;
    new_name = rec + offs;

    if (isUNDO)
    {
    	rename_collection(new_name, old_name);
    }
    else
    {
    	rename_collection(old_name, new_name);
    }
}

// Recover document node
static void llRcvDoc(LSN curr_lsn, void *Rec)
{
    char *rec = (char *)Rec;
    const char* name, *collection;
    xptr self;
    size_t offs;
	bool isUNDO = (rollback_active != 0);
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
          if (isUNDO) indirectionSetRollbackRecord(self);
          insert_document(name);
	   }
       else
       {
		  if (isUNDO) indirectionSetRollbackRecord(self);
          insert_document_into_collection(collection, name);
       }

	   xptr self_res = indirectionGetLastRecord();
	   if (self_res != self) indir_map.insert(self, self_res);
    }
    else
    {
       if (strlen(collection) == 0)
       {
          delete_document(name);
       }
       else
          delete_document_from_collection(collection, name);

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
     size_t offs;
	 bool isUNDO = (rollback_active != 0);
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
            indir_map.find(left, &left);
            indir_map.find(right, &right);
            indir_map.find(parent, &parent);
	   }
	   else
	       indirectionSetRollbackRecord(self);

       insert_comment(removeIndirection(left),
                      removeIndirection(right),
                      removeIndirection(parent),
                      value,
                      value_size);

       xptr self_res = indirectionGetLastRecord();
       if (self_res != self) indir_map.insert(self, self_res);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find_remove(self, &self);
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
     size_t offs;
	 bool isUNDO = (rollback_active != 0);
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
            indir_map.find(left, &left);
            indir_map.find(right, &right);
            indir_map.find(parent, &parent);
	   }
	   else
	       indirectionSetRollbackRecord(self);

       insert_pi(removeIndirection(left),
                 removeIndirection(right),
                 removeIndirection(parent),
                 value,
                 target_size,
                 value + target_size,
                 total_size - target_size);

       xptr self_res = indirectionGetLastRecord();
       if (self_res != self) indir_map.insert(self, self_res);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find_remove(self, &self);
	   }

       delete_node(removeIndirection(self));
     }
}


// Recover collection
static void llRcvCollection(LSN curr_lsn, void *Rec)
{
	 char *rec = (char *)Rec;
     const char* name;
     size_t offs;
	 bool isUNDO = (rollback_active != 0);
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
     size_t offs;
	 bool isUNDO = (rollback_active != 0);
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
            indir_map.find(left, &left);
            indir_map.find(right, &right);
            indir_map.find(parent, &parent);
	   }
	   else
	       indirectionSetRollbackRecord(self);

       xmlns_ptr ns = llGetNamespaceFromRecord(prefix, uri);
       U_ASSERT(ns != NULL_XMLNS);

       insert_namespace(removeIndirection(left),
                        removeIndirection(right),
                        removeIndirection(parent),
                        ns);

       xptr self_res = indirectionGetLastRecord();
       if (self_res != self) indir_map.insert(self, self_res);
     }
     else
     {
	   if (!isUNDO)
	   {
	   		indir_map.find_remove(self, &self);
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
     size_t offs;
	 bool isUNDO = (rollback_active != 0);
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
           doc_schema_node_cptr doc_node = find_document(doc_name);
           if (doc_node->type == document || doc_node->type == virtual_root)
              create_index (lr2PathExpr(NULL, obj_path, pe_catalog_aspace),
                            lr2PathExpr(NULL, key_path, pe_catalog_aspace),
                            key_type,
                            doc_node,
                            ind_name,
                            doc_name,
                            true);
           else throw SYSTEM_EXCEPTION("Can't create index for document");
        }
        else
        {
           col_schema_node_cptr col_node = find_collection(doc_name);
           if (col_node->type == document || col_node->type == virtual_root)
               create_index (lr2PathExpr(NULL, obj_path, pe_catalog_aspace),
                             lr2PathExpr(NULL, key_path, pe_catalog_aspace),
                             key_type,
                             col_node.ptr(),
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
#ifdef SE_ENABLE_DTSEARCH //TODO: add stuff for FTSEARCH
	 char *rec = (char *)Rec;
     const char *obj_path, *ind_name, *doc_name, *custom_tree_buf;
     int itconst;
     int custom_tree_size;
     size_t offs;
	 bool isUNDO = (rollback_active != 0);
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
           doc_schema_node_cptr doc_node = find_document(doc_name);

           if (doc_node->type == document || doc_node->type == virtual_root)
              create_ft_index (lr2PathExpr(NULL, obj_path, pe_catalog_aspace),
                            (ft_index_type)itconst,
                            (doc_schema_node_xptr)doc_node.ptr(),
                            ind_name,
                            doc_name,
                            true,
                            ft_rebuild_cust_tree(custom_tree_buf, custom_tree_size),
							true, ft_ind_dtsearch);
           else throw SYSTEM_EXCEPTION("Can't create index for document");
        }
        else
        {
           col_schema_node_cptr col_node = find_collection(doc_name);
           if (col_node->type == document || col_node->type == virtual_root)
              create_ft_index (lr2PathExpr(NULL, obj_path, pe_catalog_aspace),
                            (ft_index_type)itconst,
                            (doc_schema_node_xptr)col_node.ptr(),
                            ind_name,
                            doc_name,
                            false,
                            ft_rebuild_cust_tree(custom_tree_buf, custom_tree_size),
							true, ft_ind_dtsearch);
           else throw SYSTEM_EXCEPTION("Can't create index for collection");

        }
     }
     else
     {//delete index
          delete_ft_index(ind_name, true);
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
	bool isUNDO = (rollback_active != 0);
	char op = rec[0];

    int tmp;

    size_t offs = sizeof(char) + sizeof(transaction_id);

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
    trigger_action_cell *trac = (trigger_action_cell *)malloc(sizeof(trigger_action_cell));
    rcv_tac = trac;

    while (i < tr_action_size)
    {
        trac->statement = (char *)malloc(strlen(tr_action_buf + i) + 1);
        strcpy(trac->statement, tr_action_buf + i);
        i += strlen(tr_action_buf + i) + 1;

        if (i < tr_action_size)
        	trac->next = (trigger_action_cell *)malloc(sizeof(trigger_action_cell));
        else
        	trac->next = NULL;

        trac = trac->next;
    }

    U_ASSERT(i == tr_action_size);

    if ((isUNDO && (op == LL_DELETE_DOC_TRG || op == LL_DELETE_COL_TRG) ) || (!isUNDO && (op == LL_INSERT_DOC_TRG || op == LL_INSERT_COL_TRG)))
    {//create trigger
        if (op == LL_DELETE_DOC_TRG || op == LL_INSERT_DOC_TRG)
        {// is_doc - true
           schema_node_xptr doc_node = find_document(doc_name);

           if (doc_node->type == document || doc_node->type == virtual_root)
               create_trigger(tr_time, tr_event,
               								(strlen(trigger_path)) ? lr2PathExpr(NULL, trigger_path, pe_catalog_aspace) : NULL,
               								tr_gran,
                                            NULL,
                                            innode,
                                            (strlen(path_to_parent)) ? lr2PathExpr(NULL, path_to_parent, pe_catalog_aspace) : NULL,
                                            (doc_schema_node_xptr )doc_node,
                                            trigger_title, doc_name, true);
           else throw SYSTEM_EXCEPTION("Can't create trigger for document");
   		}
        else
        {
           schema_node_xptr coll_node = find_collection(doc_name);

           if (coll_node->type == document || coll_node->type == virtual_root)
               create_trigger(tr_time, tr_event,
               								(strlen(trigger_path)) ? lr2PathExpr(NULL, trigger_path, pe_catalog_aspace) : NULL,
               								tr_gran,
                                            NULL,
                                            innode,
                                            (strlen(path_to_parent)) ? lr2PathExpr(NULL, path_to_parent, pe_catalog_aspace) : NULL,
                                            (doc_schema_node_xptr) coll_node,
                                            trigger_title, doc_name, false);
           else throw SYSTEM_EXCEPTION("Can't create trigger for collection");
        }
    }
    else // delete trigger
    	delete_trigger(trigger_title);
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
	{LL_RENAME_COLLECTION, llRcvRenameColl},
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

	// rollback transaction by scaning all its records
	llScanRecords(llRcvLogRecsInfo, llRcvLogRecsInfoLen, llInfo->llTransInfoTable[trid].last_lsn, llGetPrevRollbackLsn, NULL);

	rollback_active = false;
}

static void llRcvRedoTrns(trn_cell_analysis_redo *rcv_list, LSN start_lsn)
{
	if (rcv_list == NULL) return;

	// redo transactions by scaning all its records
	llScanRecords(llRcvLogRecsInfo, llRcvLogRecsInfoLen, start_lsn, llGetNextRcvRec, llRcvPrereqRedo);
}

// this function is run from the special recovery process
void llLogicalRecover()
{
	LSN start_analysis_lsn;

	assert(llInfo->checkpoint_lsn != LFS_INVALID_LSN);

	// determine starting lsn to analyze transactions
	if (llInfo->min_rcv_lsn != LFS_INVALID_LSN)
		start_analysis_lsn = llInfo->min_rcv_lsn;
	else if (llInfo->checkpoint_lsn != LFS_INVALID_LSN)
		start_analysis_lsn = llInfo->checkpoint_lsn;
	else
		return;

	// determine transactions that need to be recovered
	rcv_list = llGetRedoList(start_analysis_lsn);

    // get high watermark for redo process
    highRcvLSN = llGetHighRcvLSN(rcv_list);

	//redo committed transactions
	llRcvRedoTrns(rcv_list, start_analysis_lsn);

	RECOVERY_CRASH;

#ifdef SE_ENABLE_DTSEARCH //TODO: make sure FTSEARCH is ok like this
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
    schema_node_cptr scm = XNULL;
    cat_list<ft_index_cell_xptr>::item *sft;

    if (llInfo->hotbackup_needed) return; // since we rebuild ft-indexes on hot-backup anyway

    clear_ft_sequences(); //FIXME: remove this?

    for (it = indir_map.begin(); it != indir_map.end(); ++it)
    {
        new_x = removeIndirection(*it);
        CHECKP(new_x);
        scm = (GETBLOCKBYNODE(new_x))->snode;
        sft = scm->ft_index_list->first;

        while (sft != NULL)
        {
            update_insert_sequence(*it, ft_index_cell_cptr(sft->object));
            update_delete_sequence(it.getKey(), ft_index_cell_cptr(sft->object));
            sft = sft->next;
        }
    }

    execute_modifications();
}
#endif
