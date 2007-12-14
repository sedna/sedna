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

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif

using namespace std;

int rcv_number_of_records =0;//for debug
int rcv_number_of_text = 0;

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
#ifdef LOGICAL_LOG
 #ifdef SE_ENABLE_FTSEARCH
  tr_llmgr->recover_db_by_logical_log(SednaIndexJob::recover_db,exec_micro_op, switch_to_rollback_mode, vmm_rcv_add_to_indir_block_set,vmm_rcv_clear_indir_block_set, sync_indirection_table, last_cp_lsn,  MODE_UNDO, MODE_REDO, true);  
#else
  tr_llmgr->recover_db_by_logical_log(exec_micro_op, switch_to_rollback_mode, vmm_rcv_add_to_indir_block_set,vmm_rcv_clear_indir_block_set, sync_indirection_table, last_cp_lsn,  MODE_UNDO, MODE_REDO, true);	
#endif
  string str = string("recover+db_by_logical_log finished\n");
  WRITE_DEBUG_LOG(str.c_str());


#endif
}


void exec_micro_op(const char* rec, int len, bool isUNDO)
{
try{
  if (len < 1 )
	 throw SYSTEM_EXCEPTION("bad logical record given from logical log");

  char op = rec[0];

  if(op == LL_INSERT_ELEM || op == LL_DELETE_ELEM || op == LL_INDIR_INSERT_ELEM || op == LL_INDIR_DELETE_ELEM)
  {
     const char* name, *uri, *prefix;
     xmlscm_type type;
     xptr self, left, right, parent;
     int offs;
     std::vector<xptr> indir_blocks;
     int cl_hint;


     offs = sizeof(char) + sizeof(transaction_id);

     if (op == LL_INDIR_INSERT_ELEM || op == LL_INDIR_DELETE_ELEM)
     {
        int blocks_num;

        //cl_hint = (int*)(rec + offs);
        memcpy(&cl_hint, rec + offs, sizeof(int));
        offs += sizeof(int);
        //blocks_num = (int*)(rec + offs);
        memcpy(&blocks_num, rec + offs, sizeof(int));
        offs += sizeof(int);

        for (int i=0; i< blocks_num; i++)
        {        
            xptr tmp_xptr;
            memcpy(&tmp_xptr, rec + offs, sizeof(xptr));
            indir_blocks.push_back(tmp_xptr);            
            //indir_blocks.push_back(*((xptr*)(rec + offs)));
            offs += sizeof(xptr);
        }
     }

     name = rec + offs;
     offs+= strlen(name) + 1;  
     uri = rec +offs;
     offs += strlen(uri) + 1;
     prefix = rec + offs;
     offs += strlen(prefix) + 1;
     //type = (xmlscm_type*)(rec + offs);
     memcpy(&type, rec + offs, sizeof(xmlscm_type));
     offs += sizeof(xmlscm_type);
     //self = (xptr*)(rec + offs);
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //left = (xptr*)(rec + offs);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //right = (xptr*)(rec + offs);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //parent = (xptr*)(rec + offs);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if( (isUNDO && (op == LL_DELETE_ELEM || op == LL_INDIR_DELETE_ELEM)) || (!isUNDO && (op == LL_INSERT_ELEM || op == LL_INDIR_INSERT_ELEM)))
     {
       if (op == LL_INDIR_INSERT_ELEM)
          set_redo_hint(cl_hint, &indir_blocks);

       set_rollback_record(self);
       insert_element(removeIndirection(left),
                      removeIndirection(right),
                      removeIndirection(parent),
                      name,
                      type,
                      (strlen(uri) != 0) ? uri: NULL,
                      (strlen(prefix) != 0) ? prefix: NULL);
     }
     else
     {
       if ( op == LL_INDIR_DELETE_ELEM)
          set_redo_hint(cl_hint, &indir_blocks);

       delete_node(removeIndirection(self));
     } 
  }
  else
  if(op == LL_INSERT_ATTR || op == LL_DELETE_ATTR || op == LL_INDIR_INSERT_ATTR || op == LL_INDIR_DELETE_ATTR)
  {
     const char* name, *uri, *prefix, *value;
     int value_size;
     xmlscm_type type;
     xptr self, left, right, parent;
     int offs;
     std::vector<xptr> indir_blocks;
     int cl_hint;


     offs = sizeof(char) + sizeof(transaction_id);

     if (op == LL_INDIR_INSERT_ATTR || op == LL_INDIR_DELETE_ATTR)
     {
        int blocks_num;

        //cl_hint = (int*)(rec + offs);
        memcpy(&cl_hint, rec + offs, sizeof(int));
        offs += sizeof(int);
        //blocks_num = (int*)(rec + offs);
        memcpy(&blocks_num, rec + offs, sizeof(int));
        offs += sizeof(int);

        for (int i=0; i< blocks_num; i++)
        {        
            xptr tmp_xptr;
            memcpy(&tmp_xptr, rec + offs, sizeof(xptr));
            indir_blocks.push_back(tmp_xptr);            
            //indir_blocks.push_back(*((xptr*)(rec + offs)));

            offs += sizeof(xptr);
        }
     }


     name = rec + offs;
     offs += strlen(name) + 1;
     uri = rec + offs;
     offs += strlen(uri) + 1;
     prefix = rec + offs;
     offs += strlen(prefix) + 1;
     //value_size = (int*)(rec + offs);
     memcpy(&value_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     value = rec + offs;
     offs += value_size;
     //type = (xmlscm_type*)(rec + offs);
     memcpy(&type, rec + offs, sizeof(xmlscm_type));
     offs += sizeof(xmlscm_type);
     //self = (xptr*)(rec + offs);
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //left = (xptr*)(rec + offs);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //right = (xptr*)(rec + offs);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //parent = (xptr*)(rec + offs);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if ( (isUNDO && (op == LL_DELETE_ATTR || op == LL_INDIR_DELETE_ATTR)) || (!isUNDO && (op == LL_INSERT_ATTR || op == LL_INDIR_INSERT_ATTR))) 
     {

       if (op == LL_INDIR_INSERT_ATTR)
          set_redo_hint(cl_hint, &indir_blocks);

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
     }
     else
     {
       if (op == LL_INDIR_DELETE_ATTR)
          set_redo_hint(cl_hint, &indir_blocks);

       delete_node(removeIndirection(self));
     }  
  }
  else
  if ( op == LL_INSERT_TEXT || op == LL_DELETE_TEXT || op == LL_INDIR_INSERT_TEXT || op == LL_INDIR_DELETE_TEXT )
  {
     const char* value;
     int value_size;
     xptr self, left, right, parent;
     int offs;
     std::vector<xptr> indir_blocks;
     int cl_hint;


     offs = sizeof(char) + sizeof(transaction_id);

     if (op == LL_INDIR_INSERT_TEXT || op == LL_INDIR_DELETE_TEXT)
     {
        int blocks_num;

        //cl_hint = (int*)(rec + offs);
        memcpy(&cl_hint, rec + offs, sizeof(int));
        offs += sizeof(int);
        //blocks_num = (int*)(rec + offs);
        memcpy(&blocks_num, rec + offs, sizeof(int));
        offs += sizeof(int);

        for (int i=0; i< blocks_num; i++)
        {        
            xptr tmp_xptr;
            memcpy(&tmp_xptr, rec + offs, sizeof(xptr));
            indir_blocks.push_back(tmp_xptr);            

//          indir_blocks.push_back(*((xptr*)(rec + offs)));
            offs += sizeof(xptr);
        }
     }


     //value_size = (int*)(rec + offs);
     memcpy(&value_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     value = rec + offs;
     offs += value_size;
     //self = (xptr*)(rec + offs);
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //left = (xptr*)(rec + offs);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //right = (xptr*)(rec + offs);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //parent = (xptr*)(rec + offs);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if ((isUNDO && (op == LL_DELETE_TEXT || op == LL_INDIR_DELETE_TEXT)) || (!isUNDO && (op == LL_INSERT_TEXT || op == LL_INDIR_INSERT_TEXT)))
     {
       if (op == LL_INDIR_INSERT_TEXT)
          set_redo_hint(cl_hint, &indir_blocks);

       set_rollback_record(self);
       insert_text(removeIndirection(left),
                   removeIndirection(right),
                   removeIndirection(parent),
                   value,
                   value_size);
     }
     else
     {
       if (op == LL_INDIR_DELETE_TEXT)
          set_redo_hint(cl_hint, &indir_blocks);

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

     //value_size = (int*)(rec + offs);
     memcpy(&value_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     value = rec + offs;
     offs += value_size;
     //self = (xptr*)(rec + offs);
     memcpy(&self, rec + offs, sizeof(xptr));

     if (   (isUNDO && (op == LL_DELETE_LEFT_TEXT || op == LL_DELETE_RIGHT_TEXT)) || (!isUNDO && (op == LL_INSERT_LEFT_TEXT || op == LL_INSERT_RIGHT_TEXT)))
     {//insert case
       if (op == LL_INSERT_RIGHT_TEXT || op == LL_DELETE_RIGHT_TEXT)
           appendTextValue(removeIndirection(self), value, value_size,text_mem);
       else
           insertTextValue(removeIndirection(self), value, value_size,text_mem);
     }
     else 
     {//delete case
       if (op == LL_INSERT_RIGHT_TEXT || op == LL_DELETE_RIGHT_TEXT)
          delete_text_tail(removeIndirection(self), value_size);
       else
          delete_text_head(removeIndirection(self), value_size);
     }
        
  }
  else
  if (op == LL_INSERT_DOC || op == LL_DELETE_DOC || op == LL_INDIR_INSERT_DOC || op == LL_INDIR_DELETE_DOC)
  {
    const char* name, *collection;
    xptr self;
    int offs;
    std::vector<xptr> indir_blocks;
    int cl_hint;


    offs = sizeof(char) + sizeof(transaction_id);


     if (op == LL_INDIR_INSERT_DOC || op == LL_INDIR_DELETE_DOC)
     {
        int blocks_num;

        //cl_hint = (int*)(rec + offs);
        memcpy(&cl_hint, rec + offs, sizeof(int));
        offs += sizeof(int);
        //blocks_num = (int*)(rec + offs);
        memcpy(&blocks_num, rec + offs, sizeof(int));
        offs += sizeof(int);

        for (int i=0; i< blocks_num; i++)
        {     
            xptr tmp_xptr;
            memcpy(&tmp_xptr, rec + offs, sizeof(xptr));
            indir_blocks.push_back(tmp_xptr);            

            //indir_blocks.push_back(*((xptr*)(rec + offs)));
            offs += sizeof(xptr);
        }
     }


    name = rec + offs;
    offs += strlen(name) + 1;
    collection = rec + offs;
    offs += strlen(collection) + 1;
    //self = (xptr*)(rec + offs);
    memcpy(&self, rec + offs, sizeof(xptr));

    if ((isUNDO && (op == LL_DELETE_DOC || op == LL_INDIR_DELETE_DOC)) || (!isUNDO && (op == LL_INSERT_DOC || op == LL_INDIR_INSERT_DOC)))
    {
       if (op == LL_INDIR_INSERT_DOC)
           set_redo_hint(cl_hint, &indir_blocks);

       if(strlen(collection) == 0)
	   {
          set_rollback_record(self);
          insert_document(name);
	   }
       else
       {
		  set_rollback_record(self);
          insert_document_in_collection(collection, name);    
       }
    }   
    else
    {
       if (op == LL_INDIR_DELETE_DOC)
          set_redo_hint(cl_hint, &indir_blocks);

       if(strlen(collection) == 0)
       {
          delete_document(name);
       }
       else
          delete_document(collection, name);
    }
    

  }
  else
  if (op == LL_INSERT_COMMENT || op == LL_DELETE_COMMENT || op == LL_INDIR_INSERT_COMMENT || op == LL_INDIR_DELETE_COMMENT)
  {
     const char* value;
     int value_size;
     xptr self, left, right, parent;
     int offs;
     std::vector<xptr> indir_blocks;
     int cl_hint;


     offs = sizeof(char) + sizeof(transaction_id);

     if (op == LL_INDIR_INSERT_COMMENT || op == LL_INDIR_DELETE_COMMENT)
     {
        int blocks_num;

        //cl_hint = (int*)(rec + offs);
        memcpy(&cl_hint, rec + offs, sizeof(int));
        offs += sizeof(int);
        //blocks_num = (int*)(rec + offs);
        memcpy(&blocks_num, rec + offs, sizeof(int));
        offs += sizeof(int);

        for (int i=0; i< blocks_num; i++)
        {        
            xptr tmp_xptr;
            memcpy(&tmp_xptr, rec + offs, sizeof(xptr));
            indir_blocks.push_back(tmp_xptr);            

            //indir_blocks.push_back(*((xptr*)(rec + offs)));
            offs += sizeof(xptr);
        }
     }


     //value_size = (int*)(rec + offs);
     memcpy(&value_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     value = rec + offs;
     offs += value_size;
     //self = (xptr*)(rec + offs);
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //left = (xptr*)(rec + offs);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //right = (xptr*)(rec + offs);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //parent = (xptr*)(rec + offs);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if((isUNDO && (op == LL_DELETE_COMMENT || op == LL_INDIR_DELETE_COMMENT)) || (!isUNDO && (op == LL_INSERT_COMMENT || op == LL_INDIR_INSERT_COMMENT)))
     {
       if (op == LL_INDIR_INSERT_COMMENT)
           set_redo_hint(cl_hint, &indir_blocks);

       set_rollback_record(self);
       insert_comment(removeIndirection(left),
                      removeIndirection(right),
                      removeIndirection(parent),
                      value,
                      value_size);

     }
     else
     {
       if (op == LL_INDIR_DELETE_COMMENT)
          set_redo_hint(cl_hint, &indir_blocks);

       delete_node(removeIndirection(self));
     }
  }
  else
  if(op == LL_INSERT_PI || op == LL_DELETE_PI || op == LL_INDIR_INSERT_PI || op == LL_INDIR_DELETE_PI)
  {
     const char* value;
     int total_size;
     shft target_size;
     xptr self, left, right, parent;
     int offs;
     std::vector<xptr> indir_blocks;
     int cl_hint;


     offs = sizeof(char) + sizeof(transaction_id);

     if (op == LL_INDIR_INSERT_PI || op == LL_INDIR_DELETE_PI)
     {
        int blocks_num;

        //cl_hint = (int*)(rec + offs);
        memcpy(&cl_hint, rec + offs, sizeof(int));
        offs += sizeof(int);
        //blocks_num = (int*)(rec + offs);
        memcpy(&blocks_num, rec + offs, sizeof(int));
        offs += sizeof(int);

        for (int i=0; i< blocks_num; i++)
        {        
            xptr tmp_xptr;
            memcpy(&tmp_xptr, rec + offs, sizeof(xptr));
            indir_blocks.push_back(tmp_xptr);            

            //indir_blocks.push_back(*((xptr*)(rec + offs)));
            offs += sizeof(xptr);
        }
     }


     //total_size = (int*)(rec + offs);
     memcpy(&total_size, rec + offs, sizeof(int));
     offs += sizeof(int);
     //target_size = (shft*)(rec + offs);
     memcpy(&target_size, rec + offs, sizeof(shft));
     offs += sizeof(shft);
     value = rec + offs;
     offs += total_size;
     //self = (xptr*)(rec + offs);
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //left = (xptr*)(rec + offs);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //right = (xptr*)(rec + offs);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //parent = (xptr*)(rec + offs);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if ((isUNDO && (op == LL_DELETE_PI || op == LL_INDIR_DELETE_PI)) || (!isUNDO && (op == LL_INSERT_PI || op == LL_INDIR_INSERT_PI)))
     {
       if (op == LL_INDIR_INSERT_PI)
           set_redo_hint(cl_hint, &indir_blocks);


       set_rollback_record(self);
       insert_pi(removeIndirection(left),
                 removeIndirection(right),
                 removeIndirection(parent),
                 value,
                 target_size,
                 value+(target_size),
                (total_size)-(target_size));
     }
     else
     {
       if (op == LL_INDIR_DELETE_PI)
          set_redo_hint(cl_hint, &indir_blocks);

       delete_node(removeIndirection(self));
     }
     
  }
  else
  if(op == LL_INSERT_COLLECTION || op == LL_DELETE_COLLECTION || op == LL_INDIR_INSERT_COLLECTION || op == LL_INDIR_DELETE_COLLECTION)
  {
     const char* name;
     std::vector<xptr> indir_blocks;
     int cl_hint;
     int offs;

     offs = sizeof(char) + sizeof(transaction_id);

     if (op == LL_INDIR_INSERT_COLLECTION || op == LL_INDIR_DELETE_COLLECTION)
     {
        int blocks_num;

        //cl_hint = (int*)(rec + offs);
        memcpy(&cl_hint, rec + offs, sizeof(int));
        offs += sizeof(int);
        //blocks_num = (int*)(rec + offs);
        memcpy(&blocks_num, rec + offs, sizeof(int));
        offs += sizeof(int);

        for (int i=0; i< blocks_num; i++)
        {        
            xptr tmp_xptr;
            memcpy(&tmp_xptr, rec + offs, sizeof(xptr));
            indir_blocks.push_back(tmp_xptr);            

            //indir_blocks.push_back(*((xptr*)(rec + offs)));
            offs += sizeof(xptr);
        }
     }


     name = rec + offs;

     if ((isUNDO && (op == LL_DELETE_COLLECTION || op == LL_INDIR_DELETE_COLLECTION)) || (!isUNDO && (op == LL_INSERT_COLLECTION || op == LL_INDIR_INSERT_COLLECTION)))
     {
       if (op == LL_INDIR_INSERT_COLLECTION)
           set_redo_hint(cl_hint, &indir_blocks);

        insert_collection(name);
     }
     else
     {
       if (op == LL_INDIR_DELETE_COLLECTION)
          set_redo_hint(cl_hint, &indir_blocks);

        delete_collection(name);
     }
  }
  else
  if(op == LL_INSERT_NS || op == LL_DELETE_NS || op == LL_INDIR_INSERT_NS || op == LL_INDIR_DELETE_NS)
  {
     const char *uri, *prefix;
     xptr self, left, right, parent;
     int offs;
     std::vector<xptr> indir_blocks;
     int cl_hint;


     offs = sizeof(char) + sizeof(transaction_id);

     if (op == LL_INDIR_INSERT_NS || op == LL_INDIR_DELETE_NS)
     {
        int blocks_num;

        //cl_hint = (int*)(rec + offs);
        memcpy(&cl_hint, rec + offs, sizeof(int));
        offs += sizeof(int);
        //blocks_num = (int*)(rec + offs);
        memcpy(&blocks_num, rec + offs, sizeof(int));
        offs += sizeof(int);

        for (int i=0; i< blocks_num; i++)
        {        
            xptr tmp_xptr;
            memcpy(&tmp_xptr, rec + offs, sizeof(xptr));
            indir_blocks.push_back(tmp_xptr);            

            //indir_blocks.push_back(*((xptr*)(rec + offs)));
            offs += sizeof(xptr);
        }
     }



     uri = rec + offs;
     offs += strlen(uri) + 1;
     prefix = rec + offs;
     offs += strlen(prefix) + 1;
     //self = (xptr*)(rec + offs);
     memcpy(&self, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //left = (xptr*)(rec + offs);
     memcpy(&left, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //right = (xptr*)(rec + offs);
     memcpy(&right, rec + offs, sizeof(xptr));
     offs += sizeof(xptr);
     //parent = (xptr*)(rec + offs);
     memcpy(&parent, rec + offs, sizeof(xptr));

     if((isUNDO && (op == LL_DELETE_NS || op == LL_INDIR_DELETE_NS) ) || (!isUNDO && (op == LL_INSERT_NS || op == LL_INDIR_INSERT_NS)))
     {

       if (op == LL_INDIR_INSERT_NS)
           set_redo_hint(cl_hint, &indir_blocks);

       set_rollback_record(self);
       insert_namespace(removeIndirection(left),
                        removeIndirection(right),
                        removeIndirection(parent),
                        uri,
                        prefix);

     }
     else
     {
       if (op == LL_INDIR_DELETE_NS)
          set_redo_hint(cl_hint, &indir_blocks);

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
     //key_type = *((xmlscm_type*)(rec + offs));
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

} catch (...) {
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

void rcv_allocate_blocks(const vector<xptr> &arr)
{
/*
    set<xptr> s;
    set<xptr>::iterator s_it;
    vector<xptr> d;
    int i = 0;
    xptr cur;


    for (i = 0; i < arr.size(); i++)
        s.insert(arr[i]);

    while (!s.empty())
    {
        vmm_pseudo_alloc_data_block(&cur);
        s_it = s.find(cur);
        if (s_it == s.end())
            d.push_back(cur);
        else
		{
			//d_printf1("Block allocated: ");
			s_it->print();
            s.erase(s_it);
		}
    }

    for (i = 0; i < d.size(); i++)
        vmm_pseudo_delete_block(d[i]);
*/
}
