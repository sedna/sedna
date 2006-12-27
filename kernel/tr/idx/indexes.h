/*
 * File:  indexes.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __INDEXES_H
#define __INDEXES_H

#include "sedna.h"

#include "index_data.h"
#include "XPath.h"
#include "tuple.h"
#include "btree.h"
#include "PPBase.h"

/* creates bt_key (fills key argument) from tuple_cell */
void tuple_cell2bt_key(const tuple_cell& /*in*/ tc, bt_key& /*out*/ key);

/* creates tuple_cell (fills tc argument) from bt_key */
//void bt_key2tuple_cell(const bt_key& /*in*/ key, tuple_cell& /*out*/ tc);

/* the creation of index with the following paths in data and with the following key_type connected to the xml document or xml collection identified by the descriptive schema root node*/
index_cell* create_index (PathExpr *object_path, 
                          PathExpr *key_path, 
                          xmlscm_type key_type, 
                          doc_schema_node* schemaroot,
                          const char * index_title, 
                          const char* doc_name,
                          bool is_doc);

void delete_index (const char *index_title);

/* sets the binding between the input schema_node and indexes */
//void set_schema_node_binding(schema_node* schema);

/* inserts the following node modified to key together with some objects to the all indexes needed */
//void insert_node_to_indexes (xptr node);

/* deletes the following node modified to key  together with some objects from all the indexes needed */
//void delete_node_from_indexes (xptr node);

xptr find_btree(index_id id);
counted_ptr<db_entity> find_entity(const char* title);
xptr find_btree(const char* title);

xmlscm_type get_index_xmlscm_type(const char* title);


#endif

