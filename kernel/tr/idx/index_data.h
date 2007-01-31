/*
 * File:  index_data.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _INDEX_DATA_H
#define _INDEX_DATA_H

#include "common/sedna.h"

#include "common/u/usem.h"
#include "tr/structures/nodes.h"
#include "tr/executor/base/XPathOnSchema.h"

class PathExpr;

struct index_cell
{
    index_id id;
    xmlscm_type keytype;
	xptr btree_root;
	doc_schema_node* schemaroot;
	char * index_title;
	PathExpr *object; 
	PathExpr *key;
	int err_cntr;
	index_cell *next, *pred;
	char* doc_name;
	bool is_doc;
	t_scmnodes fits_to_index_as_key(schema_node* snode);
	void put_to_index(xptr node, schema_node* accessor);
	void put_to_index(xptr node,const char* value, int size, schema_node* accessor);
	void delete_from_index(xptr node, schema_node* accessor);
	void delete_from_index(xptr node,const char* value, int size, schema_node* accessor);
	inline bool less( index_cell *p1) 
	{
		return my_strcmp(this->index_title,p1->index_title)<0;
	}
	inline bool equals( index_cell *p1) 
	{
		return my_strcmp(this->index_title,p1->index_title)==0;
	}
	inline bool less(const void* p1,const void* p2) 
	{
		return my_strcmp(this->index_title,(char*)p1)<0;
	}
	inline bool equals(const void* p1,const void* p2) 
	{
		return my_strcmp(this->index_title,(char*)p1)==0;
	}
};

extern pers_sset<index_cell,unsigned short> *indexdata;
extern index_id   *idx_counter;
extern USemaphore index_sem;

//inits metadata library
void index_on_session_begin(pers_sset<index_cell,unsigned short> * _indexdata_, index_id *_idx_counter_);
void index_on_session_end();


void inline index_sem_down()
{
#ifndef NOSEM
	USemaphoreDown(index_sem, __sys_call_error);
#endif
}
void inline index_sem_up()
{
#ifndef NOSEM
	USemaphoreUp(index_sem, __sys_call_error);
#endif
}

#endif
