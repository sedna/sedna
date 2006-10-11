/*
 * File:  ft_index_data.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_INDEX_DATA_H
#define _FT_INDEX_DATA_H


#include "sedna.h"

#include "usem.h"
#include "nodes.h"
#include "xptr_sequence.h"
#include "XPathOnSchema.h"

class PathExpr;
class QName;
enum ft_index_type
{
	ft_xml,
	ft_xml_ne, //xml without escaping special chars
	ft_xml_hl,
	ft_string_value,
	ft_delimited_value,
	ft_customized_value
};
struct ft_custom_cell
{
	xml_ns* ns;
	char* local;
	ft_index_type cm;
	static ft_custom_cell* init(xml_ns* _ns, const char* _local,ft_index_type _cm,bool persistent=true );
	inline bool less( ft_custom_cell *p1) 
	{
		int val= my_strcmp(this->local,p1->local);
		if (val<0) return true;
		if (val>0) return false;
		return ((int)ns<(int)p1->ns);
	}
	inline bool equals( ft_custom_cell *p1) 
	{
		return (my_strcmp(this->local,p1->local)==0 &&(int)this->ns==(int)p1->ns );
	}
	inline bool less(const void* p1,const void* p2) 
	{
		int val= my_strcmp(local,(char*)p1);
		if (val<0) return true;
		if (val>0) return false;
		return ((int)ns<(int)p2);		
	}
	inline bool equals(const void* p1,const void* p2) 
	{
		return (my_strcmp(this->local,(char*)p1)==0 && (int)this->ns==(int)p2);
	}
};
struct ft_index_cell
{
    index_id id;
    doc_schema_node* schemaroot;
	char * index_title;
	PathExpr *object; 
	index_cell *next, *pred;
	char* doc_name;
	bool is_doc;
	ft_index_type ftype;
	bool fits_to_index(schema_node* snode);
	pers_sset<ft_custom_cell,unsigned short> * custom_tree;
	inline bool less( ft_index_cell *p1) 
	{
		return my_strcmp(this->index_title,p1->index_title)<0;
	}
	inline bool equals( ft_index_cell *p1) 
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
	static ft_index_cell* create_index (PathExpr *object_path,ft_index_type it, doc_schema_node* schemaroot,const char * index_title, const char* doc_name,bool is_doc,std::vector< std::pair< std::pair<xml_ns*,char*>,ft_index_type> >* templ, bool just_heap=false);
	static void delete_index (const char *index_title, bool just_heap=false);
	static void delete_custom_tree (pers_sset<ft_custom_cell,unsigned short> * custom_tree);
	static ft_index_cell* find_index(const char* title);
	void update_index(xptr_sequence* upserted);
	void insert_to_index(xptr_sequence* upserted);
	void delete_from_index(xptr_sequence* deleted);
	void change_index(xptr_sequence* inserted,xptr_sequence* updated,xptr_sequence* deleted);

};

extern pers_sset<ft_index_cell,unsigned short> *ft_indexdata;
extern index_id   *ft_idx_counter;
extern USemaphore ft_index_sem;

//inits metadata library
void ft_index_on_session_begin(pers_sset<ft_index_cell,unsigned short> * _indexdata_, index_id *_idx_counter_);
void ft_index_on_session_end();


void inline ft_index_sem_down()
{
#ifndef NOSEM
	USemaphoreDown(ft_index_sem, __sys_call_error);
#endif
}
void inline ft_index_sem_up()
{
#ifndef NOSEM
	USemaphoreUp(ft_index_sem, __sys_call_error);
#endif
}

#endif
