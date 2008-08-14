/*
 * File:  metadata.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _META_DATA_H
#define _META_DATA_H

#include <string>

#include "common/sedna.h"

#include "common/xptr.h"
#include "tr/structures/schema.h"
#include "common/u/usem.h"
#include "tr/structures/pers_map.h"

#define METADATA_NAME_SIZE			256
//#define NOSEM 

struct metadata_cell
{
    char* document_name;
};

struct sn_metadata_cell: public metadata_cell
{	
	char* collection_name;
	schema_node *snode;
	inline bool less( sn_metadata_cell *p1) 
	{
		return my_strcmp(this->collection_name,p1->collection_name)<0 ||
			(my_strcmp(this->collection_name,p1->collection_name)==0 && my_strcmp(this->document_name,p1->document_name)<0);
	}
	inline bool equals( sn_metadata_cell *p1) 
	{
		return my_strcmp(this->collection_name,p1->collection_name)==0 &&
			my_strcmp(this->document_name,p1->document_name)==0;
	}
	inline bool less(const void* p1,const void* p2) 
	{
		return my_strcmp(this->collection_name,(char*)p1)<0 ||
			(my_strcmp(this->collection_name,(char*)p1)==0 && my_strcmp(this->document_name,(char*)p2)<0);
	}
	inline bool equals(const void* p1,const void* p2) 
	{
		return my_strcmp(this->collection_name,(char*)p1)==0 
			&& my_strcmp(this->document_name,(char*)p2)==0;
	}
};


extern pers_sset<sn_metadata_cell,unsigned short> *metadata;
extern USemaphore metadata_sem;

//inits metadata library
void metadata_on_session_begin(pers_sset<sn_metadata_cell,unsigned short> *mdc);
void metadata_on_session_end();


void delete_document( const char *document_name);
void delete_document(const char *collection_name,const char *document_name);
void delete_collection(const char *collection_name);
xptr insert_document(const char *uri,bool persistent=true);
schema_node *insert_collection(const char *collection_name);
xptr insert_document_in_collection(const char *collection_name, const char *uri);

schema_node *find_collection(const char *collection_name);
xptr find_document(const char *collection_name,const char *document_name);
schema_node *find_document(const char *document_name);

void rename_collection(const char *old_collection_name,const char *new_collection_name);

void inline metadata_sem_down()
{
#ifndef NOSEM
	USemaphoreDown(metadata_sem, __sys_call_error);
#endif
}
void inline metadata_sem_up()
{
#ifndef NOSEM
	USemaphoreUp(metadata_sem, __sys_call_error);
#endif
}

//UNREALIZED
std::string get_name_from_uri(const char* uri);
#endif

