/*
 * File:  triggers_data.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TRIGGERS_DATA_H
#define _TRIGGERS_DATA_H

#include <vector>
#include "usem.h"
#include "xptr.h"
#include "XPathOnSchema.h"
#include "PPBase.h"

#define TRIGGER_MAX_CASCADE_LEVEL 5

class PPXptr;    
class PathExpr;

enum trigger_event
{
	TRIGGER_INSERT_EVENT,
	TRIGGER_DELETE_EVENT,
	TRIGGER_REPLACE_EVENT
};
enum trigger_time
{
	TRIGGER_BEFORE,
	TRIGGER_AFTER
};
enum trigger_granularity
{
	TRIGGER_FOR_EACH_NODE,
	TRIGGER_FOR_EACH_STATEMENT
};
enum trigger_parameter_type
{
    TRIGGER_PARAMETER_NEW,
    TRIGGER_PARAMETER_OLD,
    TRIGGER_PARAMETER_WHERE
};
struct inserting_node
{
    char* name;
    t_item type;
};
struct trigger_action_cell
{
    char* statement;
    int cxt_size;
    trigger_action_cell* next;
};

typedef std::vector<PPXptr*> qep_parameters_vec;

struct trigger_cell
{
	xptr btree_root;
	doc_schema_node* schemaroot;
	char * trigger_title;
    enum trigger_event trigger_event;
    enum trigger_time trigger_time;
    enum trigger_granularity trigger_granularity;
    trigger_action_cell* trigger_action;
	PathExpr *trigger_path; 
	PathExpr *path_to_parent;
    inserting_node innode;
	int err_cntr;
	trigger_cell *next, *pred;
	char* doc_name;
	bool is_doc;
	bool fits_to_trigger(schema_node* snode);
   	bool fits_to_trigger_path_to_parent(schema_node* parent);
	inline bool less( trigger_cell *p1) 
	{
		return my_strcmp(this->trigger_title,p1->trigger_title)<0;
	}
	inline bool equals( trigger_cell *p1) 
	{
		return my_strcmp(this->trigger_title,p1->trigger_title)==0;
	}
	inline bool less(const void* p1,const void* p2) 
	{
		return my_strcmp(this->trigger_title,(char*)p1)<0;
	}
	inline bool equals(const void* p1,const void* p2) 
	{
		return my_strcmp(this->trigger_title,(char*)p1)==0;
	}
	static trigger_cell* create_trigger(enum trigger_time time, enum trigger_event event, PathExpr *trigger_path, enum trigger_granularity gran, scheme_list* action, inserting_node innode, PathExpr *path_to_parent, doc_schema_node* schemaroot,const char * trigger_title, const char* doc_name,bool is_doc);
	static void delete_trigger (const char *trigger_title);
	static trigger_cell* find_trigger(const char* trigger_title);
    xptr execute_trigger_action(xptr parameter_new, xptr parameter_old, xptr parameter_where);
};

extern pers_sset<trigger_cell,unsigned short> *triggerdata;
extern USemaphore trigger_sem;
extern qep_parameters_vec* qep_parameters;

//inits metadata library
void triggers_on_session_begin(pers_sset<trigger_cell,unsigned short> * _triggerdata_);
void triggers_on_session_end();

void triggers_on_statement_begin();
void triggers_on_statement_end();
    
void inline trigger_sem_down()
{
#ifndef NOSEM
	USemaphoreDown(trigger_sem, __sys_call_error);
#endif
}
void inline trigger_sem_up()
{
#ifndef NOSEM
	USemaphoreUp(trigger_sem, __sys_call_error);
#endif
}

#endif
