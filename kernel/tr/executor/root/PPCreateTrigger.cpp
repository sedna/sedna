/*
 * File:  PPCreateTrigger.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPCreateTrigger.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/PPBase.h"
#include "tr/locks/locks.h"

using namespace std;

trigger_event symb2trigger_event(const char* str)
{
    string event = string(str);
	if (event == "INSERT")
		return  TRIGGER_INSERT_EVENT;
	else if (event == "DELETE")
		return TRIGGER_DELETE_EVENT;
	else if (event == "REPLACE")
		return TRIGGER_REPLACE_EVENT;
	else
		throw USER_EXCEPTION2(SE1071, "unknown trigger event");
}

trigger_time symb2trigger_time(const char* str)
{
    string time = string(str);
	if (time == "BEFORE")
		return  TRIGGER_BEFORE;
	else if (time == "AFTER")
		return TRIGGER_AFTER;
	else
		throw USER_EXCEPTION2(SE1071, "unknown trigger time");
}

trigger_granularity symb2trigger_granularity(const char* str)
{
    string granularity = string(str);
	if (granularity == "NODE")
		return  TRIGGER_FOR_EACH_NODE;
	else if (granularity == "STATEMENT")
		return TRIGGER_FOR_EACH_STATEMENT;
	else
		throw USER_EXCEPTION2(SE1071, "unknown trigger granularity");
}
    
t_item int2t_item(int type)
{
    if(type == 0)
        return element;
    else if(type == 1)
        return attribute;
    else 
		throw USER_EXCEPTION2(SE1071, "unknown trigger inserting type parameter value");
}

PPCreateTrigger::PPCreateTrigger(char* _time_,
                                 char* _event_,
                                 counted_ptr<db_entity> _db_ent_,
                                 PathExpr *_trigger_path_,
                                 char* _granularity_,
                                 scheme_list* _action_,
                                 PPOpIn _trigger_name_,
    						 	 dynamic_context *_cxt_) :	trigger_path(_trigger_path_),
                                                            db_ent(_db_ent_),
                                                            trigger_name(_trigger_name_),
    														action(_action_),
    														cxt(_cxt_)
{
	time   = symb2trigger_time(_time_);
    event  = symb2trigger_event(_event_);
    gran   = symb2trigger_granularity(_granularity_);
    path_to_parent = NULL;
}

PPCreateTrigger::PPCreateTrigger(char* _time_,
                                 char* _event_,
                                 counted_ptr<db_entity> _db_ent_,
                                 PathExpr *_trigger_path_,
                                 char* _granularity_,
                                 scheme_list* _action_,
							     char* _inserting_name_,
 			   					 int _inserting_type_,
			                     PathExpr *_path_to_parent_,
                                 PPOpIn _trigger_name_,
    							 dynamic_context *_cxt_) :	trigger_path(_trigger_path_),
                                                            db_ent(_db_ent_),
                                                            trigger_name(_trigger_name_),
    														path_to_parent(_path_to_parent_),
       														action(_action_),
    														cxt(_cxt_)
{
	time   = symb2trigger_time(_time_);
    event  = symb2trigger_event(_event_);
    gran   = symb2trigger_granularity(_granularity_);
	innode.name = _inserting_name_;
    innode.type = int2t_item(_inserting_type_);
}

PPCreateTrigger::~PPCreateTrigger()
{
    delete trigger_name.op;
    trigger_name.op = NULL;
    
    delete cxt;
    cxt = NULL;
}

void PPCreateTrigger::open()
{
    root = get_schema_node(db_ent, "Unknown entity passed to PPCreateTrigger");
    trigger_name.op->open();
    dynamic_context::global_variables_open();
}

void PPCreateTrigger::close()
{
    trigger_name.op->close();
    root = NULL;
    dynamic_context::global_variables_close();
}


void PPCreateTrigger::execute()
{
    tuple_cell tc;
    tuple t(1);
    trigger_name.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = trigger_name.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    trigger_name.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
        
    tc = tuple_cell::make_sure_light_atomic(tc);

    local_lock_mrg->put_lock_on_trigger(tc.get_str_mem());

	trigger_cell* trc = trigger_cell::create_trigger (time,
				event,
        		trigger_path,
        		gran,
        		action,
        		innode,
        		path_to_parent,
        		(doc_schema_node*)root,
				tc.get_str_mem(),
				db_ent->name,
				(db_ent->type == dbe_document));
}
