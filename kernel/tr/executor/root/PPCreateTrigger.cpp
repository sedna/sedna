/*
 * File:  PPCreateTrigger.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"

#include "tr/executor/root/PPCreateTrigger.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/locks/locks.h"
#include "tr/triggers/triggers.h"
#include "tr/auth/auc.h"

using namespace std;

///////////////////////////////////////////////////////////////////////////////
/// Local argument converteds
/// Do we need them actually?
///////////////////////////////////////////////////////////////////////////////

static trigger_event
symb2trigger_event(const char* str)
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

static trigger_time
symb2trigger_time(const char* str)
{
    string time = string(str);
	if (time == "BEFORE")
		return  TRIGGER_BEFORE;
	else if (time == "AFTER")
		return TRIGGER_AFTER;
	else
		throw USER_EXCEPTION2(SE1071, "unknown trigger time");
}

static trigger_granularity
symb2trigger_granularity(const char* str)
{
    string granularity = string(str);
	if (granularity == "NODE")
		return  TRIGGER_FOR_EACH_NODE;
	else if (granularity == "STATEMENT")
		return TRIGGER_FOR_EACH_STATEMENT;
	else
		throw USER_EXCEPTION2(SE1071, "unknown trigger granularity");
}

static t_item
int2t_item(int type)
{
    if(type == 0)
        return element;
    else if(type == 1)
        return attribute;
    else
		throw USER_EXCEPTION2(SE1071, "unknown trigger inserting type parameter value");
}

///////////////////////////////////////////////////////////////////////////////
/// Create trigget root operation
///////////////////////////////////////////////////////////////////////////////


PPCreateTrigger::PPCreateTrigger(const char* _time_,
                                 const char* _event_,
                                 PathExprRoot _root_,
                                 PathExpr *_trigger_path_,
                                 const char* _granularity_,
                                 scheme_list* _action_,
                                 PPOpIn _trigger_name_,
    						 	 dynamic_context *_cxt_) :	trigger_path(_trigger_path_),
                                                            root(_root_),
                                                            action(_action_),
                                                            trigger_name(_trigger_name_),
    														cxt(_cxt_)
{
    time   = symb2trigger_time(_time_);
    event  = symb2trigger_event(_event_);
    gran   = symb2trigger_granularity(_granularity_);
    innode.name = NULL;
    path_to_parent = NULL;
}

PPCreateTrigger::PPCreateTrigger(const char* _time_,
                                 const char* _event_,
                                 PathExprRoot _root_,
                                 PathExpr *_trigger_path_,
                                 const char* _granularity_,
                                 scheme_list* _action_,
							     const char* _inserting_name_,
 			   					 int _inserting_type_,
			                     PathExpr *_path_to_parent_,
                                 PPOpIn _trigger_name_,
    							 dynamic_context *_cxt_) :	trigger_path(_trigger_path_),
                                                            root(_root_),
                                                            action(_action_),
                                                            path_to_parent(_path_to_parent_),
                                                            trigger_name(_trigger_name_),
    														cxt(_cxt_)
{
    time   = symb2trigger_time(_time_);
    event  = symb2trigger_event(_event_);
    gran   = symb2trigger_granularity(_granularity_);
    innode.name = (char *)malloc(strlen(_inserting_name_) + 1);
    if (!innode.name)
        throw SYSTEM_EXCEPTION("out of memory!");
    strcpy(innode.name, _inserting_name_);
    innode.type = int2t_item(_inserting_type_);
}

PPCreateTrigger::~PPCreateTrigger()
{
    delete trigger_name.op;
    trigger_name.op = NULL;

    delete cxt;
    cxt = NULL;

    delete_scheme_list(action);
    action = NULL;

    free(innode.name);

    root.release();
}

void PPCreateTrigger::open()
{
    trigger_name.op->open();
    root.open();
    dynamic_context::global_variables_open();
}

void PPCreateTrigger::close()
{
    trigger_name.op->close();
    root.close();
    dynamic_context::global_variables_close();
}

void PPCreateTrigger::accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    trigger_name.op->accept(v);
    if(root.get_operation().op != NULL)
    {
        root.get_operation().op->accept(v);
    }
    v.pop();
}

void PPCreateTrigger::execute()
{
    /* Determine trigger name */
    tuple_cell tc = get_name_from_PPOpIn(trigger_name, "trigger", "create trigger");
    
     /* Determine document or collection name to create trigger on */
    counted_ptr<db_entity> db_ent = root.get_entity("trigger", "create trigger");

    /* Get xptr on this document or collection*/
    xptr root_obj = get_schema_node(db_ent, (std::string("Unknown document/collection passed to create trigger: ") + db_ent->name).c_str());

    local_lock_mrg->put_lock_on_trigger(tc.get_str_mem());
    auth_for_create_trigger(tc.get_str_mem());
	trigger_cell_cptr trc = create_trigger (time,
                                            event,
                                      	    trigger_path,
        	                                gran,
        	                                action,
        	                                innode,
                                            path_to_parent,
                                            (doc_schema_node_xptr) root_obj,
                                            tc.get_str_mem(),
                                            db_ent->name,
                                            (db_ent->type == dbe_document));
}
