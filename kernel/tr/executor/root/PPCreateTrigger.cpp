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
using namespace xpath;

PPCreateTrigger::PPCreateTrigger(dynamic_context *_cxt_,
                                 PathExprRoot _root_,
                                 trigger_event _event_,                    
                                 trigger_time _time_,
                                 trigger_granularity _gran_,
                                 PathExpression *_trigger_path_,
                                 scheme_list* _action_,
                                 PPOpIn _trigger_name_) : PPUpdate("PPCreateTrigger"),
                                                          cxt(_cxt_),
                                                          root(_root_),
                                                          event(_event_),
                                                          time(_time_),
                                                          gran(_gran_),
                                                          trigger_path(_trigger_path_),                         
                                                          action(_action_),
                                                          trigger_name(_trigger_name_),
                                                          path_to_parent(NULL)
{
}

PPCreateTrigger::PPCreateTrigger(dynamic_context *_cxt_,
                                 PathExprRoot _root_,
                                 trigger_event _event_,                    
                                 trigger_time _time_,
                                 trigger_granularity _gran_,
                                 PathExpression *_trigger_path_,
                                 scheme_list* _action_,
                                 PPOpIn _trigger_name_,
                                 PathExpression *_path_to_parent_,
                                 inserting_node _innode_) : PPUpdate("PPCreateTrigger"),
                                                            cxt(_cxt_),
                                                            root(_root_),
                                                            event(_event_),
                                                            time(_time_),
                                                            gran(_gran_),
                                                            trigger_path(_trigger_path_),                         
                                                            action(_action_),
                                                            trigger_name(_trigger_name_),
                                                            path_to_parent(_path_to_parent_),
                                                            innode(_innode_)
{
}

PPCreateTrigger::~PPCreateTrigger()
{
    delete trigger_name.op;
    trigger_name.op = NULL;

    delete cxt;
    cxt = NULL;

    delete_scheme_list(action);
    action = NULL;

    innode.release();
    root.release();
}

void PPCreateTrigger::do_open()
{
    trigger_name.op->open();
    root.open();
    cxt->global_variables_open();
}

void PPCreateTrigger::do_close()
{
    trigger_name.op->close();
    root.close();
    cxt->global_variables_close();
}

void PPCreateTrigger::do_accept(PPVisitor &v)
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

void PPCreateTrigger::do_execute()
{
    /* Determine trigger name */
    tuple_cell tc = get_name_from_PPOpIn(trigger_name, "trigger", "create trigger");
    
     /* Determine document or collection name to create trigger on */
    counted_ptr<db_entity> db_ent = root.get_entity("trigger", "create trigger");

    /* Get xptr on this document or collection*/
    xptr root_obj = get_schema_node(db_ent, (string("Unknown document/collection passed to create trigger: ") + db_ent->name).c_str());

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
