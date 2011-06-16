/*
 * File:  PPCreateTrigger.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPCREATETRIGGER_H
#define _PPCREATETRIGGER_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/triggers/triggers_data.h"

class PPCreateTrigger : public PPUpdate
{
private:
    dynamic_context *cxt;
    xpath::PathExprRoot root;
    trigger_event event;
    trigger_time time;
    trigger_granularity gran;
    xpath::PathExpression *trigger_path;
    scheme_list* action;
    PPOpIn trigger_name;
    xpath::PathExpression *path_to_parent;
    inserting_node innode;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:
    
    PPCreateTrigger(dynamic_context *_cxt_,
                    xpath::PathExprRoot _root_,
                    trigger_event _event_,                    
                    trigger_time _time_,
                    trigger_granularity _gran_,
                    xpath::PathExpression *_trigger_path_,
                    scheme_list* _action_,
                    PPOpIn _trigger_name_);

    PPCreateTrigger(dynamic_context *_cxt_,
                    xpath::PathExprRoot _root_,
                    trigger_event _event_,
                    trigger_time _time_,
                    trigger_granularity _gran_,
                    xpath::PathExpression *_trigger_path_,
                    scheme_list* _action_,
                    PPOpIn _trigger_name_,
                    xpath::PathExpression *_path_to_parent_,
                    inserting_node _innode_);
    
    ~PPCreateTrigger();
    
    inline trigger_event get_trigger_event() const { return event; }
    inline trigger_time get_trigger_time() const { return time; }
    inline trigger_granularity get_trigger_granularity() const { return gran; }
    inline const inserting_node& get_inserting_node() const { return innode; }
    inline const xpath::PathExprRoot& get_path_expression_root() const { return root; }
    inline const xpath::PathExpression* get_trigger_path() { return trigger_path; }
    inline const xpath::PathExpression* get_path_to_parent() { return path_to_parent; }
};

#endif /* _PPCREATETRIGGER_H */
