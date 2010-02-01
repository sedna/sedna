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
    PathExpr *trigger_path;
    PathExprRoot root;
    trigger_event event;
    trigger_time time;
    trigger_granularity gran;
    scheme_list* action;
    inserting_node innode;
    PathExpr *path_to_parent;
    PPOpIn trigger_name;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);
    
    PPCreateTrigger(trigger_time _time_,
                    trigger_event _event_,
                    PathExprRoot _root_,
                    PathExpr *_trigger_path_,
                    trigger_granularity _granularity_,
                    scheme_list* _action_,
                    PPOpIn _trigger_name_,
                    dynamic_context *_cxt_);
    PPCreateTrigger(trigger_time _time_,
                    trigger_event _event_,
                    PathExprRoot _root_,
                    PathExpr *_trigger_path_,
                    trigger_granularity _granularity_,
                    scheme_list* _action_,
                    const char* _inserting_name_,
                    int _inserting_type,
                    PathExpr *_path_to_parent_,
                    PPOpIn _trigger_name_,
                    dynamic_context *_cxt_);
    ~PPCreateTrigger();
};

#endif /* _PPCREATETRIGGER_H */
