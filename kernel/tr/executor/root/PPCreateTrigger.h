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

trigger_event symb2trigger_event(const char* str);
trigger_time symb2trigger_time(const char* str);
trigger_granularity symb2trigger_granularity(const char* str);
//char* str2trigger_action(const char *str);
t_item int2t_item(int type);

class PPCreateTrigger : public PPUpdate
{
    // given parameters
    PathExpr *trigger_path;
    counted_ptr<db_entity> db_ent;
    trigger_event event;
    trigger_time time;
    trigger_granularity gran;
    scheme_list* action;
    inserting_node innode;
    PathExpr *path_to_parent;
    PPOpIn trigger_name;
    dynamic_context *cxt;

    // obtained parameters and local data
    schema_node *root;
public:
    void open();
    void close();
    void execute();

    PPCreateTrigger(char* _time_,
                    char* _event_,
                    counted_ptr<db_entity> _db_ent_,
                    PathExpr *_trigger_path_,
                    char* _granularity_,
                    scheme_list* _action_,
                    PPOpIn _trigger_name_,
    				dynamic_context *_cxt_);
    PPCreateTrigger(char* _time_,
                    char* _event_,
                    counted_ptr<db_entity> _db_ent_,
                    PathExpr *_trigger_path_,
                    char* _granularity_,
                    scheme_list* _action_,
                    char* _inserting_name_,
        			int _inserting_type,
                    PathExpr *_path_to_parent_,
                    PPOpIn _trigger_name_,
    				dynamic_context *_cxt_);
    ~PPCreateTrigger();
};



#endif
