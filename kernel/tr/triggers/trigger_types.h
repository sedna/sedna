/*
 * File:  trigger_types.h
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef TRIGGER_TYPES_H_
#define TRIGGER_TYPES_H_

#include "tr/tr_base.h"

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

struct trigger_action_cell
{
    char* statement;
    trigger_action_cell* next;
};

struct inserting_node
{
    char* name;
    t_item type;

    inserting_node(): name(NULL), type(element) {}
    inserting_node(const char* _name_,
                   t_item _type_) {
        name = (char*)malloc(strlen(_name_) + 1);
        strcpy(name, _name_);
        type = _type_;
    }

    inline void release() {
        if(name != NULL) {
            free(name);
            name = NULL;
        }
    }
    inline std::string to_string() const {
        std::string res("");
        if(name!=NULL)
        {
            if(type == attribute) res += "@";
            res += name;
        }
        return res;
    }
};

#endif /* TRIGGER_TYPES_H_ */
