/*
 * File:  trigger_utils.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __TRIGGER_UTILS_H
#define __TRIGGER_UTILS_H

#include "tr/triggers/triggers_data.h"
#include "tr/executor/por2qep/por2qep.h"
#include "common/lm_base.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/xqops/PPXptr.h"

struct built_trigger_action{
    PPQueryEssence* action_qep_tree;
    qep_subtree*    action_qep_subtree;
    qep_parameters_vec parameters;
};
typedef std::map<std::string, std::vector<built_trigger_action> > built_trigger_actions_map;

typedef std::map<schema_node*, int> update_root_map;

typedef std::set<trigger_cell*> t_triggers_set;

typedef std::map<schema_node*, std::set<trigger_cell*> > schema_nodes_triggers_map;

void nested_updates_tracking(lock_mode mode, schema_node* root, const char* doc_name);

void set_action_parameters(xptr parameter_new, xptr parameter_old, xptr parameter_where, trigger_granularity gran, std::string trigger_title);

void clear_built_trigger_actions_map();

void clear_nested_updates_track_map();

bool has_statement_triggers(trigger_event event, trigger_time time);

schema_nodes_triggers_map* get_statement_triggers(schema_nodes_triggers_map* triggers, trigger_event event, trigger_time time);

schema_nodes_triggers_map* get_statement_triggers_on_subtree(schema_node* scm_node, trigger_event event, trigger_time time, schema_nodes_triggers_map* statement_triggers);

// finds trigger that must be fired before inserting a new node 
// (when there is no the corresponding node in the descriptive schema)
trigger_cell* find_trigger_for_newly_inserted_node(schema_node* parent, const char* ins_node_name, t_item ins_node_type, t_triggers_set* treated_triggers);

// finds trigger that must be fired (according to the specified time, event, granularity) for the node
trigger_cell* find_trigger_for_node(schema_node* node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* treated_triggers);

t_triggers_set* find_triggers_for_node(schema_node* node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* triggers);

trigger_cell* find_trigger_for_docnode(doc_schema_node* doc_node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* treated_triggers);

t_triggers_set* find_triggers_for_docnode(doc_schema_node* doc_node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* triggers);

xptr prepare_old_node(xptr node, schema_node* scm_node, trigger_event event);

extern built_trigger_actions_map built_trigger_actions;
extern int current_nesting_level;

#endif

