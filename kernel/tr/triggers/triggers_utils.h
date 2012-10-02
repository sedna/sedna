/*
 * File:  trigger_utils.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __TRIGGER_UTILS_H
#define __TRIGGER_UTILS_H

#include "common/sedna.h"

#include "tr/triggers/triggers_data.h"
#include "common/lockmantypes.h"
#include "tr/xqp/XQuerytoLR.h"

struct built_trigger_action{
    PPQueryEssence* action_qep_tree;
    qep_parameters_vec parameters;
};

typedef std::map<std::string, std::vector<built_trigger_action> > built_trigger_actions_map;

typedef std::map<schema_node_xptr, int> update_root_map;

typedef std::set<trigger_cell_xptr> t_triggers_set;

typedef std::map<schema_node_xptr, std::set<trigger_cell_xptr> > schema_nodes_triggers_map;

void nested_updates_tracking(lock_mode mode, schema_node_xptr root, const char* doc_name);

void set_action_parameters(xptr parameter_new, xptr parameter_old, xptr parameter_where, trigger_granularity gran, std::string trigger_title);

void clear_built_trigger_actions_map();

void clear_nested_updates_track_map();

bool has_statement_triggers(trigger_event event, trigger_time time);

schema_nodes_triggers_map* get_statement_triggers(schema_nodes_triggers_map* triggers, trigger_event event, trigger_time time);

schema_nodes_triggers_map* get_statement_triggers_on_subtree(schema_node_cptr scm_node, trigger_event event, trigger_time time, schema_nodes_triggers_map* statement_triggers);

// finds trigger that must be fired before inserting a new node
// (when there is no the corresponding node in the descriptive schema)
xptr find_trigger_for_newly_inserted_node(schema_node_cptr parent, const char* ins_node_name, t_item ins_node_type, t_triggers_set* treated_triggers);

// finds trigger that must be fired (according to the specified time, event, granularity) for the node
xptr find_trigger_for_node(schema_node_cptr node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* treated_triggers);

t_triggers_set* find_triggers_for_node(schema_node_cptr node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* triggers);

xptr find_trigger_for_docnode(doc_schema_node_cptr doc_node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* treated_triggers);

t_triggers_set* find_triggers_for_docnode(doc_schema_node_cptr doc_node, trigger_event event, trigger_time time, trigger_granularity granularity, t_triggers_set* triggers);

xptr prepare_old_node(xptr node, schema_node_cptr scm_node, trigger_event event);

extern built_trigger_actions_map built_trigger_actions;
extern int current_nesting_level;

#endif

