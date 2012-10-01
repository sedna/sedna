/*
 * File:  triggers.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __TRIGGERS_H
#define __TRIGGERS_H

#include <set>

#include "common/sedna.h"
#include "common/xptr/xptr.h"

#include "tr/triggers/triggers_data.h"
#include "tr/triggers/triggers_utils.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/structures/schema.h"

xptr triggers_test(xptr new_var, xptr where_var, const char* name, t_item node_type);

void run_triggers(bool isOn);

xptr apply_per_node_triggers(xptr new_var, xptr old_var, xptr where_var, schema_node_cptr scm_node, trigger_time time, trigger_event event);

void apply_per_statement_triggers(xptr_sequence* target_seq, bool target_seq_direct, xptr_sequence* upd_seq, bool upd_seq_direct, trigger_time time, trigger_event event);

trigger_cell_xptr create_trigger (
            enum trigger_time tr_time, 
            enum trigger_event tr_event, 
            xpath::PathExpression *trigger_path,
            enum trigger_granularity tr_gran, 
            scheme_list* action, 
            inserting_node innode, 
            xpath::PathExpression *path_to_parent,
            doc_schema_node_xptr schemaroot, 
            const char * trigger_title, 
            const char* doc_name, 
            bool is_doc);

trigger_cell_xptr find_trigger(const char* trigger_title);

void delete_trigger (const char *trigger_title);


#endif

