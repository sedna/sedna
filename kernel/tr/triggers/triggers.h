/*
 * File:  triggers.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __TRIGGERS_H
#define __TRIGGERS_H

#include <set>
#include "tr/triggers/triggers_data.h"
#include "tr/triggers/triggers_utils.h"
#include "common/xptr.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/structures/schema.h"

xptr triggers_test(xptr new_var, xptr where_var, const char* name, t_item node_type);

xptr apply_per_node_triggers(xptr new_var, xptr old_var, xptr where_var, schema_node* scm_node, trigger_time time, trigger_event event);

void apply_per_statement_triggers(xptr_sequence* target_seq, bool target_seq_direct, xptr_sequence* upd_seq, bool upd_seq_direct, trigger_time time, trigger_event event);

#endif

