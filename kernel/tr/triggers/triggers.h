/*
 * File:  triggers.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __TRIGGERS_H
#define __TRIGGERS_H

#include <set>
#include "triggers_data.h"
#include "triggers_utils.h"
#include "xptr.h"
#include "xptr_sequence.h"
#include "schema.h"


xptr apply_per_node_triggers(xptr new_var, xptr old_var, xptr where_var, trigger_time time, trigger_event event);

void apply_per_statement_triggers(xptr_sequence* target_seq, xptr_sequence* upd_seq, trigger_time time, trigger_event event);

#endif

