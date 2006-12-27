/*
 * File:  comparison_operations.h
 * Copyright (C) 2004-2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _COMPARISON_OPERATIONS_H
#define _COMPARISON_OPERATIONS_H

#include "sedna.h"
#include "op_map.h"

tuple_cell node_comp_is(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell node_comp_isnot(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell order_comp_lt(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell order_comp_gt(const tuple_cell &a1, const tuple_cell &a2);

#endif
