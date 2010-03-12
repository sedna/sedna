/*
 * File:  comparison_operations.h
 * Copyright (C) 2004-2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/fo/comparison_operations.h"
#include "tr/nid/numb_scheme.h"

tuple_cell node_comp_is(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_node() || !a2.is_node())
        throw XQUERY_EXCEPTION2(XPTY0004, "Can not perform node and order comparison on none-nodes");
    return tuple_cell::atomic(nid_cmp(a1.get_node(), a2.get_node()) == 0);
}

tuple_cell node_comp_isnot(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_node() || !a2.is_node())
        throw XQUERY_EXCEPTION2(XPTY0004, "Can not perform node and order comparison on none-nodes");
    return tuple_cell::atomic(nid_cmp(a1.get_node(), a2.get_node()) != 0);
}

tuple_cell order_comp_lt(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_node() || !a2.is_node())
        throw XQUERY_EXCEPTION2(XPTY0004, "Can not perform node and order comparison on none-nodes");
    return tuple_cell::atomic(nid_cmp(a1.get_node(), a2.get_node()) < 0);
}

tuple_cell order_comp_gt(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_node() || !a2.is_node())
        throw XQUERY_EXCEPTION2(XPTY0004, "Can not perform node and order comparison on none-nodes");
    return tuple_cell::atomic(nid_cmp(a1.get_node(), a2.get_node()) > 0);
}
