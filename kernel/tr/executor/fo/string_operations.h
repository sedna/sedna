/*
 * File:  string_operations.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _STRING_OPERATIONS_H
#define _STRING_OPERATIONS_H

#include "sedna.h"
#include "PPBase.h"


tuple_cell fn_compare(const tuple_cell &a1, const tuple_cell &a2);



inline tuple_cell fn_compare_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2).get_xs_integer() == 0);
}

inline tuple_cell fn_compare_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2).get_xs_integer() != 0);
}

inline tuple_cell fn_compare_less_than(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2).get_xs_integer() < 0);
}

inline tuple_cell fn_compare_less_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2).get_xs_integer() <= 0);
}

inline tuple_cell fn_compare_greater_than(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2).get_xs_integer() > 0);
}

inline tuple_cell fn_compare_greater_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    return tuple_cell::atomic(fn_compare(a1, a2).get_xs_integer() >= 0);
}


#endif
