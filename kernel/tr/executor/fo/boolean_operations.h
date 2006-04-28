/*
 * File:  boolean_operations.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _BOOLEAN_OPERATIONS_H
#define _BOOLEAN_OPERATIONS_H

#include "sedna.h"
#include "PPBase.h"

/*******************************************************************************
 * BOOLEAN CONSTRUCTOR FUNCTIONS: BEGIN
 ******************************************************************************/

inline tuple_cell fn_true()
{
    return tuple_cell::atomic(true);
}

inline tuple_cell fn_false()
{
    return tuple_cell::atomic(false);
}

/*******************************************************************************
 * BOOLEAN CONSTRUCTOR FUNCTIONS: END
 ******************************************************************************/


/*******************************************************************************
 * OPERATORS ON BOOLEAN VALUES: BEGIN
 ******************************************************************************/

tuple_cell op_boolean_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_boolean_less_than(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_boolean_greater_than(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_boolean_not_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_boolean_less_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_boolean_greater_equal(const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_boolean_equal_e(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_boolean_less_than_e(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_boolean_greater_than_e(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_boolean_not_equal_e(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_boolean_less_equal_e(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_boolean_greater_equal_e(const tuple_cell &a1, const tuple_cell &a2);

/*******************************************************************************
 * OPERATORS ON BOOLEAN VALUES: END
 ******************************************************************************/


tuple_cell my_boolean_and(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell my_boolean_or(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell my_boolean_not(const tuple_cell &a);

tuple_cell my_boolean_and_e(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell my_boolean_or_e(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell my_boolean_not_e(const tuple_cell &a);


#endif

