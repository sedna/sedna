/*
 * File:  boolean_operations.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"
#include "boolean_operations.h"





/*******************************************************************************
 * OPERATORS ON BOOLEAN VALUES: BEGIN
 ******************************************************************************/

tuple_cell op_boolean_equal_e(const tuple_cell &a1, const tuple_cell &a2)
{
    if (a1.is_eos() || a2.is_eos()) return tuple_cell::eos();
    return op_boolean_equal(a1, a2);
/*
    return op_boolean_equal(a1.is_eos() ? fn_false() : a1, 
                            a2.is_eos() ? fn_false() : a2);
*/
}

tuple_cell op_boolean_less_than_e(const tuple_cell &a1, const tuple_cell &a2)
{
    if (a1.is_eos() || a2.is_eos()) return tuple_cell::eos();
    return op_boolean_less_than(a1, a2);
/*
    return op_boolean_less_than(a1.is_eos() ? fn_false() : a1, 
                                a2.is_eos() ? fn_false() : a2);
*/
}

tuple_cell op_boolean_greater_than_e(const tuple_cell &a1, const tuple_cell &a2)
{
    if (a1.is_eos() || a2.is_eos()) return tuple_cell::eos();
    return op_boolean_greater_than(a1, a2);
/*
    return op_boolean_greater_than(a1.is_eos() ? fn_false() : a1, 
                                   a2.is_eos() ? fn_false() : a2);
*/
}

tuple_cell op_boolean_not_equal_e(const tuple_cell &a1, const tuple_cell &a2)
{
    if (a1.is_eos() || a2.is_eos()) return tuple_cell::eos();
    return op_boolean_not_equal(a1, a2);
/*
    return op_boolean_not_equal(a1.is_eos() ? fn_false() : a1, 
                                a2.is_eos() ? fn_false() : a2);
*/
}

tuple_cell op_boolean_less_equal_e(const tuple_cell &a1, const tuple_cell &a2)
{
    if (a1.is_eos() || a2.is_eos()) return tuple_cell::eos();
    return op_boolean_less_equal(a1, a2);
/*
    return op_boolean_less_equal(a1.is_eos() ? fn_false() : a1, 
                                 a2.is_eos() ? fn_false() : a2);
*/
}

tuple_cell op_boolean_greater_equal_e(const tuple_cell &a1, const tuple_cell &a2)
{
    if (a1.is_eos() || a2.is_eos()) return tuple_cell::eos();
    return op_boolean_greater_equal(a1, a2);
/*
    return op_boolean_greater_equal(a1.is_eos() ? fn_false() : a1, 
                                    a2.is_eos() ? fn_false() : a2);
*/
}


tuple_cell op_boolean_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_boolean ||
        !a2.is_atomic() || a2.get_atomic_type() != xs_boolean)
        throw USER_EXCEPTION2(SE1003, "Calling op:boolean_equal on non-boolean values");

    return tuple_cell::atomic(a1.get_xs_boolean() == a2.get_xs_boolean());
}

tuple_cell op_boolean_less_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_boolean ||
        !a2.is_atomic() || a2.get_atomic_type() != xs_boolean)
        throw USER_EXCEPTION2(SE1003, "Calling op:boolean_less_than on non-boolean values");

    return tuple_cell::atomic(a1.get_xs_boolean() < a2.get_xs_boolean());
}

tuple_cell op_boolean_greater_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_boolean ||
        !a2.is_atomic() || a2.get_atomic_type() != xs_boolean)
        throw USER_EXCEPTION2(SE1003, "Calling op:boolean_greater_than on non-boolean values");

    return tuple_cell::atomic(a1.get_xs_boolean() > a2.get_xs_boolean());
}

tuple_cell op_boolean_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_boolean ||
        !a2.is_atomic() || a2.get_atomic_type() != xs_boolean)
        throw USER_EXCEPTION2(SE1003, "Calling op:boolean_not_equal on non-boolean values");

    return tuple_cell::atomic(a1.get_xs_boolean() != a2.get_xs_boolean());
}

tuple_cell op_boolean_less_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_boolean ||
        !a2.is_atomic() || a2.get_atomic_type() != xs_boolean)
        throw USER_EXCEPTION2(SE1003, "Calling op:boolean_less_equal on non-boolean values");

    return tuple_cell::atomic(a1.get_xs_boolean() <= a2.get_xs_boolean());
}

tuple_cell op_boolean_greater_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_boolean ||
        !a2.is_atomic() || a2.get_atomic_type() != xs_boolean)
        throw USER_EXCEPTION2(SE1003, "Calling op:boolean_greater_equal on non-boolean values");

    return tuple_cell::atomic(a1.get_xs_boolean() > a2.get_xs_boolean());
}


/*******************************************************************************
 * OPERATORS ON BOOLEAN VALUES: END
 ******************************************************************************/



/*******************************************************************************
 * FUNCTIONS ON BOOLEAN VALUES: BEGIN
 ******************************************************************************/

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    NOT READY
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

/*******************************************************************************
 * FUNCTIONS ON BOOLEAN VALUES: END
 ******************************************************************************/

tuple_cell my_boolean_and_e(const tuple_cell &a1, const tuple_cell &a2)
{
    return my_boolean_and(a1.is_eos() ? fn_false() : a1, 
                          a2.is_eos() ? fn_false() : a2);
}

tuple_cell my_boolean_or_e(const tuple_cell &a1, const tuple_cell &a2)
{
    return my_boolean_or(a1.is_eos() ? fn_false() : a1, 
                         a2.is_eos() ? fn_false() : a2);
}

tuple_cell my_boolean_not_e(const tuple_cell &a)
{
    return a.is_eos() ? fn_true() : my_boolean_not(a);
}


tuple_cell my_boolean_and(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_boolean ||
        !a2.is_atomic() || a2.get_atomic_type() != xs_boolean)
        throw USER_EXCEPTION2(SE1003, "Calling boolean_and on non-boolean values");

    return tuple_cell::atomic(a1.get_xs_boolean() && a2.get_xs_boolean());
}

tuple_cell my_boolean_or(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_boolean ||
        !a2.is_atomic() || a2.get_atomic_type() != xs_boolean)
        throw USER_EXCEPTION2(SE1003, "Calling boolean_or on non-boolean values");

    return tuple_cell::atomic(a1.get_xs_boolean() || a2.get_xs_boolean());
}

tuple_cell my_boolean_not(const tuple_cell &a)
{
    if (!a.is_atomic() || a.get_atomic_type() != xs_boolean)
        throw USER_EXCEPTION2(SE1003, "Calling boolean_not on non-boolean value");

    return tuple_cell::atomic(!(a.get_xs_boolean()));
}
