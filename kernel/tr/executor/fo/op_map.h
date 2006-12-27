/*
 * File:  op_map.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _OP_MAP_H
#define _OP_MAP_H

#include "sedna.h"
#include "tuple.h"
#include "strings.h"

typedef tuple_cell (*bin_op_tuple_cell_tuple_cell)(const tuple_cell&, const tuple_cell&);
typedef tuple_cell (*bin_op_tuple_cell_tuple_cell_collation)(const tuple_cell&, const tuple_cell&, CollationHandler* handler);
typedef tuple_cell (*un_op_tuple_cell)(const tuple_cell&);

enum xq_binary_op_type
{
    xqbop_add = 0,
    xqbop_sub,
    xqbop_mul,
    xqbop_div,
    xqbop_idiv,
    xqbop_mod,
    xqbop_eq,
    xqbop_ne,
    xqbop_gt,
    xqbop_lt,
    xqbop_ge,
    xqbop_le
};

enum xq_unary_op_type
{
    xquop_plus = 0,
    xquop_minus
};

tuple_cell op_add  (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_sub  (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_mul  (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_div  (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_idiv (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_mod  (const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_eq   (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);
tuple_cell op_ne   (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);
tuple_cell op_gt   (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);
tuple_cell op_lt   (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);
tuple_cell op_ge   (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);
tuple_cell op_le   (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);

tuple_cell op_numeric_add           (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_subtract      (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_multiply      (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_divide        (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_integer_divide(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_mod           (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_equal         (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_not_equal     (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_greater_than  (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_less_than     (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_greater_equal (const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_less_equal    (const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_plus (const tuple_cell &a1);
tuple_cell op_minus(const tuple_cell &a1);

tuple_cell op_numeric_unary_plus (const tuple_cell &a1);
tuple_cell op_numeric_unary_minus(const tuple_cell &a1);

struct get_binary_op_res
{
    union {
        bin_op_tuple_cell_tuple_cell_collation bf_c;
        bin_op_tuple_cell_tuple_cell bf;
    } f;
    bool collation;
};

get_binary_op_res get_binary_op(xq_binary_op_type t, xmlscm_type t1, xmlscm_type t2);
un_op_tuple_cell get_unary_op(xq_unary_op_type t, xmlscm_type t1);

#endif

