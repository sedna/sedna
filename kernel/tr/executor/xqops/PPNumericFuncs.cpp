/*
 * File:  PPNumericFuncs.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"

#include <math.h>
#include "PPNumericFuncs.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPNumericFuncs
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPNumericFuncs::PPNumericFuncs(variable_context *_cxt_,
                               PPOpIn _child_,
                               PPNumericFuncs::value_func _func_) : PPIterator(_cxt_),
                                                                    child(_child_),
                                                                    func(_func_)
{
}

PPNumericFuncs::~PPNumericFuncs()
{
    delete child.op;
    child.op = NULL;
}

void PPNumericFuncs::open  ()
{
    child.op->open();
    first_time = true;
}

void PPNumericFuncs::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPNumericFuncs::close ()
{
    child.op->close();
}

#define abs_integer(v) ((v) < 0 ? -(v) : (v))
#define abs_decimal(v) ((v).abs())
#define abs_float(v)   (fabsf(v))
#define abs_double(v)  (fabs(v))

tuple_cell PPNumericFuncs::fn_abs(const tuple_cell &tc)
{
    xmlscm_type res_type = primitive_base_type(tc.get_atomic_type());
    switch (res_type)
    {
        case xs_integer: return tuple_cell::atomic(abs_integer(tc.get_xs_integer()));
        case xs_decimal: return tuple_cell::atomic(abs_decimal(tc.get_xs_decimal()));
        case xs_float:   return tuple_cell::atomic(abs_float  (tc.get_xs_float()));
        case xs_double:  return tuple_cell::atomic(abs_double (tc.get_xs_double()));
        default:         throw USER_EXCEPTION2(SE1003, "Impossible case in fn:abs");
    }
}

#define ceil_integer(v) (v)
#define ceil_decimal(v) ((v).ceil())
#define ceil_float(v)   (ceilf(v))
#define ceil_double(v)  (ceil(v))

tuple_cell PPNumericFuncs::fn_ceiling(const tuple_cell& tc)
{
    xmlscm_type res_type = primitive_base_type(tc.get_atomic_type());
    switch (res_type)
    {
        case xs_integer: return tuple_cell::atomic(ceil_integer(tc.get_xs_integer()));
        case xs_decimal: return tuple_cell::atomic(ceil_decimal(tc.get_xs_decimal()));
        case xs_float:   return tuple_cell::atomic(ceil_float  (tc.get_xs_float()));
        case xs_double:  return tuple_cell::atomic(ceil_double (tc.get_xs_double()));
        default:         throw USER_EXCEPTION2(SE1003, "Impossible case in fn:ceiling");
    }
}

#define floor_integer(v) (v)
#define floor_decimal(v) ((v).floor())
#define floor_float(v)   (floorf(v))
#define floor_double(v)  (floor(v))

tuple_cell PPNumericFuncs::fn_floor(const tuple_cell& tc)
{
    xmlscm_type res_type = primitive_base_type(tc.get_atomic_type());
    switch (res_type)
    {
        case xs_integer: return tuple_cell::atomic(floor_integer(tc.get_xs_integer()));
        case xs_decimal: return tuple_cell::atomic(floor_decimal(tc.get_xs_decimal()));
        case xs_float:   return tuple_cell::atomic(floor_float  (tc.get_xs_float()));
        case xs_double:  return tuple_cell::atomic(floor_double (tc.get_xs_double()));
        default:         throw USER_EXCEPTION2(SE1003, "Impossible case in fn:ceiling");
    }
}

#define round_integer(v) (v)
#define round_decimal(v) ((v).round())

float round_float(float v)
{
    float c = ceilf(v);
    float f = floorf(v);
    if ((c - v) > (v - f)) return f;
    else return c;
}

double round_double(double v)
{
    double c = ceil(v);
    double f = floor(v);
    if ((c - v) > (v - f)) return f;
    else return c;
}

tuple_cell PPNumericFuncs::fn_round(const tuple_cell& tc)
{
    xmlscm_type res_type = primitive_base_type(tc.get_atomic_type());
    switch (res_type)
    {
        case xs_integer: return tuple_cell::atomic(round_integer(tc.get_xs_integer()));
        case xs_decimal: return tuple_cell::atomic(round_decimal(tc.get_xs_decimal()));
        case xs_float:   return tuple_cell::atomic(round_float  (tc.get_xs_float()));
        case xs_double:  return tuple_cell::atomic(round_double (tc.get_xs_double()));
        default:         throw USER_EXCEPTION2(SE1003, "Impossible case in fn:ceiling");
    }
}

tuple_cell PPNumericFuncs::fn_round_half_to_even(const tuple_cell& tc)
{
    return tc;
} // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



const char* PPNumericFuncs::error()
{
    if (func == &PPNumericFuncs::fn_abs)
        return "Argument of fn:abs is not a numeric";
    else if (func == &PPNumericFuncs::fn_ceiling)
        return "Argument of fn:ceiling is not a numeric";
    else if (func == &PPNumericFuncs::fn_floor)
        return "Argument of fn:floor is not a numeric";
    else if (func == &PPNumericFuncs::fn_round)
        return "Argument of fn:round is not a numeric";
    else if (func == &PPNumericFuncs::fn_round_half_to_even)
        return "Argument of fn:round-half-to-even is not a numeric";
    else throw USER_EXCEPTION2(SE1003, "Impossible case in PPNumericFuncs::error");
}

void PPNumericFuncs::next  (tuple &t)
{
    if (first_time)
    {
        child.op->next(t);

        if (t.is_eos())
            return;

        first_time = false;

        if (!(child.get(t).is_atomic()) || !(is_numeric_type(child.get(t).get_atomic_type()))) throw USER_EXCEPTION2(XPTY0004, error());
        tuple_cell tc = (this->*func)(child.get(t));

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, error());
        t.copy(tc);
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPNumericFuncs::copy(variable_context *_cxt_)
{
    PPNumericFuncs *res = new PPNumericFuncs(_cxt_, child, func);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPNumericFuncs::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPNumericFuncs::result");
}
