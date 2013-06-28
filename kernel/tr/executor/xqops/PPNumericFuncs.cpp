/*
 * File:  PPNumericFuncs.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include <math.h>
#include "tr/executor/xqops/PPNumericFuncs.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/visitor/PPVisitor.h"


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPNumericFuncs
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPNumericFuncs::PPNumericFuncs(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _child_,
                               PPNumericFuncs::value_func _func_) : PPIterator(_cxt_, _info_, "PPNumericFuncs"),
                                                                    child(_child_),
                                                                    func(_func_)
{
}

PPNumericFuncs::~PPNumericFuncs()
{
    delete child.op;
    child.op = NULL;
}

void PPNumericFuncs::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPNumericFuncs::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPNumericFuncs::do_close()
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
        default:         throw USER_EXCEPTION2(SE1003, "Impossible case in fn:floor");
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
        default:         throw USER_EXCEPTION2(SE1003, "Impossible case in fn:round");
    }
}

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
    else throw USER_EXCEPTION2(SE1003, "Impossible case in PPNumericFuncs::error");
}

void PPNumericFuncs::do_next (xqp_tuple &t)
{
    
    
    if (first_time)
    {
        child.op->next(t);

        if (t.is_eos())
            return;

        first_time = false;

        if (!(child.get(t).is_atomic()) || !(is_numeric_type(child.get(t).get_atomic_type()))) throw XQUERY_EXCEPTION2(XPTY0004, error());
        tuple_cell tc = (this->*func)(child.get(t));

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, error());
        t.copy(tc);
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPNumericFuncs::do_copy(dynamic_context *_cxt_)
{
    PPNumericFuncs *res = se_new PPNumericFuncs(_cxt_, info, child, func);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPNumericFuncs::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnRoundHalfToEven
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnRoundHalfToEven::PPFnRoundHalfToEven(dynamic_context *_cxt_,
                                         operation_info _info_,
                                         PPOpIn _child_arg_,
                                         int64_t _precision_) : PPIterator(_cxt_, _info_, "PPFnRoundHalfToEven"),
                                                                child_arg(_child_arg_),
                                                                precision(_precision_)
{
}

PPFnRoundHalfToEven::PPFnRoundHalfToEven(dynamic_context *_cxt_,
                                         operation_info _info_,
                                         PPOpIn _child_arg_,
                                         PPOpIn _child_p_) : PPIterator(_cxt_, _info_, "PPFnRoundHalfToEven"),
                                                             child_arg(_child_arg_),
                                                             child_p(_child_p_),
                                                             precision(0)
{
}

PPFnRoundHalfToEven::~PPFnRoundHalfToEven()
{
    delete child_arg.op;
    child_arg.op = NULL;
    if (child_p.op)
    {
        delete child_p.op;
        child_p.op = NULL;
    }
}

void PPFnRoundHalfToEven::do_open()
{
    child_arg.op->open();
    if (child_p.op) child_p.op->open();
    first_time = true;
}

void PPFnRoundHalfToEven::do_reopen()
{
    child_arg.op->reopen();
    if (child_p.op) child_p.op->reopen();
    first_time = true;
}

void PPFnRoundHalfToEven::do_close()
{
    child_arg.op->close();
    if (child_p.op) child_p.op->close();
}


#define round_half_to_even_decimal(v, p) ((v).round_half_to_even(p))

tuple_cell PPFnRoundHalfToEven::round_half_to_even(const tuple_cell& tc, int64_t precision)
{
    xmlscm_type res_type = primitive_base_type(tc.get_atomic_type());
    switch (res_type)
    {
        case xs_integer: return tuple_cell::atomic(round_half_to_even_integer(tc.get_xs_integer(), precision));
        case xs_decimal: return tuple_cell::atomic(round_half_to_even_decimal(tc.get_xs_decimal(), precision));
        case xs_float:   return tuple_cell::atomic(round_half_to_even_float  (tc.get_xs_float(),   precision));
        case xs_double:  return tuple_cell::atomic(round_half_to_even_double (tc.get_xs_double(),  precision));
        default:         throw USER_EXCEPTION2(SE1003, "Impossible case in fn:round-half-to-even");
    }
}

void PPFnRoundHalfToEven::do_next(xqp_tuple &t)
{
    if (first_time)
    {
        child_arg.op->next(t);

        if (t.is_eos())
            return;

        tuple_cell tc_arg = child_arg.get(t);
        if (!(tc_arg.is_atomic()) || !(is_numeric_type(tc_arg.get_atomic_type()))) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:round-half-to-even is not a numeric");

        child_arg.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:round-half-to-even is not a numeric");

        if (child_p.op)
        {
            child_p.op->next(t);

            if (t.is_eos())
                throw XQUERY_EXCEPTION2(XPTY0004, "Precision argument of fn:round-half-to-even is not an xs:integer");

            tuple_cell tc_p = child_p.get(t);
            if (!(tc_p.is_atomic()) || (tc_p.get_atomic_type() != xs_integer))
                throw XQUERY_EXCEPTION2(XPTY0004, "Precision argument of fn:round-half-to-even is not an xs:integer");

            precision = tc_p.get_xs_integer();

            child_p.op->next(t);
            if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Precision argument of fn:round-half-to-even is not an xs:integer");
        }

        first_time = false;
        t.copy(round_half_to_even(tc_arg, precision));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnRoundHalfToEven::do_copy(dynamic_context *_cxt_)
{
    PPFnRoundHalfToEven *res = NULL;
    if (child_p.op)
    {
        res = se_new PPFnRoundHalfToEven(_cxt_, info, child_arg, child_p);
        res->child_p.op = child_p.op->copy(_cxt_);
    }
    else
    {
        res = se_new PPFnRoundHalfToEven(_cxt_, info, child_arg, precision);
    }

    res->child_arg.op = child_arg.op->copy(_cxt_);

    return res;
}

void PPFnRoundHalfToEven::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if(child_p.op) child_p.op->accept(v);
    child_arg.op->accept(v);
    v.pop();
}
