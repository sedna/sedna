/*
 * File:  PPNumericFuncs.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPNUMERICFUNCS_H
#define __PPNUMERICFUNCS_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


///////////////////////////////////////////////////////////////////////////////
/// PPNumericFuncs
///////////////////////////////////////////////////////////////////////////////
class PPNumericFuncs : public PPIterator
{
public:
    typedef tuple_cell (PPNumericFuncs::*value_func)(const tuple_cell&);

protected:
    PPOpIn child;
    bool first_time;
    value_func func;

    const char* error();

public:
    tuple_cell fn_abs               (const tuple_cell& tc);
    tuple_cell fn_ceiling           (const tuple_cell& tc);
    tuple_cell fn_floor             (const tuple_cell& tc);
    tuple_cell fn_round             (const tuple_cell& tc);
    
    inline const char* value_func2c_string(value_func func)
    {
        if(func == &PPNumericFuncs::fn_abs) return "fn:abs()";
        if(func == &PPNumericFuncs::fn_ceiling) return "fn:ceiling()";
        if(func == &PPNumericFuncs::fn_floor) return "fn:floor()";
        if(func == &PPNumericFuncs::fn_round) return "fn:round()";
        throw USER_EXCEPTION2(SE1003, "Impossible case in numeric function type to string conversion");
    }

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPNumericFuncs(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _child_,
                   PPNumericFuncs::value_func _func_);
    virtual ~PPNumericFuncs();

    inline value_func get_function() const { return func; }
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnRoundHalfToEven
///////////////////////////////////////////////////////////////////////////////
class PPFnRoundHalfToEven : public PPIterator
{
protected:
    PPOpIn child_arg;
    PPOpIn child_p;
    int64_t precision;
    bool first_time;

    tuple_cell round_half_to_even(const tuple_cell& tc, int64_t precision);

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnRoundHalfToEven(dynamic_context *_cxt_,
                        operation_info _info_,
                        PPOpIn _child_arg_,
                        int64_t _precision_);

    PPFnRoundHalfToEven(dynamic_context *_cxt_,
                        operation_info _info_,
                        PPOpIn _child_arg_,
                        PPOpIn _child_p_);
    
    virtual ~PPFnRoundHalfToEven();
};

#endif
