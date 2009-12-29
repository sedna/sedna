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

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPNumericFuncs(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _child_,
                   PPNumericFuncs::value_func _func_);
    virtual ~PPNumericFuncs();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnRoundHalfToEven
///////////////////////////////////////////////////////////////////////////////
class PPFnRoundHalfToEven : public PPIterator
{
protected:
    PPOpIn child_arg;
    PPOpIn child_p;
    __int64 precision;
    bool first_time;

    tuple_cell round_half_to_even(const tuple_cell& tc, __int64 precision);

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnRoundHalfToEven(dynamic_context *_cxt_,
                        operation_info _info_,
                        PPOpIn _child_arg_,
                        __int64 _precision_);

    PPFnRoundHalfToEven(dynamic_context *_cxt_,
                        operation_info _info_,
                        PPOpIn _child_arg_,
                        PPOpIn _child_p_);
    
    virtual ~PPFnRoundHalfToEven();
};

#endif
