/*
 * File:  PPNumericFuncs.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPNUMERICFUNCS_H
#define __PPNUMERICFUNCS_H

#include "sedna.h"
#include "PPBase.h"


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

    void children(PPOpIn &_child_) { _child_ = child; }

    const char* error();

public:
    tuple_cell fn_abs               (const tuple_cell& tc);
    tuple_cell fn_ceiling           (const tuple_cell& tc);
    tuple_cell fn_floor             (const tuple_cell& tc);
    tuple_cell fn_round             (const tuple_cell& tc);
    tuple_cell fn_round_half_to_even(const tuple_cell& tc);

    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPNumericFuncs(variable_context *_cxt_,
                   PPOpIn _child_,
                   PPNumericFuncs::value_func _func_);
    virtual ~PPNumericFuncs();
};

/*
///////////////////////////////////////////////////////////////////////////////
/// PPFnCeiling
///////////////////////////////////////////////////////////////////////////////
class PPFnCeiling : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnCeiling(variable_context *_cxt_,
                PPOpIn _child_);
    virtual ~PPFnCeiling();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnFloor
///////////////////////////////////////////////////////////////////////////////
class PPFnFloor : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnFloor(variable_context *_cxt_,
                PPOpIn _child_);
    virtual ~PPFnFloor();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnRound
///////////////////////////////////////////////////////////////////////////////
class PPFnRound : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnRound(variable_context *_cxt_,
                PPOpIn _child_);
    virtual ~PPFnRound();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnRoundHalfToEven
///////////////////////////////////////////////////////////////////////////////
class PPFnRoundHalfToEven : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnRoundHalfToEven(variable_context *_cxt_,
                        PPOpIn _child_);
    virtual ~PPFnRoundHalfToEven();
};
*/

#endif
