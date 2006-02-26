/*
 * File:  PPBooleanOps.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPBooleanOps.h"
#include "boolean_operations.h"
#include "PPUtils.h"


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnTrue
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnTrue::PPFnTrue(variable_context *_cxt_) : PPIterator(_cxt_)
{
}

PPFnTrue::~PPFnTrue()
{
}

void PPFnTrue::open  ()
{
    first_time = true;
}

void PPFnTrue::reopen()
{
    first_time = true;
}

void PPFnTrue::close ()
{
}

void PPFnTrue::next  (tuple &t)
{
    if (first_time) 
    {
        first_time = false;
        t.copy(fn_true());
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnTrue::copy(variable_context *_cxt_)
{
    return new PPFnTrue(_cxt_);
}

bool PPFnTrue::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    r = new sequence(fn_true());
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnFalse
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnFalse::PPFnFalse(variable_context *_cxt_) : PPIterator(_cxt_)
{
}

PPFnFalse::~PPFnFalse()
{
}

void PPFnFalse::open  ()
{
    first_time = true;
}

void PPFnFalse::reopen()
{
    first_time = true;
}

void PPFnFalse::close ()
{
}

void PPFnFalse::next  (tuple &t)
{
    if (first_time) 
    {
        first_time = false;
        t.copy(fn_false());
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnFalse::copy(variable_context *_cxt_)
{
    return new PPFnFalse(_cxt_);
}

bool PPFnFalse::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    r = new sequence(fn_false());
    return true;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNot
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNot::PPFnNot(variable_context *_cxt_,
                 PPOpIn _child_) : PPIterator(_cxt_),
                                   child(_child_)
{
}

PPFnNot::~PPFnNot()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNot::open  ()
{
    child.op->open();

    first_time = true;
    eos_reached = true;
}

void PPFnNot::reopen()
{
    child.op->reopen();

    first_time = true;
    eos_reached = true;
}

void PPFnNot::close ()
{
    child.op->close();
}

void PPFnNot::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        if (!eos_reached) child.op->reopen();

        t.copy(my_boolean_not_e(effective_boolean_value(child, t, eos_reached)));
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnNot::copy(variable_context *_cxt_)
{
    PPFnNot *res = new PPFnNot(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnNot::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPFnNot*)cur)->children(child);

    void *not_r;
    bool not_s = (child.op->res_fun())(child.op, cxt, not_r);

    if (!not_s) // if expression is not strict
    { // create PPFnNot and transmit state
        child.op = (PPIterator*)not_r;
        r = new PPFnNot(cxt, child);
        return false;
    }

    r = new sequence(my_boolean_not_e(effective_boolean_value((sequence*)not_r)));
    delete ((sequence*)not_r);
    return true;
}
