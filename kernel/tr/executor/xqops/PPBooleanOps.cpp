/*
 * File:  PPBooleanOps.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPBooleanOps.h"
#include "tr/executor/fo/boolean_operations.h"
#include "tr/executor/base/PPUtils.h"


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnTrue
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnTrue::PPFnTrue(dynamic_context *_cxt_) : PPIterator(_cxt_)
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
    SET_XQUERY_LINE(__xquery_line);

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

    UNDO_XQUERY_LINE;
}

PPIterator* PPFnTrue::copy(dynamic_context *_cxt_)
{
    PPFnTrue* res = se_new PPFnTrue(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnTrue::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    r = se_new sequence(fn_true());
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnFalse
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnFalse::PPFnFalse(dynamic_context *_cxt_) : PPIterator(_cxt_)
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
    SET_XQUERY_LINE(__xquery_line);

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

PPIterator* PPFnFalse::copy(dynamic_context *_cxt_)
{
    PPFnFalse* res = se_new PPFnFalse(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnFalse::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    r = se_new sequence(fn_false());
    return true;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNot
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNot::PPFnNot(dynamic_context *_cxt_,
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
    SET_XQUERY_LINE(__xquery_line);
    
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

PPIterator* PPFnNot::copy(dynamic_context *_cxt_)
{
    PPFnNot *res = se_new PPFnNot(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnNot::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPFnNot*)cur)->children(child);

    void *not_r;
    bool not_s = (child.op->res_fun())(child.op, cxt, not_r);

    if (!not_s) // if expression is not strict
    { // create PPFnNot and transmit state
        child.op = (PPIterator*)not_r;
        r = se_new PPFnNot(cxt, child);
        return false;
    }

    r = se_new sequence(my_boolean_not_e(effective_boolean_value((sequence*)not_r)));
    delete ((sequence*)not_r);
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnBoolean
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////



PPFnBoolean::PPFnBoolean(dynamic_context *_cxt_,
                         PPOpIn _child_) : PPIterator(_cxt_),
                                           child(_child_)
{
}

PPFnBoolean::~PPFnBoolean()
{
    delete child.op;
    child.op = NULL;
}

void PPFnBoolean::open  ()
{
    child.op->open();

    first_time = true;
    eos_reached = true;
}

void PPFnBoolean::reopen()
{
    child.op->reopen();

    first_time = true;
    eos_reached = true;
}

void PPFnBoolean::close ()
{
    child.op->close();
}

void PPFnBoolean::next  (tuple &t)
{
    SET_XQUERY_LINE(__xquery_line);
    
    if (first_time)
    {
        first_time = false;
        if (!eos_reached) child.op->reopen();
        t.copy(effective_boolean_value(child, t, eos_reached));
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnBoolean::copy(dynamic_context *_cxt_)
{
    PPFnBoolean *res = se_new PPFnBoolean(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnBoolean::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnBoolean::result");
}
