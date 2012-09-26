/*
 * File:  PPBooleanOps.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPBooleanOps.h"
#include "tr/executor/fo/boolean_operations.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnTrue
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnTrue::PPFnTrue(dynamic_context *_cxt_,
                   operation_info _info_) : PPIterator(_cxt_, _info_, "PPFnTrue")
{
}

PPFnTrue::~PPFnTrue()
{
}

void PPFnTrue::do_open ()
{
    first_time = true;
}

void PPFnTrue::do_reopen()
{
    first_time = true;
}

void PPFnTrue::do_close()
{
}

void PPFnTrue::do_next (tuple &t)
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

PPIterator* PPFnTrue::do_copy(dynamic_context *_cxt_)
{
    PPFnTrue* res = new PPFnTrue(_cxt_, info);
    return res;
}

void PPFnTrue::do_accept(PPVisitor &v)
{
    v.visit (this);
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnFalse
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnFalse::PPFnFalse(dynamic_context *_cxt_,
                     operation_info _info_) : PPIterator(_cxt_, _info_, "PPFnFalse")
{
}

PPFnFalse::~PPFnFalse()
{
}

void PPFnFalse::do_open ()
{
    first_time = true;
}

void PPFnFalse::do_reopen()
{
    first_time = true;
}

void PPFnFalse::do_close()
{
}

void PPFnFalse::do_next (tuple &t)
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

PPIterator* PPFnFalse::do_copy(dynamic_context *_cxt_)
{
    PPFnFalse* res = new PPFnFalse(_cxt_, info);
    return res;
}

void PPFnFalse::do_accept(PPVisitor &v)
{
    v.visit (this);
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNot
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNot::PPFnNot(dynamic_context *_cxt_,
                 operation_info _info_,
                 PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnNot"),
                                   child(_child_)
{
}

PPFnNot::~PPFnNot()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNot::do_open ()
{
    child.op->open();

    first_time = true;
    eos_reached = true;
}

void PPFnNot::do_reopen()
{
    child.op->reopen();

    first_time = true;
    eos_reached = true;
}

void PPFnNot::do_close()
{
    child.op->close();
}

void PPFnNot::do_next (tuple &t)
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

PPIterator* PPFnNot::do_copy(dynamic_context *_cxt_)
{
    PPFnNot *res = new PPFnNot(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnNot::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnBoolean
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnBoolean::PPFnBoolean(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnBoolean"),
                                           child(_child_)
{
}

PPFnBoolean::~PPFnBoolean()
{
    delete child.op;
    child.op = NULL;
}

void PPFnBoolean::do_open ()
{
    child.op->open();

    first_time = true;
    eos_reached = true;
}

void PPFnBoolean::do_reopen()
{
    child.op->reopen();

    first_time = true;
    eos_reached = true;
}

void PPFnBoolean::do_close()
{
    child.op->close();
}

void PPFnBoolean::do_next (tuple &t)
{
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

PPIterator* PPFnBoolean::do_copy(dynamic_context *_cxt_)
{
    PPFnBoolean *res = new PPFnBoolean(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnBoolean::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}
