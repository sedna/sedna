/*
 * File:  PPAccessors.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPAccessors.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/base/visitor/PPVisitor.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDmStringValue
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmStringValue::PPDmStringValue(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPDmStringValue"),
                                                   child(_child_)
{
}

PPDmStringValue::~PPDmStringValue()
{
    delete child.op;
    child.op = NULL;
}

void PPDmStringValue::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPDmStringValue::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPDmStringValue::do_close()
{
    child.op->close();
}

void PPDmStringValue::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of dm:string-value is not a node");
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of dm:string-value is not a node");

        tuple_cell tc = dm_string_value(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of dm:string-value is not a node");

        t.copy(tc);
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDmStringValue::do_copy(dynamic_context *_cxt_)
{
    PPDmStringValue *res = new PPDmStringValue(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPDmStringValue::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDmTypedValue
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmTypedValue::PPDmTypedValue(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPDmTypedValue"),
                                                 child(_child_)
{
}

PPDmTypedValue::~PPDmTypedValue()
{
    delete child.op;
    child.op = NULL;
}

void PPDmTypedValue::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPDmTypedValue::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPDmTypedValue::do_close()
{
    child.op->close();
}

void PPDmTypedValue::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of dm:typed-value is not a node");
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of dm:typed-value is not a node");

        tuple_cell tc = dm_typed_value(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of dm:typed-value is not a node");

        t.copy(tc);
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDmTypedValue::do_copy(dynamic_context *_cxt_)
{
    PPDmTypedValue *res = new PPDmTypedValue(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPDmTypedValue::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}
