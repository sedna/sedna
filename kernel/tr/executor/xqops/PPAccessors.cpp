/*
 * File:  PPAccessors.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPAccessors.h"
#include "tr/executor/base/dm_accessors.h"




///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDmStringValue
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmStringValue::PPDmStringValue(dynamic_context *_cxt_,
                                 PPOpIn _child_) : PPIterator(_cxt_),
                                                   child(_child_)
{
}

PPDmStringValue::~PPDmStringValue()
{
    delete child.op;
    child.op = NULL;
}

void PPDmStringValue::open  ()
{
    child.op->open();
    first_time = true;
}

void PPDmStringValue::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPDmStringValue::close ()
{
    child.op->close();
}

void PPDmStringValue::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:string-value is not a node");
        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:string-value is not a node");

        tuple_cell tc = dm_string_value(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:string-value is not a node");

        t.copy(tc);
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDmStringValue::copy(dynamic_context *_cxt_)
{
    PPDmStringValue *res = se_new PPDmStringValue(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPDmStringValue::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPDmStringValue*)cur)->children(child);

    void *sv_r;
    bool sv_s = (child.op->res_fun())(child.op, cxt, sv_r);

    if (!sv_s) // if expression is not strict
    { // create PPDmStringValue and transmit state
        child.op = (PPIterator*)sv_r;
        r = se_new PPDmStringValue(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)sv_r;
    if (d_seq->size() != 1) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:string-value is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:string-value is not a node");

    r = se_new sequence(dm_string_value(tc.get_node()));
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDmTypedValue
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmTypedValue::PPDmTypedValue(dynamic_context *_cxt_,
                               PPOpIn _child_) : PPIterator(_cxt_),
                                                 child(_child_)
{
}

PPDmTypedValue::~PPDmTypedValue()
{
    delete child.op;
    child.op = NULL;
}

void PPDmTypedValue::open  ()
{
    child.op->open();
    first_time = true;
}

void PPDmTypedValue::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPDmTypedValue::close ()
{
    child.op->close();
}

void PPDmTypedValue::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:typed-value is not a node");
        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:typed-value is not a node");

        tuple_cell tc = dm_typed_value(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:typed-value is not a node");

        t.copy(tc);
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDmTypedValue::copy(dynamic_context *_cxt_)
{
    PPDmTypedValue *res = se_new PPDmTypedValue(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPDmTypedValue::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPDmTypedValue*)cur)->children(child);

    void *tv_r;
    bool tv_s = (child.op->res_fun())(child.op, cxt, tv_r);

    if (!tv_s) // if expression is not strict
    { // create PPDmTypedValue and transmit state
        child.op = (PPIterator*)tv_r;
        r = se_new PPDmTypedValue(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)tv_r;
    if (d_seq->size() != 1) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:typed-value is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:typed-value is not a node");

    r = se_new sequence(dm_typed_value(tc.get_node()));
    return true;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDmDocumentURI
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmDocumentURI::PPDmDocumentURI(dynamic_context *_cxt_,
                                 PPOpIn _child_) : PPIterator(_cxt_),
                                                   child(_child_)
{
}

PPDmDocumentURI::~PPDmDocumentURI()
{
    delete child.op;
    child.op = NULL;
}

void PPDmDocumentURI::open  ()
{
    child.op->open();
    first_time = true;
}

void PPDmDocumentURI::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPDmDocumentURI::close ()
{
    child.op->close();
}

void PPDmDocumentURI::next  (tuple &t)
{
    if (first_time)
    {
		child.op->next(t);

        if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:document-uri is not a node");
        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:document-uri is not a node");

        tuple_cell tc = dm_document_uri(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:document-uri is not a node");

        if (tc.is_eos()) t.set_eos();
		else 
		{
			t.copy(tc);
			first_time = false;
		}
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDmDocumentURI::copy(dynamic_context *_cxt_)
{
    PPDmDocumentURI *res = se_new PPDmDocumentURI(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPDmDocumentURI::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPDmDocumentURI*)cur)->children(child);

    void *nn_r;
    bool nn_s = (child.op->res_fun())(child.op, cxt, nn_r);

    if (!nn_s) // if expression is not strict
    { // create PPDmDocumentURI and transmit state
        child.op = (PPIterator*)nn_r;
        r = se_new PPDmDocumentURI(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)nn_r;
    if (d_seq->size() != 1) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:node-name is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of dm:node-name is not a node");

    r = se_new sequence(dm_node_name(tc.get_node()));
    return true;
}
