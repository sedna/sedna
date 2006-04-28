/*
 * File:  PPAccessors.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPAccessors.h"
#include "dm_accessors.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDmNodeKind
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmNodeKind::PPDmNodeKind(variable_context *_cxt_,
                           PPOpIn _child_) : PPIterator(_cxt_),
                                             child(_child_)
{
}

PPDmNodeKind::~PPDmNodeKind()
{
    delete child.op;
    child.op = NULL;
}

void PPDmNodeKind::open  ()
{
    child.op->open();
    first_time = true;
}

void PPDmNodeKind::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPDmNodeKind::close ()
{
    child.op->close();
}

void PPDmNodeKind::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos()) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-kind is not a node");
        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-kind is not a node");

        dm_node_kind_type res = dm_node_kind(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-kind is not a node");

        switch (res)
        {
        case nk_document				: 
             t.copy(tuple_cell::atomic_deep(xs_string, "document"));
             break;
        case nk_element					: 
             t.copy(tuple_cell::atomic_deep(xs_string, "element"));
             break;
        case nk_attribute				: 
             t.copy(tuple_cell::atomic_deep(xs_string, "attribute"));
             break;
        case nk_text					:
             t.copy(tuple_cell::atomic_deep(xs_string, "text"));
             break;
        case nk_namespace				:
             t.copy(tuple_cell::atomic_deep(xs_string, "namespace"));
             break;
        case nk_processing_instruction	:
             t.copy(tuple_cell::atomic_deep(xs_string, "processing-instruction"));
             break;
        case nk_comment					:
             t.copy(tuple_cell::atomic_deep(xs_string, "comment"));
             break;
        default							: 
             throw USER_EXCEPTION2(SE1003, "Unexpected value in dm:node-kind");
        }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDmNodeKind::copy(variable_context *_cxt_)
{
    PPDmNodeKind *res = new PPDmNodeKind(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPDmNodeKind::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPDmNodeKind*)cur)->children(child);

    void *nk_r;
    bool nk_s = (child.op->res_fun())(child.op, cxt, nk_r);

    if (!nk_s) // if expression is not strict
    { // create PPDmNodeKind and transmit state
        child.op = (PPIterator*)nk_r;
        r = new PPDmNodeKind(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)nk_r;
    if (d_seq->size() != 1) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-kind is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-kind is not a node");

    dm_node_kind_type res = dm_node_kind(tc.get_node());

    switch (res)
    {
        case nk_document				: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "document"));
             return true;
        case nk_element					: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "element"));
             return true;
        case nk_attribute				: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "attribute"));
             return true;
        case nk_text					:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "text"));
             return true;
        case nk_namespace				:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "namespace"));
             return true;
        case nk_processing_instruction	:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "processing-instruction"));
             return true;
        case nk_comment					:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "comment"));
             return true;
        default							: 
             throw USER_EXCEPTION2(SE1003, "Unexpected value in dm:node-kind");
    }
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDmNodeName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmNodeName::PPDmNodeName(variable_context *_cxt_,
                           PPOpIn _child_) : PPIterator(_cxt_),
                                             child(_child_)
{
}

PPDmNodeName::~PPDmNodeName()
{
    delete child.op;
    child.op = NULL;
}

void PPDmNodeName::open  ()
{
    child.op->open();
    first_time = true;
}

void PPDmNodeName::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPDmNodeName::close ()
{
    child.op->close();
}

void PPDmNodeName::next  (tuple &t)
{
    if (first_time)
    {
		child.op->next(t);

        if (t.is_eos()) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-name is not a node");
        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-name is not a node");

        tuple_cell tc = dm_node_name(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-name is not a node");

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

PPIterator* PPDmNodeName::copy(variable_context *_cxt_)
{
    PPDmNodeName *res = new PPDmNodeName(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPDmNodeName::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPDmNodeName*)cur)->children(child);

    void *nn_r;
    bool nn_s = (child.op->res_fun())(child.op, cxt, nn_r);

    if (!nn_s) // if expression is not strict
    { // create PPDmNodeName and transmit state
        child.op = (PPIterator*)nn_r;
        r = new PPDmNodeName(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)nn_r;
    if (d_seq->size() != 1) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-name is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-name is not a node");

    r = new sequence(dm_node_name(tc.get_node()));
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDmStringValue
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmStringValue::PPDmStringValue(variable_context *_cxt_,
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

        if (t.is_eos()) throw USER_EXCEPTION2(XP0006, "Argument of dm:string-value is not a node");
        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XP0006, "Argument of dm:string-value is not a node");

        tuple_cell tc = dm_string_value(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XP0006, "Argument of dm:string-value is not a node");

        t.copy(tc);
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDmStringValue::copy(variable_context *_cxt_)
{
    PPDmStringValue *res = new PPDmStringValue(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPDmStringValue::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPDmStringValue*)cur)->children(child);

    void *sv_r;
    bool sv_s = (child.op->res_fun())(child.op, cxt, sv_r);

    if (!sv_s) // if expression is not strict
    { // create PPDmStringValue and transmit state
        child.op = (PPIterator*)sv_r;
        r = new PPDmStringValue(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)sv_r;
    if (d_seq->size() != 1) throw USER_EXCEPTION2(XP0006, "Argument of dm:string-value is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XP0006, "Argument of dm:string-value is not a node");

    r = new sequence(dm_string_value(tc.get_node()));
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDmTypedValue
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmTypedValue::PPDmTypedValue(variable_context *_cxt_,
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

        if (t.is_eos()) throw USER_EXCEPTION2(XP0006, "Argument of dm:typed-value is not a node");
        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XP0006, "Argument of dm:typed-value is not a node");

        tuple_cell tc = dm_typed_value(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XP0006, "Argument of dm:typed-value is not a node");

        t.copy(tc);
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDmTypedValue::copy(variable_context *_cxt_)
{
    PPDmTypedValue *res = new PPDmTypedValue(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPDmTypedValue::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPDmTypedValue*)cur)->children(child);

    void *tv_r;
    bool tv_s = (child.op->res_fun())(child.op, cxt, tv_r);

    if (!tv_s) // if expression is not strict
    { // create PPDmTypedValue and transmit state
        child.op = (PPIterator*)tv_r;
        r = new PPDmTypedValue(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)tv_r;
    if (d_seq->size() != 1) throw USER_EXCEPTION2(XP0006, "Argument of dm:typed-value is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XP0006, "Argument of dm:typed-value is not a node");

    r = new sequence(dm_typed_value(tc.get_node()));
    return true;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDmDocumentURI
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmDocumentURI::PPDmDocumentURI(variable_context *_cxt_,
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

        if (t.is_eos()) throw USER_EXCEPTION2(XP0006, "Argument of dm:document-uri is not a node");
        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XP0006, "Argument of dm:document-uri is not a node");

        tuple_cell tc = dm_document_uri(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XP0006, "Argument of dm:document-uri is not a node");

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

PPIterator* PPDmDocumentURI::copy(variable_context *_cxt_)
{
    PPDmDocumentURI *res = new PPDmDocumentURI(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPDmDocumentURI::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPDmDocumentURI*)cur)->children(child);

    void *nn_r;
    bool nn_s = (child.op->res_fun())(child.op, cxt, nn_r);

    if (!nn_s) // if expression is not strict
    { // create PPDmDocumentURI and transmit state
        child.op = (PPIterator*)nn_r;
        r = new PPDmDocumentURI(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)nn_r;
    if (d_seq->size() != 1) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-name is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XP0006, "Argument of dm:node-name is not a node");

    r = new sequence(dm_node_name(tc.get_node()));
    return true;
}
