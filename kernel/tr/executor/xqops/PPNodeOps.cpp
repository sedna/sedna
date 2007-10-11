/*
 * File:  PPNodeOps.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "tr/executor/xqops/PPNodeOps.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/xqops/PPSLStub.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/casting_operations.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnName::PPFnName(dynamic_context *_cxt_,
                   PPOpIn _child_) : PPIterator(_cxt_),
                                     child(_child_)
{
}

PPFnName::~PPFnName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnName::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnName::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnName::close ()
{
    child.op->close();
}

void PPFnName::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
        {
            t.copy(EMPTY_STRING_TC);
            return;
        }

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:name is not a node");

        tuple_cell tc = dm_node_name(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:name is not a node");

        if (tc.is_eos())
        {
            t.copy(EMPTY_STRING_TC);
        }
        else
        {
            t.copy(cast_primitive_to_xs_string(tc, __xquery_line));
        }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnName::copy(dynamic_context *_cxt_)
{
    PPFnName *res = se_new PPFnName(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnName::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPFnName*)cur)->children(child);

    void *nn_r;
    bool nn_s = (child.op->res_fun())(child.op, cxt, nn_r);

    if (!nn_s) // if expression is not strict
    { // create PPFnName and transmit state
        child.op = (PPIterator*)nn_r;
        r = se_new PPFnName(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)nn_r;
    if (d_seq->size() == 0)
    {
        r = se_new sequence(EMPTY_STRING_TC);
        return true;
    }

    if (d_seq->size() != 1) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:name is not a node");
    tuple_cell tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:name is not a node");

    tc.set_xtype(xs_string); // !!! dangerous
    r = se_new sequence(dm_node_name(tc.get_node()));
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnLocalName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnLocalName::PPFnLocalName(dynamic_context *_cxt_,
                             PPOpIn _child_) : PPIterator(_cxt_),
                                               child(_child_)
{
}

PPFnLocalName::~PPFnLocalName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnLocalName::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnLocalName::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnLocalName::close ()
{
    child.op->close();
}

void PPFnLocalName::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
        {
            t.copy(EMPTY_STRING_TC);
            return;
        }

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:local-name is not a node");

        tuple_cell tc = se_node_local_name(child.get(t).get_node());
        tc.set_xtype(xs_string);

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:local-name is not a node");

        if (tc.is_eos())
        {
            t.copy(EMPTY_STRING_TC);
        }
        else
        {
            t.copy(tc);
        }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnLocalName::copy(dynamic_context *_cxt_)
{
    PPFnLocalName *res = se_new PPFnLocalName(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnLocalName::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnLocalName::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNamespaceUri
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNamespaceUri::PPFnNamespaceUri(dynamic_context *_cxt_,
                                   PPOpIn _child_) : PPIterator(_cxt_),
                                                     child(_child_)
{
}

PPFnNamespaceUri::~PPFnNamespaceUri()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNamespaceUri::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnNamespaceUri::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnNamespaceUri::close ()
{
    child.op->close();
}

void PPFnNamespaceUri::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
        {
            t.copy(EMPTY_STRING_TC);
            return;
        }

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:namespace-uri is not a node");

        tuple_cell tc = se_node_namespace_uri(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:namespace-uri is not a node");

        if (tc.is_eos())
        {
            t.copy(EMPTY_STRING_TC);
        }
        else
        {
            t.copy(tc);
        }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnNamespaceUri::copy(dynamic_context *_cxt_)
{
    PPFnNamespaceUri *res = se_new PPFnNamespaceUri(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnNamespaceUri::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnNamespaceUri::result");
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNumber
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNumber::PPFnNumber(dynamic_context *_cxt_,
                       PPOpIn _child_) : PPIterator(_cxt_),
                                         child(_child_)
{
}

PPFnNumber::~PPFnNumber()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNumber::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnNumber::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnNumber::close ()
{
    child.op->close();
}

void PPFnNumber::next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
        {
            t.copy(tuple_cell::atomic(double_NaN));
            return;
        }

        tuple_cell tc = atomize(child.get(t));

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:number is a sequence of length more than 1");

        if (tc.is_eos())
        {
            t.copy(tuple_cell::atomic(double_NaN));
        }
        else
        {
            try
            {
                t.copy(cast(tc, xs_double, __xquery_line));
            }
            catch(SednaUserException &e)
            {
                t.copy(tuple_cell::atomic(double_NaN));
            }
        }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnNumber::copy(dynamic_context *_cxt_)
{
    PPFnNumber *res = se_new PPFnNumber(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnNumber::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnNumber::result");
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnRoot
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnRoot::PPFnRoot(dynamic_context *_cxt_,
                   PPOpIn _child_) : PPIterator(_cxt_),
                                     child(_child_)
{
}

PPFnRoot::~PPFnRoot()
{
    delete child.op;
    child.op = NULL;
}

void PPFnRoot::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnRoot::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnRoot::close ()
{
    child.op->close();
}

void PPFnRoot::next  (tuple &t)
{
    if (first_time)
    {
        child.op->next(t);

        if (t.is_eos())
            return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:root is not a node");

        tuple_cell tc = tuple_cell::node(getRoot(child.get(t).get_node()));

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:root is not a node");

        first_time = false;
        t.copy(tc);
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnRoot::copy(dynamic_context *_cxt_)
{
    PPFnRoot *res = se_new PPFnRoot(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnRoot::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnRoot::result");
}
