/*
 * File:  PPNodeOps.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "tr/executor/xqops/PPNodeOps.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/casting_operations.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnName::PPFnName(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnName"),
                                     child(_child_)
{
}

PPFnName::~PPFnName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnName::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnName::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnName::do_close()
{
    child.op->close();
}

void PPFnName::do_next (xqp_tuple &t)
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
            t.copy(cast_primitive_to_xs_string(tc));
        }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnName::do_copy(dynamic_context *_cxt_)
{
    PPFnName *res = se_new PPFnName(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnName::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnLocalName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnLocalName::PPFnLocalName(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnLocalName"),
                                               child(_child_)
{
}

PPFnLocalName::~PPFnLocalName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnLocalName::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnLocalName::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnLocalName::do_close()
{
    child.op->close();
}

void PPFnLocalName::do_next (xqp_tuple &t)
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

PPIterator* PPFnLocalName::do_copy(dynamic_context *_cxt_)
{
    PPFnLocalName *res = se_new PPFnLocalName(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnLocalName::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNamespaceUri
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNamespaceUri::PPFnNamespaceUri(dynamic_context *_cxt_,
                                   operation_info _info_,
                                   PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnNamespaceUri"),
                                                     child(_child_)
{
}

PPFnNamespaceUri::~PPFnNamespaceUri()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNamespaceUri::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnNamespaceUri::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnNamespaceUri::do_close()
{
    child.op->close();
}

void PPFnNamespaceUri::do_next (xqp_tuple &t)
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

PPIterator* PPFnNamespaceUri::do_copy(dynamic_context *_cxt_)
{
    PPFnNamespaceUri *res = se_new PPFnNamespaceUri(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnNamespaceUri::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNumber
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNumber::PPFnNumber(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnNumber"),
                                         child(_child_)
{
}

PPFnNumber::~PPFnNumber()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNumber::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnNumber::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnNumber::do_close()
{
    child.op->close();
}

void PPFnNumber::do_next(xqp_tuple &t)
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
                t.copy(cast(tc, xs_double));
            }
            catch(SednaUserException)
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

PPIterator* PPFnNumber::do_copy(dynamic_context *_cxt_)
{
    PPFnNumber *res = se_new PPFnNumber(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnNumber::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnRoot
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnRoot::PPFnRoot(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnRoot"),
                                     child(_child_)
{
}

PPFnRoot::~PPFnRoot()
{
    delete child.op;
    child.op = NULL;
}

void PPFnRoot::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnRoot::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnRoot::do_close()
{
    child.op->close();
}

void PPFnRoot::do_next (xqp_tuple &t)
{
    if (first_time)
    {
        child.op->next(t);

        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:root is not a node");

        tuple_cell tc = tuple_cell::node(getRootNode(child.get(t).get_node()));

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

PPIterator* PPFnRoot::do_copy(dynamic_context *_cxt_)
{
    PPFnRoot *res = se_new PPFnRoot(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnRoot::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}
