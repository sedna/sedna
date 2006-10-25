/*
 * File: PPQName.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPQName.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnQName::PPFnQName(variable_context *_cxt_,
                     PPOpIn _child_uri_,
                     PPOpIn _child_qname_) : PPIterator(_cxt_),
                                             child_uri(_child_uri_),
                                             child_qname(_child_qname_)
{
}

PPFnQName::~PPFnQName()
{
    delete child_uri.op;
    child_uri.op = NULL;
    delete child_qname.op;
    child_qname.op = NULL;
}

void PPFnQName::open  ()
{
    child_uri.op->open();
    child_qname.op->open();
    first_time = true;
}

void PPFnQName::reopen()
{
    child_uri.op->reopen();
    child_qname.op->reopen();
    first_time = true;
}

void PPFnQName::close ()
{
    child_uri.op->close();
    child_qname.op->close();
}

void PPFnQName::next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        tuple_cell uri_tc; // eos by default
        child_uri.op->next(t);
        if (!t.is_eos())
        {
            if (!(child_uri.get(t).is_atomic()) || child_uri.get(t).get_atomic_type() != xs_string) 
                throw USER_EXCEPTION2(XPTY0004, "Wrong argument of fn:QName function");

            uri_tc = tuple_cell::make_sure_light_atomic(child_uri.get(t));

            child_uri.op->next(t);
            if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Wrong argument of fn:QName function");
        }

        tuple_cell qname_tc;
        child_qname.op->next(t);
        if (t.is_eos())
            throw USER_EXCEPTION2(XPTY0004, "Wrong argument of fn:QName function");

        if (!(child_qname.get(t).is_atomic()) || child_qname.get(t).get_atomic_type() != xs_string) 
            throw USER_EXCEPTION2(XPTY0004, "Wrong argument of fn:QName function");

        qname_tc = tuple_cell::make_sure_light_atomic(child_qname.get(t));

        child_qname.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Wrong argument of fn:QName function");

        char *qname = xs_QName_create(uri_tc.is_eos() ? NULL : uri_tc.get_str_mem(),
                                      qname_tc.get_str_mem(), 
                                      malloc);

        t.copy(tuple_cell::atomic(xs_QName, qname));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnQName::copy(variable_context *_cxt_)
{
    PPFnQName *res = new PPFnQName(_cxt_, child_uri, child_qname);
    res->child_uri.op = child_uri.op->copy(_cxt_);
    res->child_qname.op = child_qname.op->copy(_cxt_);

    return res;
}

bool PPFnQName::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnQName::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnPrefixFromQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnPrefixFromQName::PPFnPrefixFromQName(variable_context *_cxt_,
                                         PPOpIn _child_) : PPIterator(_cxt_),
                                                           child(_child_)
{
}

PPFnPrefixFromQName::~PPFnPrefixFromQName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnPrefixFromQName::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnPrefixFromQName::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnPrefixFromQName::close ()
{
    child.op->close();
}

void PPFnPrefixFromQName::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
            return;

        if (!(child.get(t).is_atomic()) || child.get(t).get_atomic_type() != xs_QName) 
            throw USER_EXCEPTION2(XPTY0004, "Wrong argument of fn:prefix-from-QName function");

        tuple_cell tc = child.get(t);

        U_ASSERT(tc.is_light_atomic());

        child.op->next(t);
        if (!(t.is_eos())) 
            throw USER_EXCEPTION2(XPTY0004, "Wrong argument of fn:prefix-from-QName function");

        const char *prefix = xs_QName_get_prefix(tc.get_str_mem());
        if (prefix)
            t.copy(tuple_cell::atomic_deep(xs_NCName, prefix));
        else
        {
            first_time = true;
            t.set_eos();
        }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnPrefixFromQName::copy(variable_context *_cxt_)
{
    PPFnPrefixFromQName *res = new PPFnPrefixFromQName(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnPrefixFromQName::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnPrefixFromQName::result");
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnLocalNameFromQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnLocalNameFromQName::PPFnLocalNameFromQName(variable_context *_cxt_,
                                               PPOpIn _child_) : PPIterator(_cxt_),
                                                                 child(_child_)
{
}

PPFnLocalNameFromQName::~PPFnLocalNameFromQName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnLocalNameFromQName::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnLocalNameFromQName::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnLocalNameFromQName::close ()
{
    child.op->close();
}

void PPFnLocalNameFromQName::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
            return;

        if (!(child.get(t).is_atomic()) || child.get(t).get_atomic_type() != xs_QName) 
            throw USER_EXCEPTION2(XPTY0004, "Wrong argument of fn:local-name-from-QName function");

        tuple_cell tc = child.get(t);

        U_ASSERT(tc.is_light_atomic());

        child.op->next(t);
        if (!(t.is_eos())) 
            throw USER_EXCEPTION2(XPTY0004, "Wrong argument of fn:local-name-from-QName function");

        const char *local_name = xs_QName_get_local_name(tc.get_str_mem());
        t.copy(tuple_cell::atomic_deep(xs_NCName, local_name));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnLocalNameFromQName::copy(variable_context *_cxt_)
{
    PPFnLocalNameFromQName *res = new PPFnLocalNameFromQName(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnLocalNameFromQName::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnLocalNameFromQName::result");
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNamespaceUriFromQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNamespaceUriFromQName::PPFnNamespaceUriFromQName(variable_context *_cxt_,
                                                     PPOpIn _child_) : PPIterator(_cxt_),
                                                                       child(_child_)
{
}

PPFnNamespaceUriFromQName::~PPFnNamespaceUriFromQName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNamespaceUriFromQName::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnNamespaceUriFromQName::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnNamespaceUriFromQName::close ()
{
    child.op->close();
}

void PPFnNamespaceUriFromQName::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
            return;

        if (!(child.get(t).is_atomic()) || child.get(t).get_atomic_type() != xs_QName) 
            throw USER_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-from-QName function");

        tuple_cell tc = child.get(t);

        U_ASSERT(tc.is_light_atomic());

        child.op->next(t);
        if (!(t.is_eos())) 
            throw USER_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-from-QName function");

        const char *uri = xs_QName_get_uri(tc.get_str_mem());
        if (!uri) uri = "";

        t.copy(tuple_cell::atomic_deep(xs_anyURI, uri));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnNamespaceUriFromQName::copy(variable_context *_cxt_)
{
    PPFnNamespaceUriFromQName *res = new PPFnNamespaceUriFromQName(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnNamespaceUriFromQName::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnNamespaceUriFromQName::result");
}
