/*
 * File: PPQName.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPQName.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnResolveQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnResolveQName::PPFnResolveQName(dynamic_context *_cxt_,
                     PPOpIn _child_qname_,
                     PPOpIn _child_elem_) : PPIterator(_cxt_),
                                             child_qname(_child_qname_),
                                             child_elem(_child_elem_)
{
}

PPFnResolveQName::~PPFnResolveQName()
{
    delete child_qname.op;
    child_qname.op = NULL;
    delete child_elem.op;
    child_elem.op = NULL;
}

void PPFnResolveQName::open  ()
{
    child_qname.op->open();
    child_elem.op->open();
    first_time = true;
}

void PPFnResolveQName::reopen()
{
    child_qname.op->reopen();
    child_elem.op->reopen();
    first_time = true;
}

void PPFnResolveQName::close ()
{
    child_qname.op->close();
    child_elem.op->close();
}

void PPFnResolveQName::next(tuple &t)
{
    SET_CURRENT_PP(this);
    
    if (first_time)
    {
        child_qname.op->next(t);
        if (t.is_eos())
            {RESTORE_CURRENT_PP; return;}

        first_time = false;
        tuple_cell qname_tc = atomize(child_qname.get(t));

        if (!is_string_type(qname_tc.get_atomic_type())) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong first argument of fn:resolve-QName function");

        child_qname.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Wrong first argument of fn:resolve-QName function");

        qname_tc = tuple_cell::make_sure_light_atomic(qname_tc);


        child_elem.op->next(t);

        if (t.is_eos())
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong second argument of fn:resolve-QName function");

        if (!child_elem.get(t).is_node())
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong second argument of fn:resolve-QName function");

        xptr node = child_elem.get(t).get_node();

        child_elem.op->next(t);
        if (!(t.is_eos())) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong second argument of fn:resolve-QName function");

        CHECKP(node);
        if (GETSCHEMENODE(XADDR(node))->type != element)
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong second argument of fn:resolve-QName function");


        char *qname = xs_QName_create(qname_tc.get_str_mem(),
                                      node, 
                                      malloc,
                                      cxt);

        t.copy(tuple_cell::atomic(xs_QName, qname));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }

    RESTORE_CURRENT_PP;
}

PPIterator* PPFnResolveQName::copy(dynamic_context *_cxt_)
{
    PPFnResolveQName *res = se_new PPFnResolveQName(_cxt_, child_qname, child_elem);
    res->child_qname.op = child_qname.op->copy(_cxt_);
    res->child_elem.op = child_elem.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnResolveQName::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnResolveQName::result");
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnQName::PPFnQName(dynamic_context *_cxt_,
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
    SET_CURRENT_PP(this);
    
    if (first_time)
    {
        first_time = false;

        tuple_cell uri_tc; // eos by default
        child_uri.op->next(t);
        if (!t.is_eos())
        {
            if (!(child_uri.get(t).is_atomic()) || child_uri.get(t).get_atomic_type() != xs_string) 
                throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:QName function");

            uri_tc = tuple_cell::make_sure_light_atomic(child_uri.get(t));

            child_uri.op->next(t);
            if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:QName function");
        }

        tuple_cell qname_tc;
        child_qname.op->next(t);
        if (t.is_eos())
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:QName function");

        if (!(child_qname.get(t).is_atomic()) || child_qname.get(t).get_atomic_type() != xs_string) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:QName function");

        qname_tc = tuple_cell::make_sure_light_atomic(child_qname.get(t));

        child_qname.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:QName function");

        char *qname = xs_QName_create(uri_tc.is_eos() ? NULL : uri_tc.get_str_mem(),
                                      qname_tc.get_str_mem(), 
                                      malloc,
                                      cxt);

        t.copy(tuple_cell::atomic(xs_QName, qname));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }

    RESTORE_CURRENT_PP;
}

PPIterator* PPFnQName::copy(dynamic_context *_cxt_)
{
    PPFnQName *res = se_new PPFnQName(_cxt_, child_uri, child_qname);
    res->child_uri.op = child_uri.op->copy(_cxt_);
    res->child_qname.op = child_qname.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnQName::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnQName::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnPrefixFromQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnPrefixFromQName::PPFnPrefixFromQName(dynamic_context *_cxt_,
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
    SET_CURRENT_PP(this);
    
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
            {RESTORE_CURRENT_PP; return;}

        if (!(child.get(t).is_atomic()) || child.get(t).get_atomic_type() != xs_QName) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:prefix-from-QName function");

        tuple_cell tc = child.get(t);

        U_ASSERT(tc.is_light_atomic());

        child.op->next(t);
        if (!(t.is_eos())) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:prefix-from-QName function");

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

    RESTORE_CURRENT_PP;
}

PPIterator* PPFnPrefixFromQName::copy(dynamic_context *_cxt_)
{
    PPFnPrefixFromQName *res = se_new PPFnPrefixFromQName(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnPrefixFromQName::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnPrefixFromQName::result");
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnLocalNameFromQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnLocalNameFromQName::PPFnLocalNameFromQName(dynamic_context *_cxt_,
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
    SET_CURRENT_PP(this);
    
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
            {RESTORE_CURRENT_PP; return;}

        if (!(child.get(t).is_atomic()) || child.get(t).get_atomic_type() != xs_QName) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:local-name-from-QName function");

        tuple_cell tc = child.get(t);

        U_ASSERT(tc.is_light_atomic());

        child.op->next(t);
        if (!(t.is_eos())) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:local-name-from-QName function");

        const char *local_name = xs_QName_get_local_name(tc.get_str_mem());
        t.copy(tuple_cell::atomic_deep(xs_NCName, local_name));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }

    RESTORE_CURRENT_PP;
}

PPIterator* PPFnLocalNameFromQName::copy(dynamic_context *_cxt_)
{
    PPFnLocalNameFromQName *res = se_new PPFnLocalNameFromQName(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnLocalNameFromQName::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnLocalNameFromQName::result");
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNamespaceUriFromQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNamespaceUriFromQName::PPFnNamespaceUriFromQName(dynamic_context *_cxt_,
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
    SET_CURRENT_PP(this);
    
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
            {RESTORE_CURRENT_PP; return;}

        if (!(child.get(t).is_atomic()) || child.get(t).get_atomic_type() != xs_QName) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-from-QName function");

        tuple_cell tc = child.get(t);

        U_ASSERT(tc.is_light_atomic());

        child.op->next(t);
        if (!(t.is_eos())) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-from-QName function");

        const char *uri = xs_QName_get_uri(tc.get_str_mem());
        if (!uri) uri = "";

        t.copy(tuple_cell::atomic_deep(xs_anyURI, uri));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }

    RESTORE_CURRENT_PP;
}

PPIterator* PPFnNamespaceUriFromQName::copy(dynamic_context *_cxt_)
{
    PPFnNamespaceUriFromQName *res = se_new PPFnNamespaceUriFromQName(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnNamespaceUriFromQName::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnNamespaceUriFromQName::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNamespaceUriForPrefix
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNamespaceUriForPrefix::PPFnNamespaceUriForPrefix(dynamic_context *_cxt_,
                                                     PPOpIn _child_prefix_,
                                                     PPOpIn _child_element_) : PPIterator(_cxt_),
                                                                               child_prefix(_child_prefix_),
                                                                               child_element(_child_element_)
{
}

PPFnNamespaceUriForPrefix::~PPFnNamespaceUriForPrefix()
{
    delete child_prefix.op;
    child_prefix.op = NULL;
    delete child_element.op;
    child_element.op = NULL;
}

void PPFnNamespaceUriForPrefix::open  ()
{
    child_prefix.op->open();
    child_element.op->open();
    first_time = true;
}

void PPFnNamespaceUriForPrefix::reopen()
{
    child_prefix.op->reopen();
    child_element.op->reopen();
    first_time = true;
}

void PPFnNamespaceUriForPrefix::close ()
{
    child_prefix.op->close();
    child_element.op->close();
}

void PPFnNamespaceUriForPrefix::next(tuple &t)
{
    SET_CURRENT_PP(this);
    
    if (first_time)
    {
        tuple_cell prefix_tc;
        const char *prefix = "";
        child_prefix.op->next(t);
        if (!t.is_eos())
        {
            if (!(child_prefix.get(t).is_atomic()) || child_prefix.get(t).get_atomic_type() != xs_string) 
                throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-for-prefix function");

            prefix_tc = tuple_cell::make_sure_light_atomic(child_prefix.get(t));
            prefix = prefix_tc.get_str_mem();

            child_prefix.op->next(t);
            if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-for-prefix function");
        }

        child_element.op->next(t);
        if (t.is_eos())
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-for-prefix function");

        if (!child_element.get(t).is_node())
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-for-prefix function");

        child_element.op->next(t);
        if (!(t.is_eos())) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-for-prefix function");

        xptr node = child_element.get(t).get_node();

        CHECKP(node);
        if (GETSCHEMENODE(XADDR(node))->type != element)
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-for-prefix function");



        std::vector<xmlns_ptr> xmlns;
        get_in_scope_namespaces(node, xmlns, cxt);

        for (unsigned int i = 0; i < xmlns.size(); ++i)
        {
            const char *pr = (xmlns[i]->prefix ? xmlns[i]->prefix : "");
            if (strcmp(pr, prefix) == 0)
            {
                t.copy(tuple_cell::atomic_deep(xs_anyURI, xmlns[i]->uri));
                first_time = false;
                {RESTORE_CURRENT_PP; return;}
            }
        }
    }

    first_time = true;
    t.set_eos();

    RESTORE_CURRENT_PP;
}

PPIterator* PPFnNamespaceUriForPrefix::copy(dynamic_context *_cxt_)
{
    PPFnNamespaceUriForPrefix *res = se_new PPFnNamespaceUriForPrefix(_cxt_, child_prefix, child_element);
    res->child_prefix.op = child_prefix.op->copy(_cxt_);
    res->child_element.op = child_element.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnNamespaceUriForPrefix::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnNamespaceUriForPrefix::result");
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnInScopePrefixes
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnInScopePrefixes::PPFnInScopePrefixes(dynamic_context *_cxt_,
                                         PPOpIn _child_) : PPIterator(_cxt_),
                                                           child(_child_)
{
}

PPFnInScopePrefixes::~PPFnInScopePrefixes()
{
    delete child.op;
    child.op = NULL;
}

void PPFnInScopePrefixes::open  ()
{
    child.op->open();
    pos = -1;
}

void PPFnInScopePrefixes::reopen()
{
    child.op->reopen();
    pos = -1;
    xmlns.clear();
}

void PPFnInScopePrefixes::close ()
{
    child.op->close();
    pos = -1;
    xmlns.clear();
}

void PPFnInScopePrefixes::next  (tuple &t)
{
    SET_CURRENT_PP(this);
    
    if (pos < 0)
    {
        child.op->next(t);

        if (t.is_eos())
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:in-scope-prefixes function");

        if (!child.get(t).is_node())
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:in-scope-prefixes function");

        xptr node = child.get(t).get_node();

        child.op->next(t);
        if (!(t.is_eos())) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:in-scope-prefixes function");

        CHECKP(node);
        if (GETSCHEMENODE(XADDR(node))->type != element)
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:in-scope-prefixes function");


        get_in_scope_namespaces(node, xmlns, cxt);
        pos = 0;
    }

    if (pos < (signed)xmlns.size())
    {
        xmlns_ptr ns = xmlns[pos++];
        if (ns->prefix)
            t.copy(tuple_cell::atomic_deep(xs_NCName, ns->prefix));
        else
            t.copy(EMPTY_STRING_TC);
    }
    else
    {
        t.set_eos();
        pos = -1;
        xmlns.clear();
    }

    RESTORE_CURRENT_PP;
}

PPIterator* PPFnInScopePrefixes::copy(dynamic_context *_cxt_)
{
    PPFnInScopePrefixes *res = se_new PPFnInScopePrefixes(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFnInScopePrefixes::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnInScopePrefixes::result");
}
