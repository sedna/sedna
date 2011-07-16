/*
 * File: PPQName.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPQName.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/executor/base/inscns.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnResolveQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnResolveQName::PPFnResolveQName(dynamic_context *_cxt_,
                                   operation_info _info_,
                                   PPOpIn _child_qname_,
                                   PPOpIn _child_elem_) : PPIterator(_cxt_, _info_, "PPFnResolveQName"),
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

void PPFnResolveQName::do_open ()
{
    child_qname.op->open();
    child_elem.op->open();
    first_time = true;
}

void PPFnResolveQName::do_reopen()
{
    child_qname.op->reopen();
    child_elem.op->reopen();
    first_time = true;
}

void PPFnResolveQName::do_close()
{
    child_qname.op->close();
    child_elem.op->close();
}

void PPFnResolveQName::do_next(tuple &t)
{
    if (first_time)
    {
        child_qname.op->next(t);
        if (t.is_eos())
            return;

        first_time = false;
        tuple_cell qname_tc = atomize(child_qname.get(t));

        if (!is_string_type(qname_tc.get_atomic_type())) {
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong first argument of fn:resolve-QName function");
        }

        child_qname.op->next(t);
        if (!(t.is_eos())) {
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong first argument of fn:resolve-QName function");
        }

        qname_tc = tuple_cell::make_sure_light_atomic(qname_tc);

        child_elem.op->next(t);
        if (t.is_eos() || !child_elem.get(t).is_node()) {
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong second argument of fn:resolve-QName function");
        }

        xptr node = child_elem.get(t).get_node();

        child_elem.op->next(t);
        if (!(t.is_eos())) {
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong second argument of fn:resolve-QName function");
        }

        CHECKP(node);
        if (getNodeType(node) != element)
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong second argument of fn:resolve-QName function");

        scoped_ptr<InscopeNamespaceMap> inscopeNamespaces = new InscopeNamespaceMap(node, cxt->get_static_context()->getStaticallyKnownNamespaces());
        t.copy(tuple_cell::atomic(xsd::QName::createResolve(qname_tc.get_str_mem(), inscopeNamespaces.get())));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnResolveQName::do_copy(dynamic_context *_cxt_)
{
    PPFnResolveQName *res = se_new PPFnResolveQName(_cxt_, info, child_qname, child_elem);
    res->child_qname.op = child_qname.op->copy(_cxt_);
    res->child_elem.op = child_elem.op->copy(_cxt_);
    return res;
}

void PPFnResolveQName::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child_qname.op->accept(v);
    child_elem.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnQName::PPFnQName(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPOpIn _child_uri_,
                     PPOpIn _child_qname_) : PPIterator(_cxt_, _info_, "PPFnQName"),
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

void PPFnQName::do_open ()
{
    child_uri.op->open();
    child_qname.op->open();
    first_time = true;
}

void PPFnQName::do_reopen()
{
    child_uri.op->reopen();
    child_qname.op->reopen();
    first_time = true;
}

void PPFnQName::do_close()
{
    child_uri.op->close();
    child_qname.op->close();
}

void PPFnQName::do_next(tuple &t)
{
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

        t.copy(tuple_cell::atomic(xsd::QName::createUCn(uri_tc.is_eos() ? NULL : uri_tc.get_str_mem(), qname_tc.get_str_mem())));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnQName::do_copy(dynamic_context *_cxt_)
{
    PPFnQName *res = se_new PPFnQName(_cxt_, info, child_uri, child_qname);
    res->child_uri.op = child_uri.op->copy(_cxt_);
    res->child_qname.op = child_qname.op->copy(_cxt_);
    return res;
}

void PPFnQName::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child_uri.op->accept(v);
    child_qname.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnPrefixFromQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnPrefixFromQName::PPFnPrefixFromQName(dynamic_context *_cxt_,
                                         operation_info _info_,
                                         PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnPrefixFromQName"),
                                                           child(_child_)
{
}

PPFnPrefixFromQName::~PPFnPrefixFromQName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnPrefixFromQName::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnPrefixFromQName::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnPrefixFromQName::do_close()
{
    child.op->close();
}

void PPFnPrefixFromQName::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
            return;

        if (!(child.get(t).is_atomic()) || child.get(t).get_atomic_type() != xs_QName)
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:prefix-from-QName function");

        tuple_cell tc = child.get(t);

        U_ASSERT(tc.is_light_atomic());

        child.op->next(t);
        if (!(t.is_eos()))
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:prefix-from-QName function");

        if (tc.get_xs_qname().getXmlNs() != NULL_XMLNS) {
            t.copy(tuple_cell::atomic_deep(xs_NCName, tc.get_xs_qname().getPrefix()));
        }
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

PPIterator* PPFnPrefixFromQName::do_copy(dynamic_context *_cxt_)
{
    PPFnPrefixFromQName *res = se_new PPFnPrefixFromQName(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnPrefixFromQName::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnLocalNameFromQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnLocalNameFromQName::PPFnLocalNameFromQName(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnLocalNameFromQName"),
                                                                 child(_child_)
{
}

PPFnLocalNameFromQName::~PPFnLocalNameFromQName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnLocalNameFromQName::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnLocalNameFromQName::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnLocalNameFromQName::do_close()
{
    child.op->close();
}

void PPFnLocalNameFromQName::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
            return;

        if (!(child.get(t).is_atomic()) || child.get(t).get_atomic_type() != xs_QName)
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:local-name-from-QName function");

        tuple_cell tc = child.get(t);

        U_ASSERT(tc.is_light_atomic());

        child.op->next(t);
        if (!(t.is_eos()))
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:local-name-from-QName function");

        const char *local_name = tc.get_xs_qname().getLocalName();
        t.copy(tuple_cell::atomic_deep(xs_NCName, local_name));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnLocalNameFromQName::do_copy(dynamic_context *_cxt_)
{
    PPFnLocalNameFromQName *res = se_new PPFnLocalNameFromQName(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnLocalNameFromQName::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNamespaceUriFromQName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNamespaceUriFromQName::PPFnNamespaceUriFromQName(dynamic_context *_cxt_,
                                                     operation_info _info_,
                                                     PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnNamespaceUriFromQName"),
                                                                       child(_child_)
{
}

PPFnNamespaceUriFromQName::~PPFnNamespaceUriFromQName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNamespaceUriFromQName::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnNamespaceUriFromQName::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnNamespaceUriFromQName::do_close()
{
    child.op->close();
}

void PPFnNamespaceUriFromQName::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
            return;

        if (!(child.get(t).is_atomic()) || child.get(t).get_atomic_type() != xs_QName)
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-from-QName function");

        tuple_cell tc = child.get(t);

        U_ASSERT(tc.is_light_atomic());

        child.op->next(t);
        if (!(t.is_eos()))
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-from-QName function");

        const char *uri = tc.get_xs_qname().getUri();
        if (!uri) uri = "";

        t.copy(tuple_cell::atomic_deep(xs_anyURI, uri));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnNamespaceUriFromQName::do_copy(dynamic_context *_cxt_)
{
    PPFnNamespaceUriFromQName *res = se_new PPFnNamespaceUriFromQName(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnNamespaceUriFromQName::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNamespaceUriForPrefix
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNamespaceUriForPrefix::PPFnNamespaceUriForPrefix(dynamic_context *_cxt_,
                                                     operation_info _info_,
                                                     PPOpIn _child_prefix_,
                                                     PPOpIn _child_element_) : PPIterator(_cxt_, _info_, "PPFnNamespaceUriForPrefix"),
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

void PPFnNamespaceUriForPrefix::do_open ()
{
    child_prefix.op->open();
    child_element.op->open();
    first_time = true;
}

void PPFnNamespaceUriForPrefix::do_reopen()
{
    child_prefix.op->reopen();
    child_element.op->reopen();
    first_time = true;
}

void PPFnNamespaceUriForPrefix::do_close()
{
    child_prefix.op->close();
    child_element.op->close();
}

void PPFnNamespaceUriForPrefix::do_next(tuple &t)
{
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
        if (getNodeType(node) != element)
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:namespace-uri-for-prefix function");

        scoped_ptr<InscopeNamespaceMap> inscopeNamespaces = new InscopeNamespaceMap(node, cxt->get_static_context()->getStaticallyKnownNamespaces());

        xmlns_ptr x = inscopeNamespaces->resolvePrefix(prefix);

        if (x != NULL_XMLNS) {
            t.copy(tuple_cell::atomic_deep(xs_anyURI, x->get_uri()));
            first_time = false;
            return;
        } else if (*prefix == '\0') {
            // For default prefix we should always return some value.
            t.copy(EMPTY_STRING_TC);
            first_time = false;
            return;
        }
    }

    first_time = true;
    t.set_eos();
}

PPIterator* PPFnNamespaceUriForPrefix::do_copy(dynamic_context *_cxt_)
{
    PPFnNamespaceUriForPrefix *res = se_new PPFnNamespaceUriForPrefix(_cxt_, info, child_prefix, child_element);
    res->child_prefix.op = child_prefix.op->copy(_cxt_);
    res->child_element.op = child_element.op->copy(_cxt_);
    return res;
}

void PPFnNamespaceUriForPrefix::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child_prefix.op->accept(v);
    child_element.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnInScopePrefixes
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnInScopePrefixes::PPFnInScopePrefixes(dynamic_context *_cxt_,
                                         operation_info _info_,
                                         PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnInScopePrefixes"),
                                                           child(_child_), namespaces(NULL)
{
}

PPFnInScopePrefixes::~PPFnInScopePrefixes()
{
    delete child.op;
    child.op = NULL;
}

void PPFnInScopePrefixes::do_open ()
{
    child.op->open();
    pos = -1;
}

void PPFnInScopePrefixes::do_reopen()
{
    child.op->reopen();
    pos = -1;

    delete namespaces;
    namespaces = NULL;
}

void PPFnInScopePrefixes::do_close()
{
    child.op->close();
    pos = -1;

    delete namespaces;
    namespaces = NULL;
}

void PPFnInScopePrefixes::do_next (tuple &t)
{
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
        if (getNodeType(node) != element)
            throw XQUERY_EXCEPTION2(XPTY0004, "Wrong argument of fn:in-scope-prefixes function");

        namespaces = new InscopeNamespaceIterator(node, cxt->get_static_context()->getStaticallyKnownNamespaces());

        pos = 0;
    }

    if (namespaces->next())
    {
        xmlns_ptr ns = namespaces->get();

        if (ns->has_prefix())
            t.copy(tuple_cell::atomic_deep(xs_NCName, ns->prefix));
        else
            t.copy(EMPTY_STRING_TC);
    }
    else
    {
        t.set_eos();
        pos = -1;

        delete namespaces;
        namespaces = NULL;
    }
}

PPIterator* PPFnInScopePrefixes::do_copy(dynamic_context *_cxt_)
{
    PPFnInScopePrefixes *res = se_new PPFnInScopePrefixes(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnInScopePrefixes::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

