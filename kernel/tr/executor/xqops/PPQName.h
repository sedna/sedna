/*
 * File:  PPQName.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPQNAME_H
#define _PPQNAME_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class InscopeNamespaceIterator;

///////////////////////////////////////////////////////////////////////////////
/// PPFnResolveQName
///////////////////////////////////////////////////////////////////////////////
class PPFnResolveQName : public PPIterator
{
protected:
    PPOpIn child_qname;
    PPOpIn child_elem;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnResolveQName(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPOpIn _child_qname_,
                     PPOpIn _child_elem_);
    virtual ~PPFnResolveQName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnQName
///////////////////////////////////////////////////////////////////////////////
class PPFnQName : public PPIterator
{
protected:
    PPOpIn child_uri;
    PPOpIn child_qname;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnQName(dynamic_context *_cxt_,
              operation_info _info_,
              PPOpIn _child_uri_,
              PPOpIn _child_qname_);
    virtual ~PPFnQName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnPrefixFromQName
///////////////////////////////////////////////////////////////////////////////
class PPFnPrefixFromQName : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnPrefixFromQName(dynamic_context *_cxt_,
                        operation_info _info_,
                        PPOpIn _child_);
    virtual ~PPFnPrefixFromQName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnLocalNameFromQName
///////////////////////////////////////////////////////////////////////////////
class PPFnLocalNameFromQName : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnLocalNameFromQName(dynamic_context *_cxt_,
                           operation_info _info_,
                           PPOpIn _child_);
    virtual ~PPFnLocalNameFromQName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnNamespaceUriFromQName
///////////////////////////////////////////////////////////////////////////////
class PPFnNamespaceUriFromQName : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnNamespaceUriFromQName(dynamic_context *_cxt_,
                              operation_info _info_,
                              PPOpIn _child_);
    virtual ~PPFnNamespaceUriFromQName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnNamespaceUriForPrefix
///////////////////////////////////////////////////////////////////////////////
class PPFnNamespaceUriForPrefix : public PPIterator
{
protected:
    PPOpIn child_prefix;
    PPOpIn child_element;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnNamespaceUriForPrefix(dynamic_context *_cxt_,
                              operation_info _info_,
                              PPOpIn _child_prefix_,
                              PPOpIn _child_element_);
    virtual ~PPFnNamespaceUriForPrefix();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnInScopePrefixes
///////////////////////////////////////////////////////////////////////////////
class PPFnInScopePrefixes : public PPIterator
{
protected:
    PPOpIn child;
    InscopeNamespaceIterator * namespaces;
    int pos;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPFnInScopePrefixes(dynamic_context *_cxt_,
                        operation_info _info_,
                        PPOpIn _child_);
    virtual ~PPFnInScopePrefixes();
};

#endif
