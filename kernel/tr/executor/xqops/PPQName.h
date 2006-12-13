/*
 * File:  PPQName.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPQNAME_H
#define _PPQNAME_H

#include "sedna.h"
#include "PPBase.h"

///////////////////////////////////////////////////////////////////////////////
/// PPFnResolveQName
///////////////////////////////////////////////////////////////////////////////
class PPFnResolveQName : public PPIterator
{
protected:
    PPOpIn child_qname;
    PPOpIn child_elem;
    bool first_time;

    void children(PPOpIn &_child_qname_, PPOpIn &_child_elem_) { _child_qname_ = child_qname; _child_elem_ = child_elem; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnResolveQName(dynamic_context *_cxt_,
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

    void children(PPOpIn &_child_uri_, PPOpIn &_child_qname_) { _child_uri_ = child_uri; _child_qname_ = child_qname; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnQName(dynamic_context *_cxt_,
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

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnPrefixFromQName(dynamic_context *_cxt_,
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

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnLocalNameFromQName(dynamic_context *_cxt_,
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

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnNamespaceUriFromQName(dynamic_context *_cxt_,
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

    void children(PPOpIn &_child_prefix_, PPOpIn &_child_element_) { _child_prefix_ = child_prefix; _child_element_ = child_element; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnNamespaceUriForPrefix(dynamic_context *_cxt_,
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
    std::vector<xml_ns*> xmlns;
    int pos;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnInScopePrefixes(dynamic_context *_cxt_,
                        PPOpIn _child_);
    virtual ~PPFnInScopePrefixes();
};

#endif
