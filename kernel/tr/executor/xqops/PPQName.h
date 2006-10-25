/*
 * File:  PPQName.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPQNAME_H
#define _PPQNAME_H

#include "sedna.h"
#include "PPBase.h"

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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnQName(variable_context *_cxt_,
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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnPrefixFromQName(variable_context *_cxt_,
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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnLocalNameFromQName(variable_context *_cxt_,
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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnNamespaceUriFromQName(variable_context *_cxt_,
                              PPOpIn _child_);
    virtual ~PPFnNamespaceUriFromQName();
};

#endif
