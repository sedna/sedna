/*
 * File:  PPNodeOps.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __NODEOPS_H
#define __NODEOPS_H

#include "sedna.h"
#include "PPBase.h"

///////////////////////////////////////////////////////////////////////////////
/// PPFnName
///////////////////////////////////////////////////////////////////////////////
class PPFnName : public PPIterator
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

    PPFnName(dynamic_context *_cxt_,
             PPOpIn _child_);
    virtual ~PPFnName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnLocalName
///////////////////////////////////////////////////////////////////////////////
class PPFnLocalName : public PPIterator
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

    PPFnLocalName(dynamic_context *_cxt_,
                  PPOpIn _child_);
    virtual ~PPFnLocalName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnNamespaceUri
///////////////////////////////////////////////////////////////////////////////
class PPFnNamespaceUri : public PPIterator
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

    PPFnNamespaceUri(dynamic_context *_cxt_,
                     PPOpIn _child_);
    virtual ~PPFnNamespaceUri();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnNumber
///////////////////////////////////////////////////////////////////////////////
class PPFnNumber : public PPIterator
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

    PPFnNumber(dynamic_context *_cxt_,
               PPOpIn _child_);
    virtual ~PPFnNumber();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnRoot
///////////////////////////////////////////////////////////////////////////////
class PPFnRoot : public PPIterator
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

    PPFnRoot(dynamic_context *_cxt_,
             PPOpIn _child_);
    virtual ~PPFnRoot();
};


#endif
