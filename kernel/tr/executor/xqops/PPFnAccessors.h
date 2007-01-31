/*
 * File:  PPFnAccessors.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFNACCESSORS_H
#define _PPFNACCESSORS_H

#include "common/sedna.h"
#include "tr/executor/xqops/PPAccessors.h"


///////////////////////////////////////////////////////////////////////////////
/// PPDmNodeKind
///////////////////////////////////////////////////////////////////////////////
class PPDmNodeKind : public PPIterator
{
protected:
    // obtained parameters and local data
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

    PPDmNodeKind(dynamic_context *_cxt_,
                 PPOpIn _child_);
    virtual ~PPDmNodeKind();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnNodeKind
///////////////////////////////////////////////////////////////////////////////
typedef PPDmNodeKind PPFnNodeKind;


///!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


///////////////////////////////////////////////////////////////////////////////
/// PPFnNodeName
///////////////////////////////////////////////////////////////////////////////
class PPFnNodeName : public PPIterator
{
protected:
    // obtained parameters and local data
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

    PPFnNodeName(dynamic_context *_cxt_,
                 PPOpIn _child_);
    virtual ~PPFnNodeName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnNilled
///////////////////////////////////////////////////////////////////////////////
class PPFnNilled : public PPIterator
{
protected:
    // obtained parameters and local data
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

    PPFnNilled(dynamic_context *_cxt_,
               PPOpIn _child_);
    virtual ~PPFnNilled();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnString
///////////////////////////////////////////////////////////////////////////////
class PPFnString : public PPIterator
{
protected:
    // obtained parameters and local data
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

    PPFnString(dynamic_context *_cxt_,
               PPOpIn _child_);
    virtual ~PPFnString();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnData
///////////////////////////////////////////////////////////////////////////////
class PPFnData : public PPIterator
{
protected:
    // obtained parameters and local data
    PPOpIn child;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnData(dynamic_context *_cxt_,
             PPOpIn _child_);
    virtual ~PPFnData();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnBaseURI
///////////////////////////////////////////////////////////////////////////////
class PPFnBaseURI : public PPIterator
{
protected:
    // obtained parameters and local data
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

    PPFnBaseURI(dynamic_context *_cxt_,
                PPOpIn _child_);
    virtual ~PPFnBaseURI();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnDocumentURI
///////////////////////////////////////////////////////////////////////////////
class PPFnDocumentURI : public PPIterator
{
protected:
    // obtained parameters and local data
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

    PPFnDocumentURI(dynamic_context *_cxt_,
                    PPOpIn _child_);
    virtual ~PPFnDocumentURI();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnStaticBaseUri
///////////////////////////////////////////////////////////////////////////////
class PPFnStaticBaseUri : public PPIterator
{

private:
    bool first_time;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnStaticBaseUri(dynamic_context *_cxt_);
    virtual ~PPFnStaticBaseUri();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnDefaultCollation
///////////////////////////////////////////////////////////////////////////////
class PPFnDefaultCollation : public PPIterator
{

private:
    bool first_time;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnDefaultCollation(dynamic_context *_cxt_);
    virtual ~PPFnDefaultCollation();
};



#endif

