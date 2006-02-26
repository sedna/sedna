/*
 * File:  PPFnAccessors.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFNACCESSORS_H
#define _PPFNACCESSORS_H

#include "PPAccessors.h"


///////////////////////////////////////////////////////////////////////////////
/// PPFnNodeKind
///////////////////////////////////////////////////////////////////////////////
typedef PPDmNodeKind PPFnNodeKind;


///////////////////////////////////////////////////////////////////////////////
/// PPFnNodeName
///////////////////////////////////////////////////////////////////////////////
typedef PPDmNodeName PPFnNodeName;


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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnString(variable_context *_cxt_,
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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnData(variable_context *_cxt_,
             PPOpIn _child_);
    virtual ~PPFnData();
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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnDocumentURI(variable_context *_cxt_,
                    PPOpIn _child_);
    virtual ~PPFnDocumentURI();
};


#endif

