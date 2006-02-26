/*
 * File:  PPAccessors.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPACCESSORS_H
#define _PPACCESSORS_H

#include "PPBase.h"


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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPDmNodeKind(variable_context *_cxt_,
                 PPOpIn _child_);
    virtual ~PPDmNodeKind();
};

///////////////////////////////////////////////////////////////////////////////
/// PPDmNodeName
///////////////////////////////////////////////////////////////////////////////
class PPDmNodeName : public PPIterator
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

    PPDmNodeName(variable_context *_cxt_,
                 PPOpIn _child_);
    virtual ~PPDmNodeName();
};

///////////////////////////////////////////////////////////////////////////////
/// PPDmStringValue
///////////////////////////////////////////////////////////////////////////////
class PPDmStringValue : public PPIterator
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

    PPDmStringValue(variable_context *_cxt_,
                    PPOpIn _child_);
    virtual ~PPDmStringValue();
};

///////////////////////////////////////////////////////////////////////////////
/// PPDmTypedValue
///////////////////////////////////////////////////////////////////////////////
class PPDmTypedValue : public PPIterator
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

    PPDmTypedValue(variable_context *_cxt_,
                   PPOpIn _child_);
    virtual ~PPDmTypedValue();
};

///////////////////////////////////////////////////////////////////////////////
/// PPDmDocumentURI
///////////////////////////////////////////////////////////////////////////////
class PPDmDocumentURI : public PPIterator
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

    PPDmDocumentURI(variable_context *_cxt_,
                    PPOpIn _child_);
    virtual ~PPDmDocumentURI();
};


#endif

