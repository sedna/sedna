/*
 * File:  PPConGen.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPCONGEN_H
#define _PPCONGEN_H

#include "PPBase.h"


///////////////////////////////////////////////////////////////////////////////
/// PPConGen1
///////////////////////////////////////////////////////////////////////////////
class PPConGen1 : public PPIterator
{
protected:
    // obtained parameters and local data
    PPOpIn child;
    tuple data;
    var_dsc dsc;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPConGen1(variable_context *_cxt_,
              var_dsc _dsc_,
              PPOpIn _child_);
    virtual ~PPConGen1();
};

///////////////////////////////////////////////////////////////////////////////
/// PPConGen2
///////////////////////////////////////////////////////////////////////////////
class PPConGen2 : public PPIterator
{
protected:
    // obtained parameters and local data
    PPOpIn child;
    tuple data;
    sequence *s;
    int counter;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPConGen2(variable_context *_cxt_,
              PPOpIn _child_);
    virtual ~PPConGen2();
};


#endif
