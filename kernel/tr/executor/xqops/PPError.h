/*
 * File:  PPError.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPERROR_H
#define _PPERROR_H

#include "sedna.h"
#include "exec_output.h"
#include "PPBase.h"

///////////////////////////////////////////////////////////////////////////////
/// fn:error
///////////////////////////////////////////////////////////////////////////////
class PPFnError : public PPIterator
{
protected:
    // obtained parameters and local data
    PPOpIn child_err;
    PPOpIn child_descr;
    PPOpIn child_obj;

    void children(PPOpIn &_child_err_,
                  PPOpIn &_child_descr_,
                  PPOpIn &_child_obj_) 
    { 
        _child_err_   = child_err; 
        _child_descr_ = child_descr;
        _child_obj_   = child_obj; 
    }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnError(dynamic_context *_cxt_, 
              PPOpIn &_child_err_,
              PPOpIn &_child_descr_,
              PPOpIn &_child_obj_);
    virtual ~PPFnError();
};

///////////////////////////////////////////////////////////////////////////////
/// fn:trace
///////////////////////////////////////////////////////////////////////////////
class PPFnTrace : public PPIterator
{
protected:
    // obtained parameters and local data
    PPOpIn value_child, label_child;
    bool first_time;
    se_ostream *debug_ostream;

    void children(PPOpIn &_value_child_, PPOpIn &_label_child_) 
    { 
        _value_child_ = value_child; 
        _label_child_ = label_child; 
    }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnTrace(dynamic_context *_cxt_, 
              PPOpIn _value_child_,
              PPOpIn _label_child_);
    virtual ~PPFnTrace();
};


#endif
