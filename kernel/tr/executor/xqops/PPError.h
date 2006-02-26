/*
 * File:  PPError.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPERROR_H
#define _PPERROR_H

#include "PPBase.h"

class PPFnError : public PPIterator
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

    PPFnError(variable_context *_cxt_, 
              PPOpIn _child_);
    virtual ~PPFnError();
};


#endif
