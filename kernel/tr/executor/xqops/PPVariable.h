/*
 * File:  PPVariable.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPVARIABLE_H
#define __PPVARIABLE_H

#include "sedna.h"
#include "PPBase.h"

class PPVariable : public PPIterator
{
private:
    var_dsc dsc;
    var_c_id id;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPVariable(variable_context *_cxt_, var_dsc _dsc_);
    virtual ~PPVariable();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};

#endif
