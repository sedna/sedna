/*
 * File:  PPXptr.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPXPTR_H
#define __PPXPTR_H

#include "sedna.h"
#include "PPBase.h"

class PPXptr : public PPIterator
{
private:
    xptr p;
    bool first_time;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPXptr(variable_context *_cxt_, const xptr &_p_);
    virtual ~PPXptr();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};

#endif
