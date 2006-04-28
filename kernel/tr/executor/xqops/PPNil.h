/*
 * File:  PPNil.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPNIL_H
#define __PPNIL_H

#include "sedna.h"
#include "PPBase.h"

class PPNil : public PPIterator
{
public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);


    PPNil(variable_context *_cxt_);
    virtual ~PPNil();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};


#endif
