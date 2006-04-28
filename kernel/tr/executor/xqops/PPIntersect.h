/*
 * File:  PPIntersect.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPINTERSECT_H
#define _PPINTERSECT_H

#include "sedna.h"
#include "PPBase.h"

class PPIntersect : public PPIterator
{
protected:
    // given parameters
    PPOpIn child1;
    PPOpIn child2;

    // obtained parameters and local data
    bool tug_first, tug_second;
    xptr xptr1, xptr2;

    void children(PPOpIn& _child1_, PPOpIn& _child2_) { _child1_ = child1; _child2_ = child2; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPIntersect(variable_context *_cxt_,
                PPOpIn _child1_,
                PPOpIn _child2_);
    virtual ~PPIntersect();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};


#endif
