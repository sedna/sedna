/*
 * File:  PPFilterEL.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPFEL_H
#define _PPFEL_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/sorted_sequence.h"

class PPFilterEL : public PPIterator
{

protected:
    int pos;
    sorted_sequence *s;


    // given parameters
    PPOpIn child;

    void children(PPOpIn& _child_) { _child_ = child; }

public:

    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPFilterEL(dynamic_context *_cxt_,
          PPOpIn _child_);
    virtual ~PPFilterEL();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
};


#endif
