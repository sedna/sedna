/*
 * File:  PPSLStub.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPSLSTUB_H
#define _PPSLSTUB_H


#include "PPBase.h"

class PPSLStub : public PPIterator
{
private:
    PPIterator *op;
    sequence *s;
    sequence::iterator it;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return NULL; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPSLStub(variable_context *_cxt_, PPIterator *_op_, sequence *_s_);
    virtual ~PPSLStub();
};


#endif
