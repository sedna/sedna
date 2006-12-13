/*
 * File:  PPSResLStub.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPSRESLSTUB_H
#define _PPSRESLSTUB_H

#include "sedna.h"
#include "PPBase.h"

class PPSResLStub : public PPIterator
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

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPSResLStub(dynamic_context *_cxt_, PPIterator *_op_, sequence *_s_);
    virtual ~PPSResLStub();
};


#endif
