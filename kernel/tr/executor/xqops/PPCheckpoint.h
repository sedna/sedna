/*
 * File:  PPCheckpoint.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPCHECKPOINT_H
#define __PPCHECKPOINT_H

#include "sedna.h"
#include "PPBase.h"

class PPCheckpoint : public PPIterator
{
public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPCheckpoint(dynamic_context *_cxt_);
    virtual ~PPCheckpoint();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
};

#endif
