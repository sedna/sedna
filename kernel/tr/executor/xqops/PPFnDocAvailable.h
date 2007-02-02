/*
 * File:  PPFnDocAvailable.h
 * Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFNDOCAVAILABLE_H
#define _PPFNDOCAVAILABLE_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


class PPFnDocAvailable : public PPIterator
{
protected:
    // given parameters
    PPOpIn doc_name_op;
    // obtained parameters and local data
    bool first_time;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPFnDocAvailable(dynamic_context *_cxt_, 
                     PPOpIn _doc_name_op_);
    virtual ~PPFnDocAvailable();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
};



#endif