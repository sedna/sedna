/*
 * File:  PPTuple.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPTUPLE_H
#define __PPTUPLE_H

#include "sedna.h"
#include "PPBase.h"

class PPTuple : public PPIterator
{
protected:
    arr_of_PPOpIn ch_arr;
    bool first_time;
    int i;
    tuple lt; // local tuple

    void children(arr_of_PPOpIn &_ch_arr_)
    {
        _ch_arr_ = ch_arr;
    }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPTuple(variable_context *_cxt_,
            const arr_of_PPOpIn &_children_);
    virtual ~PPTuple();
};


#endif
