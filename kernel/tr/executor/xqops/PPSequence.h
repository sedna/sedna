/*
 * File:  PPSequence.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPSEQUENCE_H
#define __PPSEQUENCE_H

#include "sedna.h"
#include "PPBase.h"

class PPSequence : public PPIterator
{
protected:
    arr_of_PPOpIn ch_arr;
    int it;

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

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPSequence(dynamic_context *_cxt_,
               const arr_of_PPOpIn &_children_);
    virtual ~PPSequence();
};


#endif
