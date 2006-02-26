/*
 * File:  PPCast.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPCAST_H
#define _PPCAST_H

#include "PPBase.h"

class PPCast : public PPIterator
{
protected:
    // Inhereted through PPIterator
    // query_prolog_type *qp;
    // PPOpOut out;

    // given parameters
    PPOpIn child;

    // obtained parameters and local data
    bool first_time;
    xmlscm_type target_type;
    bool can_be_empty_seq;

    void children(PPOpIn& _child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPCast(variable_context *_cxt_,
           PPOpIn _child_,
           xmlscm_type _target_type_,
           bool _can_be_empty_seq_);
    virtual ~PPCast();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};


#endif
