/*
 * File:  PPInstanceOf.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPINSTANCEOF_H
#define _PPINSTANCEOF_H

#include "PPBase.h"
#include "SequenceType.h"

class PPInstanceOf : public PPIterator
{
protected:
    // Inhereted through PPIterator
    // query_prolog_type *qp;
    // PPOpOut out;

    // given parameters
    PPOpIn child;
    sequence_type st;

    // obtained parameters and local data
    bool first_time;
    bool eos_reached;

    void children(PPOpIn& _child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPInstanceOf(variable_context *_cxt_,
                 PPOpIn _child_,
                 const sequence_type& _st_);
    virtual ~PPInstanceOf();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};


#endif
