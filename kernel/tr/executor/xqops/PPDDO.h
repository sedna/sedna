/*
 * File:  PPDDO.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDDO_H
#define _PPDDO_H

#include "PPBase.h"
#include "xptr_sequence.h"

//#define TURN_ON_DDO

class PPDDO : public PPIterator
{
protected:
    // Inhereted through PPIterator
    // query_prolog_type *qp;
    // PPOpOut out;

#ifdef TURN_ON_DDO
    int pos;
    xptr_sequence *s;
#endif

    // given parameters
    PPOpIn child;

    void children(PPOpIn& _child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPDDO(variable_context *_cxt_,
          PPOpIn _child_);
    virtual ~PPDDO();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};


#endif
