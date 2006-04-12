/*
 * File:  PPDDO.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDDO_H
#define _PPDDO_H

#include "PPBase.h"
#include "sorted_sequence.h"

//#define TURN_ON_DDO

class PPDDO : public PPIterator
{
private:
	static int compare_less (xptr& v1,xptr& v2);
	static int get_size (tuple& t);
	static void serialize (tuple& t,xptr v1);
	static void serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2);
	static tuple deserialize (xptr& v1);
protected:
    // Inhereted through PPIterator
    // query_prolog_type *qp;
    // PPOpOut out;

#ifdef TURN_ON_DDO
    int pos;
    sorted_sequence *s;
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
