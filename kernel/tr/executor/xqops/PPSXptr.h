/*
 * File:  PPSXptr.h
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef  __PPSXPTR_H
#define  __PPSXPTR_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/sorted_sequence.h"


class PPSXptr : public PPIterator
{
private:

    int pos;
    sorted_sequence *s;
    PPOpIn child;
    xptr ret_val;

    void children(PPOpIn& _child_) { _child_ = child; }

public:

    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPSXptr(dynamic_context *_cxt_, PPOpIn _child_);
    virtual ~PPSXptr();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
	static int  compare_less (xptr v1,xptr v2, const void * Udata);
	static int  get_size (tuple& t, const void * Udata);
	static void serialize (tuple& t,xptr v1, const void * Udata);
	static void serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
	static void deserialize (tuple &t, xptr& v1, const void * Udata);
	static void deserialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
};


#endif /* __PPSXPTR_H */

