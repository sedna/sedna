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
protected:
    int pos;
    sorted_sequence *s;
    PPOpIn child;
    xptr ret_val;
    bool atomic_mode;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPSXptr(dynamic_context *_cxt_, 
            operation_info _info_,
            PPOpIn _child_);
    virtual ~PPSXptr();

	static int  compare_less (xptr v1,xptr v2, const void * Udata);
	static int  get_size (xqp_tuple& t, const void * Udata);
	static void serialize (xqp_tuple& t,xptr v1, const void * Udata);
	static void serialize_2_blks (xqp_tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
	static void deserialize (xqp_tuple &t, xptr& v1, const void * Udata);
	static void deserialize_2_blks (xqp_tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
};


#endif /* __PPSXPTR_H */

