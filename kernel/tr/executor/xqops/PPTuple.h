/*
 * File:  PPTuple.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPTUPLE_H
#define __PPTUPLE_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPTuple : public PPIterator
{
protected:
    arr_of_PPOpIn ch_arr;
    bool first_time;
    unsigned int i;
    xqp_tuple lt; // local tuple

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPTuple(dynamic_context *_cxt_,
            operation_info _info_,
            const arr_of_PPOpIn &_children_);
    virtual ~PPTuple();
};


#endif
