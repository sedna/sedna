/*
 * File:  PPSequence.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPSEQUENCE_H
#define __PPSEQUENCE_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPSequence : public PPIterator
{
protected:
    arr_of_PPOpIn ch_arr;
    unsigned int it;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPSequence(dynamic_context *_cxt_,
               operation_info _info_,
               const arr_of_PPOpIn &_children_);
    virtual ~PPSequence();
};


#endif
