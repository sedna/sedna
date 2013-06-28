/*
 * File:  PPFilterEL.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPFEL_H
#define _PPFEL_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/sorted_sequence.h"

class PPFilterEL : public PPIterator
{

protected:
    int pos;
    sorted_sequence *s;
    PPOpIn child;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFilterEL(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _child_);
    virtual ~PPFilterEL();
};


#endif
