/*
 * File:  PPUnion.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPUNION_H
#define _PPUNION_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPUnion : public PPIterator
{
protected:
    PPOpIn child1;
    PPOpIn child2;
    bool doc_order;

    bool tug_first, tug_second;
    xptr xptr1, xptr2;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPUnion(dynamic_context *_cxt_,
            operation_info _info_,
            PPOpIn _child1_,
            PPOpIn _child2_,
            bool _doc_order_);
    virtual ~PPUnion();
};


#endif
