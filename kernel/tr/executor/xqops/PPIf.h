/*
 * File:  PPIf.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPIF_H
#define __PPIF_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPIf : public PPIterator
{
protected:
    PPOpIn if_child, then_child, else_child;
    PPIterator *data_child;
    xqp_tuple if_data;

    bool eos_reached;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPIf(dynamic_context *_cxt_,
         operation_info _info_,
         PPOpIn _if_child_, 
         PPOpIn _then_child_, 
         PPOpIn _else_child_);
    virtual ~PPIf();
};

#endif
