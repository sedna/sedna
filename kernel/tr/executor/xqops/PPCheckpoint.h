/*
 * File:  PPCheckpoint.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPCHECKPOINT_H
#define __PPCHECKPOINT_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPCheckpoint : public PPIterator
{
private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t) ; 
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPCheckpoint(dynamic_context *_cxt_, operation_info _info_);
    virtual ~PPCheckpoint();
};

#endif
