/*
 * File:  PPNil.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPNIL_H
#define __PPNIL_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPNil : public PPIterator
{
private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPNil(dynamic_context *_cxt_, operation_info _info_);
    virtual ~PPNil();
};


#endif
