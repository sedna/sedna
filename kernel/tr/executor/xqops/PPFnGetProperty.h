/*
 * File:  PPFnGetProperty.h
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPFNGETPROPERTY_H
#define __PPFNGETPROPERTY_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPFnGetProperty : public PPIterator
{
private:
    PPOpIn child;
    bool first_time;

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnGetProperty(dynamic_context *_cxt_,
                    operation_info _info_,
                    PPOpIn _child_);
    virtual ~PPFnGetProperty();
};

#endif
