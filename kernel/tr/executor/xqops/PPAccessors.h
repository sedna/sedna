/*
 * File:  PPAccessors.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPACCESSORS_H
#define _PPACCESSORS_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

///////////////////////////////////////////////////////////////////////////////
/// PPDmStringValue
///////////////////////////////////////////////////////////////////////////////
class PPDmStringValue : public PPIterator
{
protected:
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
    PPDmStringValue(dynamic_context *_cxt_,
                    operation_info _info_,
                    PPOpIn _child_);
    virtual ~PPDmStringValue();
};

///////////////////////////////////////////////////////////////////////////////
/// PPDmTypedValue
///////////////////////////////////////////////////////////////////////////////
class PPDmTypedValue : public PPIterator
{
protected:
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
    PPDmTypedValue(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _child_);
    virtual ~PPDmTypedValue();
};

#endif

