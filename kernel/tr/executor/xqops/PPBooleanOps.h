/*
 * File:  PPBooleanOps.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPBOOLEANOPS_H
#define _PPBOOLEANOPS_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

///////////////////////////////////////////////////////////////////////////////
/// PPFnTrue
///////////////////////////////////////////////////////////////////////////////
class PPFnTrue : public PPIterator
{
protected:
    // obtained parameters and local data
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnTrue(dynamic_context *_cxt_, operation_info _info_);
    virtual ~PPFnTrue();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnFalse
///////////////////////////////////////////////////////////////////////////////
class PPFnFalse : public PPIterator
{
protected:
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnFalse(dynamic_context *_cxt_, operation_info _info_);
    virtual ~PPFnFalse();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnNot
///////////////////////////////////////////////////////////////////////////////
class PPFnNot : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;
    bool eos_reached;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnNot(dynamic_context *_cxt_,
            operation_info _info_,
            PPOpIn _child_);
    virtual ~PPFnNot();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnBoolean
///////////////////////////////////////////////////////////////////////////////
class PPFnBoolean : public PPIterator
{
protected:
    bool first_time;
    bool eos_reached;

    PPOpIn child;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnBoolean(dynamic_context *_cxt_,
                operation_info _info_,
				PPOpIn _child_);
    virtual ~PPFnBoolean();
};


#endif
