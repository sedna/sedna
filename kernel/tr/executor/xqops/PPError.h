/*
 * File:  PPError.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPERROR_H
#define _PPERROR_H

#include "common/sedna.h"
#include "tr/crmutils/crmbase.h"
#include "tr/executor/base/PPBase.h"

///////////////////////////////////////////////////////////////////////////////
/// fn:error
///////////////////////////////////////////////////////////////////////////////
class PPFnError : public PPIterator
{
protected:
    PPOpIn child_err;
    PPOpIn child_descr;
    PPOpIn child_obj;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnError(dynamic_context *_cxt_, 
              operation_info _info_,
              PPOpIn &_child_err_,
              PPOpIn &_child_descr_,
              PPOpIn &_child_obj_);
    virtual ~PPFnError();
};

///////////////////////////////////////////////////////////////////////////////
/// fn:trace
///////////////////////////////////////////////////////////////////////////////
class PPFnTrace : public PPIterator
{
protected:
    PPOpIn value_child, label_child;
    tuple_cell tc;
    bool first_time;
    se_ostream *dostr;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnTrace(dynamic_context *_cxt_, 
              operation_info _info_,
              PPOpIn _value_child_,
              PPOpIn _label_child_);
    virtual ~PPFnTrace();
};

#endif
