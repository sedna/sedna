/*
 * File:  PPVariable.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPVARIABLE_H
#define __PPVARIABLE_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

///////////////////////////////////////////////////////////////////////////////
/// PPVariable
///////////////////////////////////////////////////////////////////////////////
class PPVariable : public PPIterator
{
private:
    /*
     * Can't use current variable context from cxt since
     * during function body copying we in fact get a new variable_context
     */
    producer *var_prod;
    var_dsc dsc;
    var_c_id id;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPVariable(dynamic_context *_cxt_, operation_info _info_, var_dsc _dsc_);
    virtual ~PPVariable();
    inline var_dsc get_variable_descriptor() { return dsc; }
};

///////////////////////////////////////////////////////////////////////////////
/// PPGlobalVariable
///////////////////////////////////////////////////////////////////////////////
class PPGlobalVariable : public PPIterator
{
private:
    global_var_dsc dsc;
    var_c_id id;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPGlobalVariable(dynamic_context *_cxt_,
                     operation_info _info_,
                     global_var_dsc _dsc_);
    virtual ~PPGlobalVariable();

    inline global_var_dsc get_variable_descriptor() { return dsc; }
};

#endif /* __PPVARIABLE_H */
