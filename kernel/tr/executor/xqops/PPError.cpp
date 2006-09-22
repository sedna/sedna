/*
 * File:  PPError.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"
#include "PPError.h"

///////////////////////////////////////////////////////////////////////////////
/// fn:error
///////////////////////////////////////////////////////////////////////////////
PPFnError::PPFnError(variable_context *_cxt_,
                     PPOpIn _child_) : PPIterator(_cxt_),
                                       child(_child_)
{
}

PPFnError::~PPFnError()
{
    delete child.op;
    child.op = NULL;
}

void PPFnError::open  ()
{
    child.op->open();
}

void PPFnError::reopen()
{
    child.op->reopen();
}

void PPFnError::close ()
{
    child.op->close();
}

void PPFnError::next  (tuple &t)
{
    child.op->next(t);

    if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:error");

    tuple_cell tc = child.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:error");

    child.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:error");
        
    tc = tuple_cell::make_sure_light_atomic(tc);

    throw USER_EXCEPTION2(FOER0000, tc.get_str_mem());
}

PPIterator* PPFnError::copy(variable_context *_cxt_)
{
    PPFnError *res = new PPFnError(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnError::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPFnError*)cur)->children(child);

    void *err_r;
    bool err_s = (child.op->res_fun())(child.op, cxt, err_r);

    if (!err_s) // if expression is not strict
    { // create PPFnError and transmit state
        child.op = (PPIterator*)err_r;
        r = new PPFnError(cxt, child);
        return false;
    }
/* !!!!!!!!!!!!!!!!!!!!!!!!!!
    r = new sequence(my_boolean_not_e(effective_boolean_value((sequence*)not_r)));
    delete ((sequence*)not_r);
*/
    return true;
}


///////////////////////////////////////////////////////////////////////////////
/// fn:trace
///////////////////////////////////////////////////////////////////////////////
PPFnTrace::PPFnTrace(variable_context *_cxt_,
                     PPOpIn _value_child_,
                     PPOpIn _label_child_) : PPIterator(_cxt_),
                                             value_child(_value_child_),
                                             label_child(_label_child_),
                                             first_time(true)
{
}

PPFnTrace::~PPFnTrace()
{
    delete value_child.op;
    value_child.op = NULL;
    delete label_child.op;
    label_child.op = NULL;
}

void PPFnTrace::open  ()
{
    value_child.op->open();
    label_child.op->open();
    first_time = true;
}

void PPFnTrace::reopen()
{
    value_child.op->reopen();
    label_child.op->reopen();
    first_time = true;
}

void PPFnTrace::close ()
{
    value_child.op->close();
    label_child.op->close();
}

void PPFnTrace::next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        label_child.op->next(t);    
        if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:trace");
    
        tuple_cell tc = label_child.get(t);
        if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
            throw USER_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:trace");
    
        label_child.op->next(t);
        if (!t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Wrong arguments in function fn:trace");
            
        tc = tuple_cell::make_sure_light_atomic(tc);
        fprintf(stderr, "TRACE: %s\n", tc.get_str_mem());
    }

    value_child.op->next(t);
    if (t.is_eos()) first_time = true;
}

PPIterator* PPFnTrace::copy(variable_context *_cxt_)
{
    PPFnTrace *res = new PPFnTrace(_cxt_, value_child, label_child);
    res->value_child.op = value_child.op->copy(_cxt_);
    res->label_child.op = label_child.op->copy(_cxt_);

    return res;
}

bool PPFnTrace::result(PPIterator* cur, variable_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnTrace::result");
}
