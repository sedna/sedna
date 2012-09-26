/*
 * File:  PPFnGetProperty.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"

#include "tr/executor/xqops/PPFnGetProperty.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/tr_globals.h"
#include "tr/executor/base/visitor/PPVisitor.h"

PPFnGetProperty::PPFnGetProperty(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnGetProperty"),
                                                   child(_child_)
{
}

PPFnGetProperty::~PPFnGetProperty()
{
    delete child.op;
    child.op = NULL;
}

void PPFnGetProperty::do_open ()
{
    child.op->open();
    first_time = true;
}


void PPFnGetProperty::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnGetProperty::do_close()
{
    child.op->close();
}

void PPFnGetProperty::do_next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        tuple_cell property;

        child.op->next(t);
        if (t.is_eos()) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the first argument of the se:get-property function. Argument contains zero items.");
        property = atomize(child.get(t));
        if(!is_string_type(property.get_atomic_type())) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the separator of the se:get-property function (xs_string/derived/promotable is expected).");
        child.op->next(t);
        if  (!t.is_eos()) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the first argument of the se:get-property function. Argument contains more than one item.");

        property = tuple_cell::make_sure_light_atomic(property);

        if(0 == strcmp(property.get_str_mem(), "$user"))
            t.copy(tuple_cell::atomic_deep(xs_string, tr_globals::login));
        else
            throw XQUERY_EXCEPTION2(SE4621, property.get_str_mem());
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnGetProperty::do_copy(dynamic_context *_cxt_)
{
    PPFnGetProperty *res = new PPFnGetProperty(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnGetProperty::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}
