/*
 * File:  PPIf.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/xqops/PPIf.h"
#include "tr/executor/base/PPUtils.h"


PPIf::PPIf(dynamic_context *_cxt_,
           PPOpIn _if_child_, 
           PPOpIn _then_child_, 
           PPOpIn _else_child_) : PPIterator(_cxt_),
                                  if_child(_if_child_),
                                  then_child(_then_child_),
                                  else_child(_else_child_),
                                  if_data(_if_child_.ts)
{
    data_child = NULL;
    if (then_child.ts != else_child.ts) throw USER_EXCEPTION2(SE1003, "Children of PPif operation have different tuple sizes");

}

PPIf::PPIf(dynamic_context *_cxt_,
           PPOpIn _if_child_, 
           PPOpIn _then_child_, 
           PPOpIn _else_child_,
           PPIterator* _data_child_) : PPIterator(_cxt_),
                                       if_child(_if_child_),
                                       then_child(_then_child_),
                                       else_child(_else_child_),
                                       if_data(_if_child_.ts),
                                       data_child(_data_child_)
{
    eos_reached = true;
    if (then_child.ts != else_child.ts) throw USER_EXCEPTION2(SE1003, "Children of PPif operation have different tuple sizes");
}


PPIf::~PPIf()
{
    delete if_child.op;
    if_child.op = NULL;
    delete then_child.op;
    then_child.op = NULL;
    delete else_child.op;
    else_child.op = NULL;
}

void PPIf::open ()
{
    if_child.op->open();
    then_child.op->open();
    else_child.op->open();

    data_child = NULL;
    eos_reached = false;
}

void PPIf::reopen ()
{
    if (data_child != NULL)
    {
        if (!eos_reached) if_child.op->reopen();
        data_child->reopen();

        data_child = NULL;
    }
}

void PPIf::close ()
{
    if_child.op->close();
    then_child.op->close();
    else_child.op->close();
}

void PPIf::next(tuple &t)
{
    if (data_child == NULL)
    {
        eos_reached = true;
        tuple_cell c = effective_boolean_value(if_child, if_data, eos_reached);

        if (c.get_xs_boolean()) data_child = then_child.op;
        else data_child = else_child.op;
    }

    data_child->next(t);

    if (t.is_eos())
    {
        data_child = NULL;
        if (!eos_reached) if_child.op->reopen();
    }
}

PPIterator* PPIf::copy(dynamic_context *_cxt_)
{
    PPIf *res = new PPIf(_cxt_, if_child, then_child, else_child);
    res->if_child.op   = if_child.op->copy(_cxt_);
    res->then_child.op = then_child.op->copy(_cxt_);
    res->else_child.op = else_child.op->copy(_cxt_);

    return res;
}

bool PPIf::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn if_child, then_child, else_child;
    ((PPIf*)cur)->children(if_child, then_child, else_child);

    void *if_r;
    bool if_s = (if_child.op->res_fun())(if_child.op, cxt, if_r);

    if (!if_s) // if expression is not strict
    { // create PPIf and transmit state
        if_child.op = (PPIterator*)if_r;
        then_child.op = then_child.op->copy(cxt);
        else_child.op = else_child.op->copy(cxt);
        PPIf *res_op = new PPIf(cxt, if_child, then_child, else_child);

        r = res_op;
        return false;
    }

    tuple_cell c = effective_boolean_value((sequence*)if_r);
    delete ((sequence*)if_r);

    void *data_r;
    bool data_s;
    bool if_cnd = c.get_xs_boolean();
    if (if_cnd) data_s = (then_child.op->res_fun())(then_child.op, cxt, data_r);
    else        data_s = (else_child.op->res_fun())(else_child.op, cxt, data_r);

    if (!data_s)
    { // create PPIf and transmit state
        if_child.op = if_child.op->copy(cxt);
        PPIterator* data_child;
        if (if_cnd)
        {
            then_child.op = (PPIterator*)data_r;
            else_child.op = else_child.op->copy(cxt);
            data_child = then_child.op;
        }
        else
        {
            then_child.op = then_child.op->copy(cxt);
            else_child.op = (PPIterator*)data_r;
            data_child = else_child.op;
        }
        PPIf *res_op = new PPIf(cxt, if_child, then_child, else_child, data_child);

        r = res_op;
        return false;
    }

    r = data_r;
    return true;
}

