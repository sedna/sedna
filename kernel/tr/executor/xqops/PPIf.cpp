/*
 * File:  PPIf.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/xqops/PPIf.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPIf::PPIf(dynamic_context *_cxt_,
           operation_info _info_,
           PPOpIn _if_child_, 
           PPOpIn _then_child_, 
           PPOpIn _else_child_) : PPIterator(_cxt_, _info_, "PPIf"),
                                  if_child(_if_child_),
                                  then_child(_then_child_),
                                  else_child(_else_child_),
                                  if_data(_if_child_.ts)
{
    data_child = NULL;
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

void PPIf::do_open ()
{
    if_child.op->open();
    then_child.op->open();
    else_child.op->open();

    data_child = NULL;
    eos_reached = false;
}

void PPIf::do_reopen()
{
    if (data_child != NULL)
    {
        if (!eos_reached) if_child.op->reopen();
        data_child->reopen();
        data_child = NULL;
    }
}

void PPIf::do_close()
{
    if_child.op->close();
    then_child.op->close();
    else_child.op->close();
}

void PPIf::do_next(tuple &t)
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

PPIterator* PPIf::do_copy(dynamic_context *_cxt_)
{
    PPIf *res = se_new PPIf(_cxt_, info, if_child, then_child, else_child);
    res->if_child.op   = if_child.op->copy(_cxt_);
    res->then_child.op = then_child.op->copy(_cxt_);
    res->else_child.op = else_child.op->copy(_cxt_);
    return res;
}

void PPIf::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    if_child.op->accept(v);
    then_child.op->accept(v);
    else_child.op->accept(v);
    v.pop();
}
