/*
 * File:  PPUnion.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPUnion.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPUnion::PPUnion(dynamic_context *_cxt_,
                 operation_info _info_,
                 PPOpIn _child1_,
                 PPOpIn _child2_,
                 bool _doc_order_) : PPIterator(_cxt_, _info_, "PPUnion"),
                                     child1(_child1_),
                                     child2(_child2_),
                                     doc_order(_doc_order_)
{
}

PPUnion::~PPUnion()
{
    delete child1.op;
    child1.op = NULL;
    delete child2.op;
    child2.op = NULL;
}

void PPUnion::do_open ()
{
    child1.op->open();
    child2.op->open();

    tug_first = true;
    tug_second = true;
}

void PPUnion::do_reopen()
{
    child1.op->reopen();
    child2.op->reopen();

    tug_first = true;
    tug_second = true;
}

void PPUnion::do_close()
{
    child1.op->close();
    child2.op->close();
}

void PPUnion::do_next (tuple &t)
{
    if (tug_first)
    {
        child1.op->next(t);
        if (t.is_eos())
        {
            xptr1 = XNULL;
        }
        else
        {
            if (!child1.get(t).is_node()) 
                throw XQUERY_EXCEPTION2(XPTY0004, "First argument of union operation contains item which is not a node");
            xptr1 = child1.get(t).get_node();
        }

        tug_first = false;
    }

    if (tug_second)
    {
        child2.op->next(t);
        if (t.is_eos())
        {
            xptr2 = XNULL;
        }
        else
        {
            if (!child2.get(t).is_node()) 
                throw XQUERY_EXCEPTION2(XPTY0004, "Second argument of union operation contains item which is not a node");
            xptr2 = child2.get(t).get_node();
        }

        tug_second = false;
    }

    /* XNULL by definition is > of any xptr (except itself); */
    switch (doc_order ? doc_order_merge_cmp(&xptr1, &xptr2) : xptr_compare(xptr1, xptr2))
    {
        case -1: /// 1 < 2
        {
            tug_first = true;
            t.copy(tuple_cell::node(xptr1));
            return;
        }
        case  0: /// 1 == 2
        {
            if (xptr1 == XNULL)
                t.set_eos();
            else
                t.copy(tuple_cell::node(xptr1));

            tug_first = true;
            tug_second = true;

            return;
        }
        case  1: /// 1 > 2
        {
            tug_second = true;
            t.copy(tuple_cell::node(xptr2));
            return;
        }
        default: throw USER_EXCEPTION2(SE1003, "Impossible case in PPUnion::next");
    }
}

PPIterator* PPUnion::do_copy(dynamic_context *_cxt_)
{
    PPUnion *res = new PPUnion(_cxt_, info, child1, child2, doc_order);
    res->child1.op = child1.op->copy(_cxt_);
    res->child2.op = child2.op->copy(_cxt_);
    return res;
}

void PPUnion::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child1.op->accept(v);
    child2.op->accept(v);    
    v.pop();
}
