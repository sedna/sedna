/*
 * File:  PPUnion.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPUnion.h"
#include "tr/executor/base/merge.h"


PPUnion::PPUnion(dynamic_context *_cxt_,
                 PPOpIn _child1_,
                 PPOpIn _child2_) : PPIterator(_cxt_),
                                    child1(_child1_),
                                    child2(_child2_)
{
}

PPUnion::~PPUnion()
{
    delete child1.op;
    child1.op = NULL;
    delete child2.op;
    child2.op = NULL;
}

void PPUnion::open  ()
{
    child1.op->open();
    child2.op->open();

    tug_first = true;
    tug_second = true;
}

void PPUnion::reopen()
{
    child1.op->reopen();
    child2.op->reopen();

    tug_first = true;
    tug_second = true;
}

void PPUnion::close ()
{
    child1.op->close();
    child2.op->close();
}

void PPUnion::next  (tuple &t)
{
    SET_CURRENT_PP(this);
    
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

    /// XNULL by definition is equal to any xptr with addr == NULL;
    /// XNULL by definition is > of any xptr (except itself);
    switch (xptr_compare(xptr1, xptr2))
    {
        case -1: /// 1 < 2
        {
            tug_first = true;
            t.copy(tuple_cell::node(xptr1));
            {RESTORE_CURRENT_PP; return;}
        }
        case  0: /// 1 == 2
        {
            if (xptr1 == XNULL)
                t.set_eos();
            else
                t.copy(tuple_cell::node(xptr1));

            tug_first = true;
            tug_second = true;

            {RESTORE_CURRENT_PP; return;}
        }
        case  1: /// 1 > 2
        {
            tug_second = true;
            t.copy(tuple_cell::node(xptr2));
            {RESTORE_CURRENT_PP; return;}
        }
        default: throw USER_EXCEPTION2(SE1003, "Impossible case in PPUnion::next");
    }

    RESTORE_CURRENT_PP;
}

PPIterator* PPUnion::copy(dynamic_context *_cxt_)
{
    PPUnion *res = se_new PPUnion(_cxt_, child1, child2);
    res->child1.op = child1.op->copy(_cxt_);
    res->child2.op = child2.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPUnion::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPUnion::result");
}

