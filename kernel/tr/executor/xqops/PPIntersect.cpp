/*
 * File:  PPIntersect.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPIntersect.h"
#include "merge.h"


PPIntersect::PPIntersect(variable_context *_cxt_,
                         PPOpIn _child1_,
                         PPOpIn _child2_) : PPIterator(_cxt_),
                                            child1(_child1_),
                                            child2(_child2_)
{
}

PPIntersect::~PPIntersect()
{
    delete child1.op;
    child1.op = NULL;
    delete child2.op;
    child2.op = NULL;
}

void PPIntersect::open  ()
{
    child1.op->open();
    child2.op->open();

    tug_first = true;
    tug_second = true;
}

void PPIntersect::reopen()
{
    child1.op->reopen();
    child2.op->reopen();

    tug_first = true;
    tug_second = true;
}

void PPIntersect::close ()
{
    child1.op->close();
    child2.op->close();
}

void PPIntersect::next  (tuple &t)
{
    while (true)
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
                if (!child1.get(t).is_node()) throw USER_EXCEPTION2(XPTY0004, "Argument of intersect is not a node");
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
                if (!child2.get(t).is_node()) throw USER_EXCEPTION2(XPTY0004, "Argument of intersect is not a node");
                xptr2 = child2.get(t).get_node();
            }

            tug_second = false;
        }

        switch (doc_order_merge_cmp(&xptr1, &xptr2))
        {
            case -1: /// (1) < (2)
            {
                tug_first = true;
                break;
            }
            case  0: /// (1) == (2)
            {
                if (xptr1 == NULL)
                    t.set_eos();
                else
                    t.copy(tuple_cell::node(xptr1));

                tug_first = true;
                tug_second = true;

                return;
            }
            case  1: /// (1) > (2)
            {
                tug_second = true;
                break;
            }
            default: throw USER_EXCEPTION2(SE1003, "Impossible case in PPIntersect::next");
        }
    }
}

PPIterator* PPIntersect::copy(variable_context *_cxt_)
{
    PPIntersect *res = new PPIntersect(_cxt_, child1, child2);
    res->child1.op = child1.op->copy(_cxt_);
    res->child2.op = child2.op->copy(_cxt_);

    return res;
}

bool PPIntersect::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPIntersect::result");
}

