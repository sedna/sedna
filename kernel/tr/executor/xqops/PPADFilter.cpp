/*
 * File:  PPADFilter.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPADFilter.h"
#include "merge.h"


PPADFilter::PPADFilter(variable_context *_cxt_,
                       PPOpIn _child1_,
                       PPOpIn _child2_) : PPIterator(_cxt_),
                                          child1(_child1_),
                                          child2(_child2_)
{
}

PPADFilter::~PPADFilter()
{
    delete child1.op;
    child1.op = NULL;
    delete child2.op;
    child2.op = NULL;
}

void PPADFilter::open  ()
{
    child1.op->open();
    child2.op->open();

    tug_first = true;
    tug_second = true;
}

void PPADFilter::reopen()
{
    child1.op->reopen();
    child2.op->reopen();

    tug_first = true;
    tug_second = true;
}

void PPADFilter::close ()
{
    child1.op->close();
    child2.op->close();
}

void PPADFilter::next  (tuple &t)
{
    while (true)
    {
        if (tug_first)
        {
            child1.op->next(t);
            if (t.is_eos())
            {
                tug_first = true;
                tug_second = true;

                child2.op->reopen();

                return;
            }
            else
            {
                if (!child1.get(t).is_node()) throw USER_EXCEPTION2(XP0006, "Argument of ancestor-descendant-filter is not a node");
                xptr1 = child1.get(t).get_node();
            }

            tug_first = false;
        }

        if (tug_second)
        {
            child2.op->next(t);
            if (t.is_eos())
            {
                tug_first = true;
                tug_second = true;

                child1.op->reopen();

                return;
            }
            else
            {
                if (!child2.get(t).is_node()) throw USER_EXCEPTION2(XP0006, "Argument of ancestor-descendant-filter is not a node");
                xptr2 = child2.get(t).get_node();
            }

            tug_second = false;
        }


        switch (nid_cmp_effective(xptr1, xptr2))
        {
            case -2: /// (1) ancestor (2)
            {
                tug_first = true;
                t.copy(tuple_cell::node(xptr1));
                return;
            }
            case -1: /// (1) < (2)
            {
                tug_first = true;
                break;
            }
            case  0: /// (1) == (2)
            {
                tug_first = true;
                tug_second = true;
                t.copy(tuple_cell::node(xptr1));
                return;
            }
            case  1: /// (1) > (2)
            {
                tug_second = true;
                break;
            }
            case  2: /// (1) descendant (2)
            {
                tug_second = true;
                break;
            }
            default: throw USER_EXCEPTION2(SE1003, "Impossible case in PPADFilter::next");
        }
    }
}

PPIterator* PPADFilter::copy(variable_context *_cxt_)
{
    PPADFilter *res = new PPADFilter(_cxt_, child1, child2);
    res->child1.op = child1.op->copy(_cxt_);
    res->child2.op = child2.op->copy(_cxt_);

    return res;
}

bool PPADFilter::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPADFilter::result");
}

