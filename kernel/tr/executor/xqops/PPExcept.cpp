/*
 * File:  PPExcept.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/xqops/PPExcept.h"
#include "tr/executor/base/merge.h"


PPExcept::PPExcept(dynamic_context *_cxt_,
                   PPOpIn _child1_,
                   PPOpIn _child2_) : PPIterator(_cxt_),
                                      child1(_child1_),
                                      child2(_child2_)
{
}

PPExcept::~PPExcept()
{
    delete child1.op;
    child1.op = NULL;
    delete child2.op;
    child2.op = NULL;
}

void PPExcept::open  ()
{
    child1.op->open();
    child2.op->open();

    tug_first = true;
    tug_second = true;
}

void PPExcept::reopen()
{
    child1.op->reopen();
    child2.op->reopen();

    tug_first = true;
    tug_second = true;
}

void PPExcept::close ()
{
    child1.op->close();
    child2.op->close();
}

void PPExcept::next  (tuple &t)
{
    SET_XQUERY_LINE(__xquery_line);
    
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
                if (!child1.get(t).is_node()) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of except is not a node");
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
                if (!child2.get(t).is_node()) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of except is not a node");
                xptr2 = child2.get(t).get_node();
            }
            tug_second = false;
        }

        switch (doc_order_merge_cmp(&xptr1, &xptr2))
        {
            case -1: /// (1) < (2)
            {
                tug_first = true;
                t.copy(tuple_cell::node(xptr1));
                UNDO_XQUERY_LINE; return;
            }
            case  0: /// (1) == (2)
            {
                tug_first = true;
                tug_second = true;

                if (xptr1 == NULL) 
                {
                    t.set_eos();
                    UNDO_XQUERY_LINE; return;
                }

                break;
            }
            case  1: /// (1) > (2)
            {
                tug_second = true;
                break;
            }
            default: throw USER_EXCEPTION2(SE1003, "Impossible case in PPExcept::next");
        }
    }

    UNDO_XQUERY_LINE;
}

PPIterator* PPExcept::copy(dynamic_context *_cxt_)
{
    PPExcept *res = se_new PPExcept(_cxt_, child1, child2);
    res->child1.op = child1.op->copy(_cxt_);
    res->child2.op = child2.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPExcept::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPExcept::result");
}

