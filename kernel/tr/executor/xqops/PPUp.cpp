/*
 * File:  PPUp.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPUp.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPUp::PPUp(dynamic_context *_cxt_,
           operation_info _info_,
           PPOpIn _child_,
           schema_node_xptr _scm_node_) : PPIterator(_cxt_,_info_),
                                          child(_child_),
                                          scm_node(_scm_node_)
{
}

PPUp::~PPUp()
{
    delete child.op;
    child.op = NULL;
}

void PPUp::do_open ()
{
    previous = XNULL;
    child.op->open();
}

void PPUp::do_reopen()
{
    previous = XNULL;
    child.op->reopen();
}

void PPUp::do_close()
{
    child.op->close();
}

void PPUp::do_next (tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) 
        {
            previous = XNULL;
            return;
        }
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of PPUp is not a node");

        xptr p = child.get(t).get_node();
                                   
        CHECKP(p);
        while (GETSCHEMENODEX(p) != scm_node)
        {
		    p = get_parent_node(p);
            if (p == XNULL) throw USER_EXCEPTION2(SE1003, "NULL xptr in PPUp::next");
            CHECKP(p);
        }

        if (p != previous)
        {
            previous = p;
            t.copy(tuple_cell::node(p));
            return;
        }
    }
}

PPIterator* PPUp::do_copy(dynamic_context *_cxt_)
{
    PPUp *res = se_new PPUp(_cxt_, info, child, scm_node);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPUp::do_accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    child.op->accept(v);
    v.pop();
}
