/*
 * File:  PPUp.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPUp.h"
#include "tr/executor/base/dm_accessors.h"


PPUp::PPUp(dynamic_context *_cxt_,
           PPOpIn _child_,
           schema_node *_scm_node_) : PPIterator(_cxt_),
                                      child(_child_),
                                      scm_node(_scm_node_)
{
}

PPUp::~PPUp()
{
    delete child.op;
    child.op = NULL;
}

void PPUp::open  ()
{
    previous = XNULL;
    child.op->open();
}

void PPUp::reopen()
{
    previous = XNULL;
    child.op->reopen();
}

void PPUp::close ()
{
    child.op->close();
}

void PPUp::next  (tuple &t)
{
    SET_XQUERY_LINE(__xquery_line);
    
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) 
        {
            previous = XNULL;
            {UNDO_XQUERY_LINE; return;}
        }
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of PPUp is not a node");

        xptr p = child.get(t).get_node();
                                   
        CHECKP(p);
        while (GETSCHEMENODEX(p) != scm_node)
        {
		    p = get_parent_node(p);
            if (p == NULL) throw USER_EXCEPTION2(SE1003, "NULL xptr in PPUp::next");
            CHECKP(p);
        }

        if (p != previous)
        {
            previous = p;
            t.copy(tuple_cell::node(p));
            {UNDO_XQUERY_LINE; return;}
        }
    }

    UNDO_XQUERY_LINE;
}

PPIterator* PPUp::copy(dynamic_context *_cxt_)
{
    PPUp *res = se_new PPUp(_cxt_, child, scm_node);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPUp::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPUp::result");
}

