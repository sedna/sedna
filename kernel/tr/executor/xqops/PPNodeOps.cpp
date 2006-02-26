/*
 * File:  PPNodeOps.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPNodeOps.h"
#include "dm_accessors.h"
#include "PPSLStub.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnName::PPFnName(variable_context *_cxt_,
                   PPOpIn _child_) : PPIterator(_cxt_),
                                     child(_child_)
{
}

PPFnName::~PPFnName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnName::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnName::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnName::close ()
{
    child.op->close();
}

void PPFnName::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
        {
            t.copy(EMPTY_STRING_TC);
            return;
        }

        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XP0006, "Argument of fn:name is not a node");

        tuple_cell tc = dm_node_name(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XP0006, "Argument of fn:name is not a node");

        if (tc.is_eos())
        {
            t.copy(EMPTY_STRING_TC);
        }
        else
        {
            tc.set_xtype(xs_string); // !!! dangerous
            t.copy(tc);
        }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnName::copy(variable_context *_cxt_)
{
    PPFnName *res = new PPFnName(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnName::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPFnName*)cur)->children(child);

    void *nn_r;
    bool nn_s = (child.op->res_fun())(child.op, cxt, nn_r);

    if (!nn_s) // if expression is not strict
    { // create PPFnName and transmit state
        child.op = (PPIterator*)nn_r;
        r = new PPFnName(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)nn_r;
    if (d_seq->size() == 0)
    {
        r = new sequence(EMPTY_STRING_TC);
        return true;
    }

    if (d_seq->size() != 1) throw USER_EXCEPTION2(XP0006, "Argument of fn:name is not a node");
    tuple_cell tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XP0006, "Argument of fn:name is not a node");

    tc.set_xtype(xs_string); // !!! dangerous
    r = new sequence(dm_node_name(tc.get_node()));
    return true;
}
