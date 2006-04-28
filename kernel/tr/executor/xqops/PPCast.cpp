/*
 * File:  PPCast.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPCast.h"
#include "PPUtils.h"
#include "casting_operations.h"


PPCast::PPCast(variable_context *_cxt_,
               PPOpIn _child_,
               xmlscm_type _target_type_,
               bool _can_be_empty_seq_) : PPIterator(_cxt_),
                                          child(_child_),
                                          target_type(_target_type_),
                                          can_be_empty_seq(_can_be_empty_seq_)
{
}

PPCast::~PPCast()
{
    delete child.op;
    child.op = NULL;
}

void PPCast::open  ()
{
    child.op->open();
    first_time = true;
}

void PPCast::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPCast::close ()
{
    child.op->close();
}

void PPCast::next  (tuple &t)
{
    if (first_time)
    {
		first_time = false;
        child.op->next(t);

        if (t.is_eos()) 
            if (can_be_empty_seq)
            {
                first_time = true;
                t.set_eos();
                return;
            }
            else throw USER_EXCEPTION2(XP0006, "cast expression ('?' is not specified in target type but empty sequence is given)");

        tuple_cell tc = atomize(child.get(t));

        child.op->next(t);
        if (!t.is_eos()) throw USER_EXCEPTION2(XP0006, "cast expression (the result of atomization is a sequence of more than one atomic value)");

        t.copy(cast(tc, target_type));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPCast::copy(variable_context *_cxt_)
{
    PPCast *res = new PPCast(_cxt_, child, target_type, can_be_empty_seq);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPCast::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPCast*)cur)->children(child);

    void *child_r;
    bool child_s = (child.op->res_fun())(child.op, cxt, child_r);

    if (!child_s) // if expression is not strict
    { // create PPCast and transmit state
        child.op = (PPIterator*)child_r;
        PPCast *res_op = new PPCast(cxt, child, 
                                    ((PPCast*)cur)->target_type, 
                                    ((PPCast*)cur)->can_be_empty_seq);

        r = res_op;
        return false;
    }

    sequence *child_seq = (sequence*)child_r;
    if (child_seq->size() == 0)
        if (((PPCast*)cur)->can_be_empty_seq)
        {
            r = child_seq;
            return true;
        }
        else throw USER_EXCEPTION2(XP0006, "cast expression ('?' is not specified in target type but empty sequence is given)");

    if (child_seq->size() != 1) throw USER_EXCEPTION2(XP0006, "cast expression (the result of atomization is a sequence of more than one atomic value)");

    tuple t(1);
    child_seq->get(t, child_seq->begin());
    t.cells[0] = cast(atomize(t.cells[0]), ((PPCast*)cur)->target_type);
    child_seq->clear();
    child_seq->add(t);

    r = child_seq;
    return true;
}

