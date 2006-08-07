/*
 * File:  PPSequenceOps.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPSequenceOps.h"
#include "boolean_operations.h"
#include "casting_operations.h"
#include "PPSLStub.h"
#include "comparison_operations.h"
#include "PPUtils.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnEmpty
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnEmpty::PPFnEmpty(variable_context *_cxt_,
                     PPOpIn _child_) : PPIterator(_cxt_),
                                       child(_child_)
{
}

PPFnEmpty::~PPFnEmpty()
{
    delete child.op;
    child.op = NULL;
}

void PPFnEmpty::open  ()
{
    child.op->open();
    first_time = true;
    eos_reached = true;
}

void PPFnEmpty::reopen()
{
    child.op->reopen();
    first_time = true;
    eos_reached = true;
}

void PPFnEmpty::close ()
{
    child.op->close();
}

void PPFnEmpty::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        if (!eos_reached) child.op->reopen();

        child.op->next(t);

        if (t.is_eos()) { t.copy(fn_true()); eos_reached = true; }
        else { t.copy(fn_false()); eos_reached = false; }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnEmpty::copy(variable_context *_cxt_)
{
    PPFnEmpty *res = new PPFnEmpty(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnEmpty::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPFnEmpty*)cur)->children(child);

    void *em_r;
    bool em_s = (child.op->res_fun())(child.op, cxt, em_r);

    if (!em_s) // if expression is not strict
    { // create PPFnEmpty and transmit state
        child.op = (PPIterator*)em_r;
        r = new PPFnEmpty(cxt, child);
        return false;
    }

    if (((sequence*)em_r)->size() == 0)
        r = new sequence(fn_true());
    else
        r = new sequence(fn_false());

    return true;
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnExists
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnExists::PPFnExists(variable_context *_cxt_,
                       PPOpIn _child_) : PPIterator(_cxt_),
                                         child(_child_)
{
}

PPFnExists::~PPFnExists()
{
    delete child.op;
    child.op = NULL;
}

void PPFnExists::open  ()
{
    child.op->open();
    first_time = true;
    eos_reached = true;
}

void PPFnExists::reopen()
{
    child.op->reopen();
    first_time = true;
    eos_reached = true;
}

void PPFnExists::close ()
{
    child.op->close();
}

void PPFnExists::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        if (!eos_reached) child.op->reopen();

        child.op->next(t);

        if (t.is_eos()) { t.copy(fn_false()); eos_reached = true; }
        else { t.copy(fn_true()); eos_reached = false; }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnExists::copy(variable_context *_cxt_)
{
    PPFnExists *res = new PPFnExists(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnExists::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPFnExists*)cur)->children(child);

    void *ex_r;
    bool ex_s = (child.op->res_fun())(child.op, cxt, ex_r);

    if (!ex_s) // if expression is not strict
    { // create PPFnExists and transmit state
        child.op = (PPIterator*)ex_r;
        r = new PPFnExists(cxt, child);
        return false;
    }

    if (((sequence*)ex_r)->size() == 0)
        r = new sequence(fn_false());
    else
        r = new sequence(fn_true());

    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnItemAt
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnItemAt::PPFnItemAt(variable_context *_cxt_,
                       PPOpIn _seq_child_,
                       PPOpIn _pos_child_) : PPIterator(_cxt_),
                                             seq_child(_seq_child_),
                                             pos_child(_pos_child_)
{
}

PPFnItemAt::~PPFnItemAt()
{
    delete seq_child.op;
    seq_child.op = NULL;
    delete pos_child.op;
    pos_child.op = NULL;
}

void PPFnItemAt::open  ()
{
    seq_child.op->open();
    pos_child.op->open();
    first_time = true;
}

void PPFnItemAt::reopen()
{
    seq_child.op->reopen();
    pos_child.op->reopen();
    first_time = true;
}

void PPFnItemAt::close ()
{
    seq_child.op->close();
    pos_child.op->close();
}

void PPFnItemAt::next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        pos_child.op->next(t);
        if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Invalid argument to fn:item-at");

        int pos = cast_to_xs_integer(pos_child.get(t)).get_xs_integer();

        pos_child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Invalid argument to fn:item-at");
        if (pos < 1) throw USER_EXCEPTION(SE1007);

        for (int i = 1; i <= pos; i++)
        {
            seq_child.op->next(t);
            if (t.is_eos())
            {
                if (i == 1)
                {
                    t.set_eos();
                    first_time = true;
                    return;
                }

                throw USER_EXCEPTION(SE1007);
            }
        }
    }
    else
    {
        first_time = true;
        seq_child.op->reopen();
        t.set_eos();
    }
}

PPIterator* PPFnItemAt::copy(variable_context *_cxt_)
{
    PPFnItemAt *res = new PPFnItemAt(_cxt_, seq_child, pos_child);
    res->seq_child.op = seq_child.op->copy(_cxt_);
    res->pos_child.op = pos_child.op->copy(_cxt_);

    return res;
}

bool PPFnItemAt::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn seq_child, pos_child;
    ((PPFnItemAt*)cur)->children(seq_child, pos_child);

    void *pos_r;
    bool pos_s = (pos_child.op->res_fun())(pos_child.op, cxt, pos_r);

    if (!pos_s) // if expression is not strict
    { // create PPFnItemAt and transmit state
        seq_child.op = seq_child.op->copy(cxt);
        pos_child.op = (PPIterator*)pos_r;
        r = new PPFnItemAt(cxt, seq_child, pos_child);
        return false;
    }

    void *seq_r;
    bool seq_s = (seq_child.op->res_fun())(seq_child.op, cxt, seq_r);

    if (!seq_s) // if expression is not strict
    { // create PPFnItemAt and transmit state
        seq_child.op = (PPIterator*)seq_r;
        pos_child.op = new PPSLStub(cxt, pos_child.op->copy(cxt), (sequence*)pos_r);
        r = new PPFnItemAt(cxt, seq_child, pos_child);
        return false;
    }

    sequence* pos_seq = (sequence*)pos_r;
    if (pos_seq->size() != 1) throw USER_EXCEPTION2(XPTY0004, "Invalid argument to fn:item-at");

    int pos = cast_to_xs_integer(pos_seq->get_00()).get_xs_integer();

    if (pos < 1) throw USER_EXCEPTION(SE1007);

    sequence* seq_seq = (sequence*)seq_r;
    r = new sequence(seq_child.ts);
    if (seq_seq->size() == 0) return true;
    if (seq_seq->size() < pos) throw USER_EXCEPTION(SE1007);

    ((sequence*)r)->add((*seq_seq)[pos - 1]);
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnDistinctValues
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnDistinctValues::PPFnDistinctValues(variable_context *_cxt_,
                                       PPOpIn _child_) : PPIterator(_cxt_),
                                                         child(_child_),
                                                         stored_tuple(1)
{
}

PPFnDistinctValues::~PPFnDistinctValues()
{
    delete child.op;
    child.op = NULL;
}

void PPFnDistinctValues::open  ()
{
    child.op->open();

    s = new sequence(1);
}

void PPFnDistinctValues::reopen()
{
    child.op->reopen();

    s->clear();
}

void PPFnDistinctValues::close ()
{
    child.op->close();

    delete s;
    s = NULL;
}

void PPFnDistinctValues::next(tuple &t)
{
    while (true)
    {
        child.op->next(t);

        if (t.is_eos())
        {
            s->clear();
            return;
        }

        tuple_cell tc = atomize(child.get(t));

        int pos = 0;
        for (pos = 0; pos < s->size(); ++pos)
        {
            s->get(stored_tuple, pos);
            try {
                tuple_cell comp_res = value_comp_eq(tc, stored_tuple.cells[0]);
                if (comp_res.get_xs_boolean()) break;
            } catch (SednaUserException &e) {
                // continue cycle
            }
        }

        if (pos == s->size())
        {
            stored_tuple.copy(tc);
            s->add(stored_tuple);

            t.copy(tc);
            return;
        }
    }
}

PPIterator* PPFnDistinctValues::copy(variable_context *_cxt_)
{
    PPFnDistinctValues *res = new PPFnDistinctValues(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnDistinctValues::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPFnDistinctValues*)cur)->children(child);

    void *dv_r;
    bool dv_s = (child.op->res_fun())(child.op, cxt, dv_r);

    if (!dv_s) // if expression is not strict
    { // create PPFnExists and transmit state
        child.op = (PPIterator*)dv_r;
        r = new PPFnDistinctValues(cxt, child);
        return false;
    }

    sequence *data_seq = (sequence*)dv_r;
    sequence *res_seq = new sequence(1);
    tuple t(1);

    for (int data_pos = 0; data_pos < data_seq->size(); ++data_pos)
    {
        data_seq->get(t, data_pos);
        tuple_cell tc = atomize(t.cells[0]);

        int res_pos = 0;
        for (res_pos = 0; res_pos < res_seq->size(); ++res_pos)
        {
            res_seq->get(t, res_pos);
            try {
                tuple_cell comp_res = value_comp_eq(tc, t.cells[0]);
                if (comp_res.get_xs_boolean()) break;
            } catch (SednaUserException &e) {
                // continue cycle
            }
        }

        if (res_pos == res_seq->size())
        {
            t.copy(tc);
            res_seq->add(t);
        }
    }

    r = res_seq;
    return true;
}

