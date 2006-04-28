/*
 * File:  PPInstanceOf.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPInstanceOf.h"


PPInstanceOf::PPInstanceOf(variable_context *_cxt_,
                           PPOpIn _child_,
                           const sequence_type& _st_) : PPIterator(_cxt_),
                                                        child(_child_),
                                                        st(_st_)
{
}

PPInstanceOf::~PPInstanceOf()
{
    delete child.op;
    child.op = NULL;
}

void PPInstanceOf::open  ()
{
    child.op->open();
    first_time = true;
    eos_reached = true;
}

void PPInstanceOf::reopen()
{
    child.op->reopen();
    first_time = true;
    eos_reached = true;
}

void PPInstanceOf::close ()
{
    child.op->close();
}

bool type_matches(const PPOpIn &child, tuple &t, bool &eos_reached, const sequence_type& st);

void PPInstanceOf::next  (tuple &t)
{
    if (first_time)
    {
		first_time = false;

        if (!eos_reached) child.op->reopen();

        bool res = type_matches(child, t, eos_reached, st);

        t.copy(tuple_cell::atomic(res));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPInstanceOf::copy(variable_context *_cxt_)
{
    PPInstanceOf *res = new PPInstanceOf(_cxt_, child, st);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPInstanceOf::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPInstanceOf::result");
}
