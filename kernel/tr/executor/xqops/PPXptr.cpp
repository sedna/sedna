/*
 * File:  PPXptr.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "PPXptr.h"

PPXptr::PPXptr(variable_context *_cxt_, const xptr &_p_) : PPIterator(_cxt_), p(_p_)
{
}

PPXptr::~PPXptr()
{
}

void PPXptr::open ()
{
    first_time = true;
}

void PPXptr::reopen ()
{
    first_time = true;
}

void PPXptr::close ()
{
    // nothing to do
}

void PPXptr::next (tuple &t)
{
    if (first_time)
    {
        first_time = false;
        t.copy(tuple_cell::node(p));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPXptr::copy(variable_context *_cxt_)
{
    PPXptr *res = new PPXptr(_cxt_, p);
    return res;
}

bool PPXptr::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPXptr::result");
}
