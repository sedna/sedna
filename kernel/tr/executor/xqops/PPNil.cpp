/*
 * File:  PPNil.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPNil.h"

PPNil::PPNil(dynamic_context *_cxt_) : PPIterator(_cxt_)
{
}

PPNil::~PPNil()
{
}

void PPNil::open ()
{
}

void PPNil::reopen ()
{
}

void PPNil::close ()
{
}

void PPNil::next(tuple &t)
{
    t.set_eos();
}

PPIterator* PPNil::copy(dynamic_context *_cxt_)
{
    PPNil *res = new PPNil(_cxt_);
    return res;
}

bool PPNil::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    r = new sequence(1);
    return true;
}
