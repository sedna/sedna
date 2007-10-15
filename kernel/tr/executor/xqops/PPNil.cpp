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
    SET_XQUERY_LINE(__xquery_line);
    t.set_eos();
    UNDO_XQUERY_LINE;
}

PPIterator* PPNil::copy(dynamic_context *_cxt_)
{
    PPNil *res = se_new PPNil(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPNil::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    r = se_new sequence(1);
    return true;
}
