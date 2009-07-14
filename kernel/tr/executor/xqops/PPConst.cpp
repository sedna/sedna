/*
 * File:  PPConst.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPConst.h"
#include "tr/executor/base/PPUtils.h"
#include "common/utils.h"



PPConst::PPConst(dynamic_context *_cxt_,
                 const tuple_cell &_c_) : PPIterator(_cxt_)
{
    c = _c_;
}

PPConst::~PPConst()
{
    // nothing to do
}

void PPConst::open ()
{
    first_time = true;
}

void PPConst::reopen ()
{
    first_time = true;
}

void PPConst::close ()
{
    // nothing to do
}

void PPConst::next(tuple &t)
{
    SET_CURRENT_PP(this);

    if (first_time)
    {
        first_time = false;
        t.copy(c);
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }

    RESTORE_CURRENT_PP;
}

PPIterator* PPConst::copy(dynamic_context *_cxt_)
{
    PPConst *res = se_new PPConst(_cxt_, c);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPConst::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    r = se_new sequence(((PPConst*)cur)->c);
    return true;
}
