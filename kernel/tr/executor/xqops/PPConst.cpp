/*
 * File:  PPConst.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPConst.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "auxiliary/utils.h"



PPConst::PPConst(dynamic_context *_cxt_,
                 operation_info _info_,
                 const tuple_cell &_c_) : PPIterator(_cxt_, _info_, "PPConst")
{
    c = _c_;
}

PPConst::~PPConst()
{
    // nothing to do
}

void PPConst::do_open ()
{
    first_time = true;
}

void PPConst::do_reopen()
{
    first_time = true;
}

void PPConst::do_close()
{
    // nothing to do
}

void PPConst::do_accept(PPVisitor &v)
{
    v.visit (this);
}

void PPConst::do_next(tuple &t)
{
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
}

PPIterator* PPConst::do_copy(dynamic_context *_cxt_)
{
    PPConst *res = new PPConst(_cxt_, info, c);
    return res;
}
