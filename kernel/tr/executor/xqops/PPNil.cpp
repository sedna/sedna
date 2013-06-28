/*
 * File:  PPNil.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPNil.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPNil::PPNil(dynamic_context *_cxt_,
             operation_info _info_) : PPIterator(_cxt_, _info_, "PPNil")
{
}

PPNil::~PPNil()
{
}

void PPNil::do_open ()
{
}

void PPNil::do_reopen()
{
}

void PPNil::do_close()
{
}

void PPNil::do_next(xqp_tuple &t)
{
    t.set_eos();
}

PPIterator* PPNil::do_copy(dynamic_context *_cxt_)
{
    PPNil *res = se_new PPNil(_cxt_, info);
    return res;
}

void PPNil::do_accept(PPVisitor &v)
{
    v.visit (this);
}
