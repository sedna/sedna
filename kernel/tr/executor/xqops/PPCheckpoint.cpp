/*
 * File:  PPCheckpoint.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPCheckpoint.h"
#include "tr/log/log.h"

PPCheckpoint::PPCheckpoint(dynamic_context *_cxt_) : PPIterator(_cxt_)
{
}

PPCheckpoint::~PPCheckpoint()
{
    // nothing to do
}

void PPCheckpoint::open ()
{
    // nothing to do
}

void PPCheckpoint::reopen ()
{
    // nothing to do
}

void PPCheckpoint::close ()
{
    // nothing to do
}

void PPCheckpoint::next (tuple &t)
{
    t.set_eos();
    activate_and_wait_for_end_checkpoint();
    // call checkpoint here
}

PPIterator* PPCheckpoint::copy(dynamic_context *_cxt_)
{
    PPCheckpoint *res = se_new PPCheckpoint(_cxt_);
    return res;
}

bool PPCheckpoint::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPCheckpoint::result");
}
