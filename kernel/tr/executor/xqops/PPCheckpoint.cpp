/*
 * File:  PPCheckpoint.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "PPCheckpoint.h"

PPCheckpoint::PPCheckpoint(variable_context *_cxt_) : PPIterator(_cxt_)
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
    // call checkpoint here
}

PPIterator* PPCheckpoint::copy(variable_context *_cxt_)
{
    PPCheckpoint *res = new PPCheckpoint(_cxt_);
    return res;
}

bool PPCheckpoint::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPCheckpoint::result");
}
