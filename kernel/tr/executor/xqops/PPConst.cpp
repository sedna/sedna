/*
 * File:  PPConst.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPConst.h"
#include "PPUtils.h"
#include "utils.h"



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
/***********************************
#define ALLOC_SIZE	1024
#define ALLOC_ATTEMPTS	(1024 * 12)
    int i = 0;
    void *p = NULL;
    for (i = 0; i < ALLOC_ATTEMPTS; ++i)
    {
        p = pers_malloc(ALLOC_SIZE);
        if (!p) 
            throw USER_EXCEPTION2(FODC0005, "Not enough memory");
    }

/***********************************/
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

PPIterator* PPConst::copy(dynamic_context *_cxt_)
{
    PPConst *res = new PPConst(_cxt_, c);
    return res;
}

bool PPConst::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    r = new sequence(((PPConst*)cur)->c);
    return true;
}
