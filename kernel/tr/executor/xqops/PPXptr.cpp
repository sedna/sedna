/*
 * File:  PPXptr.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "PPXptr.h"

PPXptr::PPXptr(dynamic_context *_cxt_, trigger_parameter_type _var_type_) : PPIterator(_cxt_), var_type(_var_type_)
{
}

PPXptr::PPXptr(dynamic_context *_cxt_, trigger_parameter_type _var_type_, const xptr &_p_) : PPIterator(_cxt_), 
                                                                                      var_type(_var_type_),
                                                                                      p(_p_)
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

PPIterator* PPXptr::copy(dynamic_context *_cxt_)
{
    PPXptr *res = new PPXptr(_cxt_, var_type, p);
    return res;
}

bool PPXptr::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPXptr::result");
}
