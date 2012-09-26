/*
 * File:  PPXptr.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPXptr.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPXptr::PPXptr(dynamic_context *_cxt_,
               operation_info _info_,
               trigger_parameter_type _var_type_) : PPIterator(_cxt_, _info_, "PPXptr"), 
                                                    var_type(_var_type_)
{
}

PPXptr::PPXptr(dynamic_context *_cxt_,
               operation_info _info_,
               trigger_parameter_type _var_type_,
               const xptr &_p_) : PPIterator(_cxt_, _info_, "PPXptr"),
                                  p(_p_),
                                  var_type(_var_type_)
{
}

PPXptr::~PPXptr()
{
}

void PPXptr::do_open ()
{
    first_time = true;
}

void PPXptr::do_reopen()
{
    first_time = true;
}

void PPXptr::do_close()
{
    // nothing to do
}

void PPXptr::do_next (tuple &t)
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

PPIterator* PPXptr::do_copy(dynamic_context *_cxt_)
{
    PPXptr *res = new PPXptr(_cxt_, info, var_type, p);
    return res;
}

void PPXptr::do_accept(PPVisitor &v)
{
    v.visit (this);
}
