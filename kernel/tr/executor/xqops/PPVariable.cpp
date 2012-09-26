/*
 * File:  PPVariable.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPVariable.h"
#include "tr/executor/base/visitor/PPVisitor.h"

///////////////////////////////////////////////////////////////////////////////
/// PPVariable
///////////////////////////////////////////////////////////////////////////////
PPVariable::PPVariable(dynamic_context *_cxt_,
                       operation_info _info_, 
                       var_dsc _dsc_) : PPIterator(_cxt_, _info_, "PPVariable"),
                                        var_prod(NULL),
                                        dsc(_dsc_)
{
}

PPVariable::~PPVariable()
{
    // nothing to do
}

void PPVariable::do_open ()
{
    U_ASSERT(cxt);

    var_prod = &(cxt->get_var_producer(dsc, var_cxt));

    id = var_prod->op->register_consumer(dsc);
}

void PPVariable::do_reopen()
{
    var_prod->op->reopen(dsc, id);
}

void PPVariable::do_close()
{
    // nothing to do
}

void PPVariable::do_next (tuple &t)
{
    var_prod->op->next(t, dsc, id);
}

PPIterator* PPVariable::do_copy(dynamic_context *_cxt_)
{
    PPVariable *res = new PPVariable(_cxt_, info, dsc);

    return res;
}

void PPVariable::do_accept(PPVisitor &v)
{
    v.visit (this);
}

///////////////////////////////////////////////////////////////////////////////
/// PPGlobalVariable
///////////////////////////////////////////////////////////////////////////////
PPGlobalVariable::PPGlobalVariable(dynamic_context *_cxt_,
                                   operation_info _info_, 
                                   global_var_dsc _dsc_) : PPIterator(_cxt_, _info_, "PPGlobalVariable"),
                                                    dsc(_dsc_)
{
}

PPGlobalVariable::~PPGlobalVariable()
{
    // nothing to do
}

void PPGlobalVariable::do_open ()
{
    U_ASSERT(cxt);
    id = dsc.first->get_global_var_producer(dsc.second).op->register_consumer(dsc.second);
}

void PPGlobalVariable::do_reopen()
{
    dsc.first->get_global_var_producer(dsc.second).op->reopen(dsc.second, id);
}

void PPGlobalVariable::do_close()
{
    dsc.first->get_global_var_producer(dsc.second).op->close(dsc.second, id);
}

void PPGlobalVariable::do_next (tuple &t)
{
    dsc.first->get_global_var_producer(dsc.second).op->next(t, dsc.second, id);
}

PPIterator* PPGlobalVariable::do_copy(dynamic_context *_cxt_)
{
    PPGlobalVariable *res = new PPGlobalVariable(_cxt_, info, dsc);
    return res;
}

void PPGlobalVariable::do_accept(PPVisitor &v)
{
    v.visit (this);
}
