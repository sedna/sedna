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
    id = cxt->var_cxt.producers[dsc].op->register_consumer(dsc);
}

void PPVariable::do_reopen()
{
    cxt->var_cxt.producers[dsc].op->reopen(dsc, id);
}

void PPVariable::do_close()
{
    // nothing to do
}

void PPVariable::do_next (tuple &t)
{
    cxt->var_cxt.producers[dsc].op->next(t, dsc, id);
}

PPIterator* PPVariable::do_copy(dynamic_context *_cxt_)
{
    PPVariable *res = se_new PPVariable(_cxt_, info, dsc);
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
                                   var_dsc _dsc_) : PPIterator(_cxt_, _info_, "PPGlobalVariable"), 
                                                    dsc(_dsc_)
{
}

PPGlobalVariable::~PPGlobalVariable()
{
    // nothing to do
}

void PPGlobalVariable::setVarId(var_dsc _dsc_)
{
    U_ASSERT(dsc == -1); // this function should be called only for previously unresolved vars

    dsc = _dsc_;
}

void PPGlobalVariable::do_open ()
{
    U_ASSERT(cxt);
    id = dynamic_context::glb_var_cxt.producers[dsc].op->register_consumer(dsc);
}

void PPGlobalVariable::do_reopen()
{
    dynamic_context::glb_var_cxt.producers[dsc].op->reopen(dsc, id);
}

void PPGlobalVariable::do_close()
{
    dynamic_context::glb_var_cxt.producers[dsc].op->close(dsc, id);
}

void PPGlobalVariable::do_next (tuple &t)
{
    dynamic_context::glb_var_cxt.producers[dsc].op->next(t, dsc, id);
}

PPIterator* PPGlobalVariable::do_copy(dynamic_context *_cxt_)
{
    PPGlobalVariable *res = se_new PPGlobalVariable(_cxt_, info, dsc);
    return res;
}

void PPGlobalVariable::do_accept(PPVisitor &v)
{
    v.visit (this);
}
