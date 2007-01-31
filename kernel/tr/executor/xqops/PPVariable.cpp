/*
 * File:  PPVariable.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPVariable.h"

///////////////////////////////////////////////////////////////////////////////
/// PPVariable
///////////////////////////////////////////////////////////////////////////////
PPVariable::PPVariable(dynamic_context *_cxt_, 
                       var_dsc _dsc_) : PPIterator(_cxt_), 
                                        dsc(_dsc_)
{
}

PPVariable::~PPVariable()
{
    // nothing to do
}

void PPVariable::open ()
{
    U_ASSERT(cxt);

    id = cxt->var_cxt.producers[dsc].op->register_consumer(dsc);
}

void PPVariable::reopen ()
{
    cxt->var_cxt.producers[dsc].op->reopen(dsc, id);
}

void PPVariable::close ()
{
    // nothing to do
}

void PPVariable::next (tuple &t)
{
    cxt->var_cxt.producers[dsc].op->next(t, dsc, id);

//    if (t.is_eos()) cxt->producers[dsc].op->reopen(dsc, id);
}

PPIterator* PPVariable::copy(dynamic_context *_cxt_)
{
    PPVariable *res = new PPVariable(_cxt_, dsc);
    return res;
}

bool PPVariable::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
/*
    producer &p = cxt->producers[((PPVariable*)cur)->dsc];

    sequence *res_seq = new sequence(1);
    switch (p.type)
    {
    case pt_tuple	: res_seq->add(*(p.t)); break;
    case pt_seq		: res_seq->copy(p.s); break;
    default			: throw USER_EXCEPTION2(SE1003, "Unexpected type of the producer in PPVariable");
    };

    return strict_op_result(cur, res_seq, cxt, r);
*/
	throw USER_EXCEPTION2(SE1002, "PPVariable::result");
}

///////////////////////////////////////////////////////////////////////////////
/// PPGlobalVariable
///////////////////////////////////////////////////////////////////////////////
PPGlobalVariable::PPGlobalVariable(dynamic_context *_cxt_, 
                                   var_dsc _dsc_) : PPIterator(_cxt_), 
                                                    dsc(_dsc_)
{
}

PPGlobalVariable::~PPGlobalVariable()
{
    // nothing to do
}

void PPGlobalVariable::open ()
{
    U_ASSERT(cxt);

    id = dynamic_context::glb_var_cxt.producers[dsc].op->register_consumer(dsc);
}

void PPGlobalVariable::reopen ()
{
    dynamic_context::glb_var_cxt.producers[dsc].op->reopen(dsc, id);
}

void PPGlobalVariable::close ()
{
    dynamic_context::glb_var_cxt.producers[dsc].op->close(dsc, id);
}

void PPGlobalVariable::next (tuple &t)
{
    dynamic_context::glb_var_cxt.producers[dsc].op->next(t, dsc, id);
}

PPIterator* PPGlobalVariable::copy(dynamic_context *_cxt_)
{
    PPGlobalVariable *res = new PPGlobalVariable(_cxt_, dsc);
    return res;
}

bool PPGlobalVariable::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPGlobalVariable::result");
}
