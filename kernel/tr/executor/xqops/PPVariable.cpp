/*
 * File:  PPVariable.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "PPVariable.h"

PPVariable::PPVariable(variable_context *_cxt_, 
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
    if (cxt == NULL) 
        throw USER_EXCEPTION2(SE1003, "Context is not set up in PPVariable");

    id = cxt->producers[dsc].op->register_consumer(dsc);
}

void PPVariable::reopen ()
{
    cxt->producers[dsc].op->reopen(dsc, id);
}

void PPVariable::close ()
{
    // nothing to do
}

void PPVariable::next (tuple &t)
{
    cxt->producers[dsc].op->next(t, dsc, id);

//    if (t.is_eos()) cxt->producers[dsc].op->reopen(dsc, id);
}

PPIterator* PPVariable::copy(variable_context *_cxt_)
{
    PPVariable *res = new PPVariable(_cxt_, dsc);
    return res;
}

bool PPVariable::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    producer &p = cxt->producers[((PPVariable*)cur)->dsc];

    sequence *res_seq = new sequence(1);
    switch (p.type)
    {
    case pt_tuple	: res_seq->add(*(p.t)); break;
    case pt_seq		: res_seq->copy(p.s); break;
    default			: throw USER_EXCEPTION2(SE1003, "Unexpected type of the producer in PPVariable");
    };

    return strict_op_result(cur, res_seq, cxt, r);
}
