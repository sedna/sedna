/*
 * File:  PPSLStub.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPSLStub.h"


PPSLStub::PPSLStub(variable_context *_cxt_, 
                   PPIterator *_op_, 
                   sequence *_s_) : PPIterator(_cxt_), op(_op_), s(_s_)
{
    it = s->begin();
}

PPSLStub::~PPSLStub()
{
    delete op;
    op = NULL;
    if (s)
    {
        delete s;
        s = NULL;
    }
}

void PPSLStub::open ()
{
    op->open();
}

void PPSLStub::reopen ()
{
    op->reopen();
}

void PPSLStub::close ()
{
    op->close();
}

void PPSLStub::next(tuple &t)
{
    if (s)
    {
        if (it != s->end())
        {
            s->get(t, it);
            ++it;
        }
        else
        {
            delete s;
            s = NULL;
            t.set_eos();
        }
    }
    else
        op->next(t);
}

PPIterator* PPSLStub::copy(variable_context *_cxt_)
{
    throw USER_EXCEPTION2(SE1003, "Method PPSLStub::copy must not be called");
}

