/*
 * File:  PPSResLStub.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPSResLStub.h"


PPSResLStub::PPSResLStub(variable_context *_cxt_, 
                         PPIterator *_op_, 
                         sequence *_s_) : PPIterator(_cxt_), 
                                          op(_op_), 
                                          s(_s_)
{
    it = s->begin();
}

PPSResLStub::~PPSResLStub()
{
    delete op;
    op = NULL;
    if (s)
    {
        delete s;
        s = NULL;
    }
}

void PPSResLStub::open ()
{
    op->open();
}

void PPSResLStub::reopen ()
{
    op->reopen();
}

void PPSResLStub::close ()
{
    op->close();
}

void PPSResLStub::next(tuple &t)
{
    if (s)
    {
        if (it != s->end())
        {
            s->get(t, it);
            ++it;
            return;
        }
        else
        {
            delete s;
            s = NULL;
        }
    }

    op->next(t);
}

PPIterator* PPSResLStub::copy(variable_context *_cxt_)
{
    throw USER_EXCEPTION2(SE1003, "Method PPSResLStub::copy must not be called");
}

