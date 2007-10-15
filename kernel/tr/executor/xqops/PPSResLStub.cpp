/*
 * File:  PPSResLStub.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "tr/executor/xqops/PPSResLStub.h"


PPSResLStub::PPSResLStub(dynamic_context *_cxt_, 
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
    SET_XQUERY_LINE(__xquery_line);
    
    if (s)
    {
        if (it != s->end())
        {
            s->get(t, it);
            ++it;
            {UNDO_XQUERY_LINE; return;}
        }
        else
        {
            delete s;
            s = NULL;
        }
    }

    op->next(t);

    UNDO_XQUERY_LINE;
}

PPIterator* PPSResLStub::copy(dynamic_context *_cxt_)
{
    throw USER_EXCEPTION2(SE1003, "Method PPSResLStub::copy must not be called");
}

