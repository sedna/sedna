/*
 * File:  PPTuple.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <vector>
#include "common/sedna.h"
#include "tr/executor/xqops/PPTuple.h"
#include "tr/executor/base/visitor/PPVisitor.h"

using namespace std;

PPTuple::PPTuple(dynamic_context *_cxt_,
                 operation_info _info_,
                 const arr_of_PPOpIn &_ch_arr_) : PPIterator(_cxt_, _info_, "PPTuple"),
                                                  ch_arr(_ch_arr_),
                                                  lt(1)
{
}

PPTuple::~PPTuple()
{
    for (i = 0; i < ch_arr.size(); i++) 
    {
        delete (ch_arr[i].op);
        ch_arr[i].op = NULL;
    }
}

void PPTuple::do_open ()
{
    for (i = 0; i < ch_arr.size(); i++) 
        ch_arr[i].op->open();

    i = 0;
}

void PPTuple::do_reopen()
{
    for (i = 0; i < ch_arr.size(); i++) 
        ch_arr[i].op->reopen();

    i = 0;
}

void PPTuple::do_close()
{
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->close();

    i = 0;
}

void PPTuple::do_next(xqp_tuple &t)
{
    if (!i)
    {
        t.eos = false;
        for (; i < ch_arr.size(); i++)
        {
            ch_arr[i].op->next(lt);
            if (lt.is_eos())
            {
                t.cells[i] = tuple_cell::eos();
            }
            else
            {
                t.cells[i] = ch_arr[i].get(lt);
                ch_arr[i].op->next(lt);
                if (!lt.is_eos()) throw USER_EXCEPTION2(SE1003, "The tuple cell value is a sequence in PPTuple");
            }
        }
    }
    else 
    {
        t.set_eos();
        i = 0;
    }
}

PPIterator* PPTuple::do_copy(dynamic_context *_cxt_)
{
    PPTuple *res = se_new PPTuple(_cxt_, info, ch_arr);

    for (i = 0; i < ch_arr.size(); i++)
        res->ch_arr[i].op = ch_arr[i].op->copy(_cxt_);

    return res;
}

void PPTuple::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->accept(v);
    v.pop();
}
