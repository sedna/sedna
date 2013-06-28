/*
 * File:  PPSequence.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <vector>

#include "common/sedna.h"

#include "tr/executor/xqops/PPSequence.h"
#include "tr/executor/base/visitor/PPVisitor.h"

using namespace std;

PPSequence::PPSequence(dynamic_context *_cxt_,
                       operation_info _info_,
                       const arr_of_PPOpIn &_ch_arr_) : PPIterator(_cxt_, _info_, "PPSequence"),
                                                        ch_arr(_ch_arr_)
{
    int ts = ch_arr[0].ts;
    for (it = 1; it < ch_arr.size(); it++)
    {
        if (ch_arr[it].ts != ts) throw USER_EXCEPTION2(SE1003, "Children of PPSequence operation have different tuple sizes");
    }
}

PPSequence::~PPSequence()
{
    for (it = 0; it < ch_arr.size(); it++) 
    {
        delete (ch_arr[it].op);
        ch_arr[it].op = NULL;
    }
}


void PPSequence::do_open ()
{
    for (it = 0; it < ch_arr.size(); it++) 
        ch_arr[it].op->open();

    it = 0;
}

void PPSequence::do_reopen()
{
    for (it = 0; it < ch_arr.size(); it++) 
        ch_arr[it].op->reopen();

    it = 0;
}

void PPSequence::do_close()
{
    for (it = 0; it < ch_arr.size(); it++)
        ch_arr[it].op->close();

    it = 0;
}

void PPSequence::do_next(xqp_tuple &t)
{
    while (it < ch_arr.size())
    {
        ch_arr[it].op->next(t);

        if (t.is_eos()) it++;
        else return;
    }

    t.set_eos();
    it = 0;
}

PPIterator* PPSequence::do_copy(dynamic_context *_cxt_)
{
    PPSequence *res = se_new PPSequence(_cxt_, info, ch_arr);

    for (it = 0; it < ch_arr.size(); it++)
        res->ch_arr[it].op = ch_arr[it].op->copy(_cxt_);

    return res;
}

void PPSequence::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    for (it = 0; it < ch_arr.size(); it++)
        ch_arr[it].op->accept(v);
    v.pop();
}
