/*
 * File:  PPSequence.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <vector>

#include "common/sedna.h"

#include "tr/executor/xqops/PPSLStub.h"
#include "tr/executor/xqops/PPSequence.h"

using namespace std;

PPSequence::PPSequence(dynamic_context *_cxt_,
                       const arr_of_PPOpIn &_ch_arr_) : PPIterator(_cxt_),
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


void PPSequence::open ()
{
    for (it = 0; it < ch_arr.size(); it++) 
        ch_arr[it].op->open();

    it = 0;
}

void PPSequence::reopen ()
{
    for (it = 0; it < ch_arr.size(); it++) 
        ch_arr[it].op->reopen();

    it = 0;
}

void PPSequence::close ()
{
    for (it = 0; it < ch_arr.size(); it++)
        ch_arr[it].op->close();

    it = 0;
}

void PPSequence::next(tuple &t)
{
    SET_CURRENT_PP(this);
    
    while (it < ch_arr.size())
    {
        ch_arr[it].op->next(t);

        if (t.is_eos()) it++;
        else {RESTORE_CURRENT_PP; return;}
    }

    t.set_eos();
    it = 0;

    RESTORE_CURRENT_PP;
}

PPIterator* PPSequence::copy(dynamic_context *_cxt_)
{
    PPSequence *res = se_new PPSequence(_cxt_, ch_arr);

    for (it = 0; it < ch_arr.size(); it++)
        res->ch_arr[it].op = ch_arr[it].op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);

    return res;
}

bool PPSequence::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    arr_of_PPOpIn ch_arr;
    ((PPSequence*)cur)->children(ch_arr);

    vector<void*> ch_r(ch_arr.size());
    vector<bool>  ch_s(ch_arr.size());

    bool is_everything_strict = true;
    int i = 0;
    for (i = 0; i < ch_arr.size(); i++)
    {
        ch_s[i] = (ch_arr[i].op->res_fun())(ch_arr[i].op, cxt, ch_r[i]);
        is_everything_strict = is_everything_strict && ch_s[i];
    }

    if (!is_everything_strict)
    {
        for (i = 0; i < ch_arr.size(); i++)
        {
            if (ch_s[i])
            { // result is strict
                ch_arr[i].op = se_new PPSLStub(cxt, 
                                            ch_arr[i].op->copy(cxt), 
                                            (sequence*)(ch_r[i]));
                
            }
            else
            { // result is NON strict
                ch_arr[i].op = (PPIterator*)(ch_r[i]);
            }
        }

        r = se_new PPSequence(cxt, ch_arr);
        return false;
    }

    sequence* res_seq = se_new sequence(ch_arr[0].ts);
    tuple t(ch_arr[0].ts);

    for (i = 0; i < ch_arr.size(); i++)
    {
        sequence *ch_seq = (sequence*)(ch_r[i]);
        for (int j = 0; j < ch_seq->size(); j++)
        {
            ch_seq->get(t, j);
            res_seq->add(t);
        }
        delete ch_seq;
        ch_r[i] = NULL;
    }

    return strict_op_result(cur, res_seq, cxt, r);
}
