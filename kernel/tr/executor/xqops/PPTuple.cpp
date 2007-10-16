/*
 * File:  PPTuple.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <vector>
#include "common/sedna.h"
#include "tr/executor/xqops/PPTuple.h"

using namespace std;

PPTuple::PPTuple(dynamic_context *_cxt_,
                 const arr_of_PPOpIn &_ch_arr_) : PPIterator(_cxt_),
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

void PPTuple::open ()
{
    for (i = 0; i < ch_arr.size(); i++) 
        ch_arr[i].op->open();

    i = 0;
}

void PPTuple::reopen ()
{
    for (i = 0; i < ch_arr.size(); i++) 
        ch_arr[i].op->reopen();

    i = 0;
}

void PPTuple::close ()
{
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->close();

    i = 0;
}

void PPTuple::next(tuple &t)
{
    SET_CURRENT_PP(this);
    
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

    RESTORE_CURRENT_PP;
}

PPIterator* PPTuple::copy(dynamic_context *_cxt_)
{
    PPTuple *res = se_new PPTuple(_cxt_, ch_arr);

    for (i = 0; i < ch_arr.size(); i++)
        res->ch_arr[i].op = ch_arr[i].op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);

    return res;
}

bool PPTuple::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
/*
    arr_of_PPOpIn ch_arr;
    ((PPTuple*)cur)->children(ch_arr);

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

        r = se_new PPTuple(cxt, ch_arr);
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
*/
    return true;
}
