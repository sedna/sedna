/*
 * File:  PPCalculate.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <vector>

#include "common/sedna.h"

#include "tr/executor/xqops/PPCalculate.h"
#include "tr/executor/xqops/PPSLStub.h"

using namespace std;


PPCalculate::PPCalculate(dynamic_context *_cxt_, 
                         arr_of_PPOpIn *_ch_arr_,
                         CalcOp *_tree_) : PPIterator(_cxt_),
                                           ch_arr(_ch_arr_),
                                           tree(_tree_)
{
}

PPCalculate::~PPCalculate()
{
    for (int i = 0; i < ch_arr->size(); i++)
    {
        delete ch_arr->at(i).op;
        ch_arr->at(i).op = NULL;
    }

    delete tree;
    delete ch_arr;
}

void PPCalculate::open ()
{
    for (int i = 0; i < ch_arr->size(); i++) ch_arr->at(i).op->open();

    first_time = true;
}

void PPCalculate::reopen ()
{
    tree->reopen();

    first_time = true;
}

void PPCalculate::close ()
{
    for (int i = 0; i < ch_arr->size(); i++) ch_arr->at(i).op->close();
}

void PPCalculate::next(tuple &t)
{
    if (first_time)
    {
        first_time = false;
        tuple_cell tc = tree->next(cxt);
        if (tc.is_eos())
		{
			t.set_eos();
			first_time = true;
		}
        else t.copy(tc);
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPCalculate::copy(dynamic_context *_cxt_)
{
	int i = 0;
    arr_of_PPOpIn *new_ch_arr = se_new arr_of_PPOpIn(ch_arr->size());
	for (i = 0; i < ch_arr->size(); i++)
        new_ch_arr->at(i).ts = ch_arr->at(i).ts;

    PPCalculate *res = se_new PPCalculate(_cxt_,
                                       new_ch_arr,
                                       tree);

    // copy children
    for (i = 0; i < ch_arr->size(); i++)
        res->ch_arr->at(i).op = ch_arr->at(i).op->copy(_cxt_);

    res->tree = tree->copy(res->ch_arr);

    return res;
}

bool PPCalculate::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
/*
    arr_of_PPOpIn *ch_arr = se_new arr_of_PPOpIn;
    ((PPCalculate*)cur)->children(ch_arr);

    //int a = ch_arr->size(); // !!! debug
    vector<void*> ch_r(ch_arr->size());
    vector<bool>  ch_s(ch_arr->size());

    bool is_everything_strict = true;
    int i = 0;
    for (i = 0; i < ch_arr->size(); i++)
    {
        ch_s[i] = (ch_arr->at(i).op->res_fun())(ch_arr->at(i).op, cxt, ch_r[i]);
        is_everything_strict = is_everything_strict && ch_s[i];
        //if (ch_s[i]) //!!! debug
        //{
        //    sequence *s = (sequence*)(ch_r[i]);
        //}
    }

    if (!is_everything_strict)
    {
        for (i = 0; i < ch_arr->size(); i++)
        {
            if (ch_s[i])
            { // result is strict
                ch_arr->at(i).op = se_new PPSLStub(cxt, 
                                                ch_arr->at(i).op->copy(cxt), 
                                                (sequence*)(ch_r[i]));
                
            }
            else
            { // result is NON strict
                ch_arr->at(i).op = (PPIterator*)(ch_r[i]);
            }
        }

        CalcOp *tree = ((PPCalculate*)cur)->tree->copy(ch_arr);
        r = se_new PPCalculate(cxt, ch_arr, tree);
        return false;
    }

    //sequence *s1 = (sequence*)ch_r[0];
    //sequence *s2 = (sequence*)ch_r[1];

    tuple_cell tc = ((PPCalculate*)cur)->tree->result(ch_r);
    if (tc.is_eos()) r = se_new sequence(1);
    else r = se_new sequence(tc);
*/
    return true;
}
