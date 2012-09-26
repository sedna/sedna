/*
 * File:  PPCalculate.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <vector>
#include "common/sedna.h"
#include "tr/executor/xqops/PPCalculate.h"
#include "tr/executor/base/visitor/PPVisitor.h"


using namespace std;


PPCalculate::PPCalculate(dynamic_context *_cxt_,
                         operation_info _info_, 
                         arr_of_PPOpIn *_ch_arr_,
                         CalcOp *_tree_) : PPIterator(_cxt_, _info_, "PPCalculate"),
                                           ch_arr(_ch_arr_),
                                           tree(_tree_)
{
}

PPCalculate::~PPCalculate()
{
    for (unsigned int i = 0; i < ch_arr->size(); i++)
    {
        delete ch_arr->at(i).op;
        ch_arr->at(i).op = NULL;
    }

    delete tree;
    delete ch_arr;
}

void PPCalculate::do_open ()
{
    for (unsigned int i = 0; i < ch_arr->size(); i++) ch_arr->at(i).op->open();

    first_time = true;
}

void PPCalculate::do_reopen()
{
    tree->reopen();

    first_time = true;
}

void PPCalculate::do_close()
{
    for (unsigned int i = 0; i < ch_arr->size(); i++) 
        ch_arr->at(i).op->close();
}

void PPCalculate::do_next(tuple &t)
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

PPIterator* PPCalculate::do_copy(dynamic_context *_cxt_)
{
	unsigned int i = 0;
    arr_of_PPOpIn *new_ch_arr = new arr_of_PPOpIn(ch_arr->size());
	for (i = 0; i < ch_arr->size(); i++)
        new_ch_arr->at(i).ts = ch_arr->at(i).ts;

    PPCalculate *res = new PPCalculate(_cxt_,
                                          info,
                                          new_ch_arr,
                                          tree);

    // copy children
    for (i = 0; i < ch_arr->size(); i++)
        res->ch_arr->at(i).op = ch_arr->at(i).op->copy(_cxt_);

    res->tree = tree->copy(res->ch_arr);

    return res;
}


///////////////////////////////////////////////////////////////////////////////
/// PPVisitor Acceptors
///////////////////////////////////////////////////////////////////////////////

void PPCalculate::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    tree->accept(v);
    v.pop();
}

void UnaryOp::do_accept(PPVisitor &v)    
{
    v.visit (this);
    v.push  (this);
    child->accept(v);
    v.pop();
}

void BinaryOp::do_accept(PPVisitor &v)    
{
    v.visit (this);
    v.push  (this);
    child1->accept(v);
    child2->accept(v);
    v.pop();
}

void BinaryOpCollation::do_accept(PPVisitor &v)    
{
    v.visit (this);
    v.push  (this);
    child1->accept(v);
    child2->accept(v);
    v.pop();
}

void BinaryOpAnd::do_accept(PPVisitor &v)    
{
    v.visit (this);
    v.push  (this);
    child1->accept(v);
    child2->accept(v);
    v.pop();
}

void BinaryOpOr::do_accept(PPVisitor &v)    
{
    v.visit (this);
    v.push  (this);
    child1->accept(v);
    child2->accept(v);
    v.pop();
}

void LeafAtomOp::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    children->at(i).op->accept(v);
    v.pop();
}

void LeafEffectBoolOp::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    children->at(i).op->accept(v);
    v.pop();
}

