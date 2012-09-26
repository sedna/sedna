/*
 * File:  PPNodeComparison.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPNodeComparison.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPNodeComparison* PPNodeComparison::PPGTNodeComparison(dynamic_context *_cxt_,
                                                       operation_info _info_, 
                                                       PPOpIn _seq1_,
                                                       PPOpIn _seq2_)
{ 
	return new PPNodeComparison(_cxt_,_info_,_seq1_,_seq2_,1);
}
PPNodeComparison* PPNodeComparison::PPLTNodeComparison(dynamic_context *_cxt_,
                                                       operation_info _info_, 
                                                       PPOpIn _seq1_,
                                                       PPOpIn _seq2_)
{ 
	return new PPNodeComparison(_cxt_,_info_,_seq1_,_seq2_,-1);
}
PPNodeComparison* PPNodeComparison::PPEQNodeComparison(dynamic_context *_cxt_,
                                                       operation_info _info_, 
                                                       PPOpIn _seq1_,
                                                       PPOpIn _seq2_)
{ 
	return new PPNodeComparison(_cxt_,_info_,_seq1_,_seq2_,0);
}
PPNodeComparison* PPNodeComparison::PPANNodeComparison(dynamic_context *_cxt_,
                                                       operation_info _info_, 
                                                       PPOpIn _seq1_,
                                                       PPOpIn _seq2_)
{ 
	return new PPNodeComparison(_cxt_,_info_,_seq1_,_seq2_,2);
}

PPNodeComparison::PPNodeComparison(dynamic_context *_cxt_,
                                   operation_info _info_,
                                   PPOpIn _seq1_,
                                   PPOpIn _seq2_,
                                   int _type_): PPIterator(_cxt_, _info_, "PPNodeComparison"),
                                                seq1(_seq1_),
                                                seq2(_seq2_),
                                                type(_type_)
{
}

PPIterator* PPNodeComparison::do_copy(dynamic_context *_cxt_)
{
	PPNodeComparison *res ;
	res = new PPNodeComparison(_cxt_, info, seq1, seq2, type);
	res->seq1.op = seq1.op->copy(_cxt_);
	res->seq2.op = seq2.op->copy(_cxt_);
    return res;
}

void PPNodeComparison::do_close()
{
    seq1.op->close();
	seq2.op->close();
}

void PPNodeComparison::do_open ()
{
    seq1.op->open();
	seq2.op->open();
    first_time = true;   
}

void PPNodeComparison::do_reopen()
{
    seq1.op->reopen();
	seq2.op->reopen();
    first_time = true;
}

PPNodeComparison::~PPNodeComparison()
{
	delete seq1.op;
	seq1.op = NULL;
	delete seq2.op;
	seq2.op = NULL;
}

void PPNodeComparison::do_next (tuple &t)
{
        
    if (first_time)
    {
        first_time = false;
		//0. operands loading
		tuple t1(seq1.ts);
		seq1.op->next(t1);
		tuple t2(seq2.ts);
		seq2.op->next(t2);
		
		//1. operands verify
		if (!( (t1.cells[0].is_node()||t1.is_eos())&&((t2.cells[0].is_node()||t2.is_eos()))))
			throw XQUERY_EXCEPTION(XPTY0004);
		//2. emty sequence check
		if (t1.is_eos()||t2.is_eos())
		{
			t.set_eos();
			first_time = true;
		}
		else
		{
			//3. comparisoning
			xptr node1=t1.cells[0].get_node();
			xptr node2=t2.cells[0].get_node();
			switch(type)
			{
			case -1:
				if (nid_cmp(node1,node2)<0)
					t.copy(tuple_cell::atomic(true));
				else
					t.copy(tuple_cell::atomic(false));
				break;
			case 0:
				if (node1==node2)
					t.copy(tuple_cell::atomic(true));
				else
					t.copy(tuple_cell::atomic(false));
				break;
			case 1:
				if (nid_cmp(node1,node2)>0)
					t.copy(tuple_cell::atomic(true));
				else
					t.copy(tuple_cell::atomic(false));
				break;
			case 2:
				if (nid_cmp_effective(node1,node2)==-2)
					t.copy(tuple_cell::atomic(true));
				else
					t.copy(tuple_cell::atomic(false));
				break;
			default:
				throw USER_EXCEPTION2(SE1003, "in PPNodeComparison");
			}
		}
		//4. finishing child sequences
		if (!t1.is_eos())
		{
			seq1.op->next(t1);
			if (!t1.is_eos())
				throw XQUERY_EXCEPTION(XPTY0004);
		}
		if (!t2.is_eos())
		{
			seq2.op->next(t2);
			if (!t2.is_eos())
				throw XQUERY_EXCEPTION(XPTY0004);
		}
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

void PPNodeComparison::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    seq1.op->accept(v);
    seq2.op->accept(v);
    v.pop();
}
