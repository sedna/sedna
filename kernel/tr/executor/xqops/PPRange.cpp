/*
 * File:  PPRange.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPRange.h"
#include "casting_operations.h"
#include "PPUtils.h"

inline tuple_cell getAtomizedCell(tuple& tup)
{
	if (!(tup.cells_number==1 )) throw USER_EXCEPTION2(XPTY0004, "Name argument of Constructor is not a single atomic value");
	return atomize(tup.cells[0]);
}
int PPRange::getIntFromOp(PPOpIn & op)
{
	tuple t(1);
	op.op->next(t);
	if (t.is_eos())
	{
		is_emp=true;
		return 0;
	}
	tuple_cell res=getAtomizedCell(t);
	op.op->next(t);
	if (!(t.is_eos())) throw USER_EXCEPTION(XPTY0004);
	return cast_to_xs_integer(res).get_xs_integer();
}

PPRange::PPRange(variable_context *_cxt_,
               const PPOpIn& _start_,const PPOpIn& _end_) : PPIterator(_cxt_),
                                                        start_op(_start_),end_op(_end_)
{
    
}

PPRange::~PPRange()
{
	delete start_op.op;
	start_op.op=NULL;
	delete end_op.op;
	end_op.op=NULL;
}


void PPRange::open ()
{
    is_emp=false;
	start_op.op->open();
	end_op.op->open();
	if (start_op.op->is_const())
		start=this->getIntFromOp(start_op);
	if (end_op.op->is_const())
		end=this->getIntFromOp(end_op);
	cur=start;
}

void PPRange::reopen ()
{
	start_op.op->reopen();
	end_op.op->reopen();
	cur=start;

}

void PPRange::close ()
{
    start_op.op->close();
	end_op.op->close();
}

void PPRange::next(tuple &t)
{
    if (cur==start)
	{
		if (!start_op.op->is_const())
			start=this->getIntFromOp(start_op);
		if (!end_op.op->is_const())
			end=this->getIntFromOp(end_op);		
		cur=start;
	}
	if (is_emp||cur>end)
	{
		t.set_eos();
		cur=start;
		return;
	}
	t.copy(tuple_cell::atomic(cur));
    cur++;
}

PPIterator* PPRange::copy(variable_context *_cxt_)
{
    PPRange *res = new PPRange(_cxt_, start_op,end_op);
	res->start_op.op=start_op.op->copy(_cxt_);
	res->end_op.op=end_op.op->copy(_cxt_);
    return res;
}
bool PPRange::result(PPIterator* cur, variable_context *cxt, void*& r)
{
 return true;
}

