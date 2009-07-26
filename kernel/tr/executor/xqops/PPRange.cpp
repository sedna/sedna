/*
 * File:  PPRange.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPRange.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/PPUtils.h"


PPRange::PPRange(dynamic_context *_cxt_,
                 const PPOpIn& _start_,
                 const PPOpIn& _end_) : PPIterator(_cxt_),
                                        start_op(_start_),
                                        end_op(_end_)
{
}


__int64 
PPRange::getIntFromOp(PPOpIn & op)
{
	tuple t(1);
	op.op->next(t);

	if (t.is_eos())	{
		is_emp = true;
		return 0;
	}

	if ( t.cells_number != 1 ) 
	    throw XQUERY_EXCEPTION2(XPTY0004, "range expression argument is not a single atomic value");

	tuple_cell res = atomize(t.cells[0]);
	
	op.op->next(t);
	
	if (!(t.is_eos())) 
	    throw XQUERY_EXCEPTION2(XPTY0004, "range expression argument is not a single atomic value");

	if (res.get_atomic_type() == xs_untypedAtomic)
		res=cast(res,xs_integer);
	
	if (res.get_atomic_type()!=xs_integer&&!is_derived_from_xs_integer(res.get_atomic_type()))
		throw XQUERY_EXCEPTION2(XPTY0004, "range expression argument is not an integer value");

	return res.get_xs_integer();
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
    SET_CURRENT_PP(this);
    
    if (cur == start)
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
		{RESTORE_CURRENT_PP; return;}
	}
	t.copy(tuple_cell::atomic((__int64)cur));
    cur++;
}

PPIterator* PPRange::copy(dynamic_context *_cxt_)
{
    PPRange *res = se_new PPRange(_cxt_, start_op,end_op);
	res->start_op.op=start_op.op->copy(_cxt_);
	res->end_op.op=end_op.op->copy(_cxt_);
	res->set_xquery_line(__xquery_line);
    return res;
}

bool PPRange::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    return true;
}

