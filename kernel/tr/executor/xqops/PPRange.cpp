/*
* File:  PPRange.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"

#include "tr/executor/xqops/PPRange.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPRange::PPRange(dynamic_context *_cxt_,
                 operation_info _info_,
                 const PPOpIn& _start_,
                 const PPOpIn& _end_) : PPIterator(_cxt_, _info_, "PPRange"),
                 start_op(_start_),
                 end_op(_end_)
{
}

int64_t
PPRange::getIntFromOp(PPOpIn & op)
{
    tuple t(1);
    op.op->next(t);

    if (t.is_eos())	{
        is_emp = true;
        return 0;
    }

    if ( t.size() != 1 ) 
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

void PPRange::do_open ()
{
    is_emp = false;
    first_time = true;
    start_op.op->open();
    end_op.op->open();
}

void PPRange::do_reopen()
{
    start_op.op->reopen();
    end_op.op->reopen();
    first_time = true;
    is_emp = false;
}

void PPRange::do_close()
{
    start_op.op->close();
    end_op.op->close();
}

void PPRange::do_next(tuple &t)
{
    if (first_time)
    {
        start = this->getIntFromOp(start_op);
        end   = this->getIntFromOp(end_op);		
        cur = start;
        first_time=false;
    }
    if (is_emp || cur>end)
    {
        t.set_eos();
        first_time=true;
        is_emp=false;
        return;
    }
    t.copy(tuple_cell::atomic((int64_t)cur));
    cur++;
}

PPIterator* PPRange::do_copy(dynamic_context *_cxt_)
{
    PPRange *res = new PPRange(_cxt_, info, start_op, end_op);
    res->start_op.op=start_op.op->copy(_cxt_);
    res->end_op.op=end_op.op->copy(_cxt_);
    return res;
}

void PPRange::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    start_op.op->accept(v);
    end_op.op->accept(v);
    v.pop();
}
