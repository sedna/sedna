/*
 * File:  PPFtIndexScan.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFtIndexScan.h"
#include "tr/ft/FTsearch.h"

PPFtIndexScan::PPFtIndexScan(dynamic_context *_cxt_,
                PPOpIn _idx_name_,
				PPOpIn _query_) :
						PPIterator(_cxt_),
						idx_name(_idx_name_),
						query(_query_),
						sj(NULL)
{
}

PPFtIndexScan::~PPFtIndexScan()
{
	if (idx_name.op)
    {
        delete idx_name.op;
        idx_name.op = NULL;
    }
	if (query.op)
    {
        delete query.op;
        query.op = NULL;
    }
	if (sj)
	{
		delete sj;
		sj = NULL;
	}
}

void PPFtIndexScan::open()
{
	idx_name.op->open();
    query.op->open();

    first_time = true;
}

void PPFtIndexScan::reopen()
{
	idx_name.op->reopen();
    query.op->reopen();

	if (sj)
	{
		delete sj;
		sj = NULL;
	}

    first_time = true;
}

void PPFtIndexScan::close()
{
	idx_name.op->close();
    query.op->close();
	if (sj != NULL)
	{
		delete sj;
		sj = NULL;
	}
	
}

void PPFtIndexScan::next(tuple &t)
{
	SET_CURRENT_PP(this);

	if (first_time)
	{
		tuple_cell tc;

		query.op->next(t);
		if (t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw XQUERY_EXCEPTION(SE1071);
		sj=se_new SednaSearchJob();
		sj->set_request(tc);
		query.op->next(t);
		if (!t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);

		idx_name.op->next(t);
		if (t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw XQUERY_EXCEPTION(SE1071);
		sj->set_index(tc);
		idx_name.op->next(t);
		if (!t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);

		first_time = false;
	}

	sj->get_next_result(t);
	if (t.is_eos())
	{
		delete sj;
		sj = NULL;
		first_time = true;
	}

	RESTORE_CURRENT_PP;
}

PPIterator*  PPFtIndexScan::copy(dynamic_context *_cxt_)
{
	PPFtIndexScan *res = se_new PPFtIndexScan(_cxt_, idx_name, query);
    res->idx_name.op = idx_name.op->copy(_cxt_);
    res->query.op = query.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
	return res;
}

bool PPFtIndexScan::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFtIndexScan::result");
}