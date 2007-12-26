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



//////////////////////////////////////////////
//// PPFtIndexScan2
//////////////////////////////////////////////

PPFtIndexScan2::PPFtIndexScan2(dynamic_context *_cxt_,
                PPOpIn _idx_name_,
				PPOpIn _query_) :
						PPIterator(_cxt_),
						idx_name(_idx_name_),
						query(_query_),
						sj(NULL)
{
	max_results.op = NULL;
	field_weights.op = NULL;
}
PPFtIndexScan2::PPFtIndexScan2(dynamic_context *_cxt_,
                PPOpIn _idx_name_,
				PPOpIn _query_,
				PPOpIn _max_results_) :
						PPIterator(_cxt_),
						idx_name(_idx_name_),
						query(_query_),
						max_results(_max_results_),
						sj(NULL)
{
	field_weights.op = NULL;
}
PPFtIndexScan2::PPFtIndexScan2(dynamic_context *_cxt_,
                PPOpIn _idx_name_,
				PPOpIn _query_,
				PPOpIn _max_results_,
				PPOpIn _field_weights_) :
						PPIterator(_cxt_),
						idx_name(_idx_name_),
						query(_query_),
						max_results(_max_results_),
						field_weights(_field_weights_),
						sj(NULL)
{
}

PPFtIndexScan2::~PPFtIndexScan2()
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
	if (max_results.op)
	{
        delete max_results.op;
        max_results.op = NULL;
	}
	if (field_weights.op)
	{
        delete field_weights.op;
        field_weights.op = NULL;
	}
	if (sj)
	{
		delete sj;
		sj = NULL;
	}
}

void PPFtIndexScan2::open()
{
	idx_name.op->open();
    query.op->open();
	if (max_results.op)
		max_results.op->open();
	if (field_weights.op)
		field_weights.op->open();

    first_time = true;
}

void PPFtIndexScan2::reopen()
{
	idx_name.op->reopen();
    query.op->reopen();
	if (max_results.op)
		max_results.op->reopen();
	if (field_weights.op)
		field_weights.op->reopen();

	if (sj)
	{
		delete sj;
		sj = NULL;
	}

    first_time = true;
}

void PPFtIndexScan2::close()
{
	idx_name.op->close();
    query.op->close();
	if (max_results.op)
		max_results.op->close();
	if (field_weights.op)
		field_weights.op->close();
	if (sj != NULL)
	{
		delete sj;
		sj = NULL;
	}
}

void PPFtIndexScan2::next(tuple &t)
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
		sj=se_new SednaSearchJob2();
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

		if (max_results.op)
		{
			max_results.op->next(t);
			if (!t.is_eos()) //ignore, if it's empty seq.
			{
				tc = t.cells[0];
				if (!tc.is_atomic() || tc.get_atomic_type() != xs_integer)
					throw XQUERY_EXCEPTION2(SE1071, "max_results in ftwindex-scan must be an xs:integer");
				sj->set_max_results(tc.get_xs_integer());
				max_results.op->next(t);
				if (!t.is_eos())
					throw XQUERY_EXCEPTION2(SE1071, "max_results in ftwindex-scan must be an xs:integer");
			}
		}
		if (field_weights.op)
		{
			field_weights.op->next(t);
			if (t.is_eos())
				throw XQUERY_EXCEPTION(SE1071);
			tc = t.cells[0];
			if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
				throw XQUERY_EXCEPTION(SE1071);
			sj->set_field_weights(tc);
			field_weights.op->next(t);
			if (!t.is_eos())
				throw XQUERY_EXCEPTION(SE1071);
		}

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


PPIterator*  PPFtIndexScan2::copy(dynamic_context *_cxt_)
{
	PPFtIndexScan2 *res = se_new PPFtIndexScan2(_cxt_, idx_name, query, max_results, field_weights); //FIXME: mb using different constructors is better?
    res->idx_name.op = idx_name.op->copy(_cxt_);
    res->query.op = query.op->copy(_cxt_);
	if (max_results.op)
		res->max_results.op = max_results.op->copy(_cxt_);
	if (field_weights.op)
		res->field_weights.op = field_weights.op->copy(_cxt_);
	
    res->set_xquery_line(__xquery_line);
	return res;
}

bool PPFtIndexScan2::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFtIndexScan2::result");
}
