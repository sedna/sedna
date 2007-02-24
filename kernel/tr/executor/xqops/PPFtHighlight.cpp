/*
 * File:  PPFtHighlight.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFtHighlight.h"
#include "tr/ft/FTsearch.h"

PPFtHighlight::PPFtHighlight(dynamic_context *_cxt_,
                PPOpIn _seq_,
				PPOpIn _query_,
				bool _hl_fragment_) :
						PPIterator(_cxt_),
						seq(_seq_),
						query(_query_),
						hl_fragment(_hl_fragment_),
						sj(NULL), ptr(NULL)
{
}

PPFtHighlight::PPFtHighlight(dynamic_context *_cxt_,
                PPOpIn _seq_,
				PPOpIn _query_,
				PPOpIn _index_,
				bool _hl_fragment_) :
						PPIterator(_cxt_),
						seq(_seq_),
						query(_query_),
						index(_index_),
						hl_fragment(_hl_fragment_),
						sj(NULL), ptr(NULL)
{
}
PPFtHighlight::~PPFtHighlight()
{
	if (seq.op)
    {
        delete seq.op;
        seq.op = NULL;
    }
	if (query.op)
    {
        delete query.op;
        query.op = NULL;
    }
	if (index.op)
    {
        delete index.op;
        index.op = NULL;
    }
	if (sj)
	{
		delete sj;
		sj = NULL;
	}
}

void PPFtHighlight::open()
{
	seq.op->open();
    query.op->open();
	if (index.op)
		index.op->open();

    first_time = true;
}

void PPFtHighlight::reopen()
{
	seq.op->reopen();
    query.op->reopen();
	if (index.op)
		index.op->reopen();

	if (sj)
	{
		delete sj;
		sj = NULL;
	}
	if (ptr)
	{
		ft_index_cell::delete_custom_tree(ptr);
		ptr = NULL;
	}

    first_time = true;
}

void PPFtHighlight::close()
{
	seq.op->close();
    query.op->close();
	if (index.op)
		index.op->close();
	if (sj != NULL)
	{
		delete sj;
		sj = NULL;
	}
	if (ptr)
	{
		ft_index_cell::delete_custom_tree(ptr);
		ptr = NULL;
	}
	
}

void PPFtHighlight::next(tuple &t)
{
	if (first_time)
	{
		tuple_cell tc;

		if (index.op)
		{
			sj=se_new SednaSearchJob(true, hl_fragment);
			index.op->next(t);
			if (t.is_eos())
				throw USER_EXCEPTION(SE1071);
			tc = t.cells[0];
			if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
				throw USER_EXCEPTION(SE1071);
	
			sj->set_index(tc);
			index.op->next(t);
			if (!t.is_eos())
				throw USER_EXCEPTION(SE1071);

		}
		else
			sj=se_new SednaSearchJob(&seq, ft_xml_hl, NULL, true, hl_fragment);

		query.op->next(t);
		if (t.is_eos())
			throw USER_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw USER_EXCEPTION(SE1071);

		sj->set_request(tc);
		query.op->next(t);
		if (!t.is_eos())
			throw USER_EXCEPTION(SE1071);

		first_time = false;
	}

	if (index.op)
	{
		while (true)
		{
			seq.op->next(t);
			if (t.is_eos())
				break;
			tuple_cell tc = t.cells[0];
			sj->set_file_cond_for_node(tc);

			sj->get_next_result(t);
			if (!t.is_eos())
			{
				tuple tmp(1);
				sj->get_next_result(tmp);
				if (!tmp.is_eos())
				{
					delete sj;
					sj = NULL;
					throw USER_EXCEPTION2(SE1071, "problem with dtsearch"); //FIXME: change exception code
				}
				break;
			}
		}
	}
	else
		sj->get_next_result(t);

	if (t.is_eos())
	{
		delete sj;
		sj = NULL;
		first_time = true;
	}
}

PPIterator*  PPFtHighlight::copy(dynamic_context *_cxt_)
{
	PPFtHighlight *res;
	if (index.op)
	{
		res = se_new PPFtHighlight(_cxt_, seq, query, index, hl_fragment);
	    res->seq.op = seq.op->copy(_cxt_);
	    res->query.op = query.op->copy(_cxt_);
		res->index.op = index.op->copy(_cxt_);
	}
	else
	{
		res = se_new PPFtHighlight(_cxt_, seq, query, hl_fragment);
	    res->seq.op = seq.op->copy(_cxt_);
	    res->query.op = query.op->copy(_cxt_);
	}

	return res;
}

bool PPFtHighlight::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFtScan::result");
}
