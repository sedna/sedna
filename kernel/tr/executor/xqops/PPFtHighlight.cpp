/*
 * File:  PPFtHighlight.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFtHighlight.h"
#include "tr/ft/FTsearch.h"
#include "tr/executor/base/visitor/PPVisitor.h"

PPFtHighlight::PPFtHighlight(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _seq_,
                             PPOpIn _query_,
                             bool _hl_fragment_) : PPIterator(_cxt_, _info_),
                                                   seq(_seq_),
                                                   query(_query_),
                                                   hl_fragment(_hl_fragment_),
                                                   sj(NULL), 
                                                   ptr(NULL)
{
}

PPFtHighlight::PPFtHighlight(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _seq_,
                             PPOpIn _query_,
                             PPOpIn _index_,
                             bool _hl_fragment_) : PPIterator(_cxt_, _info_),
                                                   seq(_seq_),
                                                   query(_query_),
                                                   index(_index_),
                                                   hl_fragment(_hl_fragment_),
                                                   sj(NULL), 
                                                   ptr(NULL)
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

void PPFtHighlight::do_open()
{
	seq.op->open();
    query.op->open();
	if (index.op)
		index.op->open();

    first_time = true;
}

void PPFtHighlight::do_reopen()
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
		delete_ft_custom_tree(ptr);
		ptr = NULL;
	}

    first_time = true;
}

void PPFtHighlight::do_close()
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
		delete_ft_custom_tree(ptr);
		ptr = NULL;
	}
	
}

void PPFtHighlight::do_next(tuple &t)
{
	if (first_time)
	{
		tuple_cell tc;

		if (index.op)
		{
			sj=se_new SednaSearchJob(true, hl_fragment);
			index.op->next(t);
			if (t.is_eos())
				throw XQUERY_EXCEPTION(SE1071);
			tc = t.cells[0];
			if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
				throw XQUERY_EXCEPTION(SE1071);
	
			ftc_index_t ftc_idx;
			ft_index_cell_cptr ft_idx = find_ft_index(op_str_buf(tc).c_str(), &ftc_idx); //FIXME: op_str_buf may be destroyed too soon
			if (!ft_idx.found())
				throw USER_EXCEPTION(SE1061);
			sj->set_index(&*ft_idx);
			index.op->next(t);
			if (!t.is_eos())
				throw XQUERY_EXCEPTION(SE1071);

		}
		else
			sj=se_new SednaSearchJob(&seq, ft_xml_hl, NULL, true, hl_fragment);

		query.op->next(t);
		if (t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw XQUERY_EXCEPTION(SE1071);

		sj->set_request(tc);
		query.op->next(t);
		if (!t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);

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
					throw XQUERY_EXCEPTION2(SE1071, "problem with dtsearch"); //FIXME: change exception code
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

PPIterator*  PPFtHighlight::do_copy(dynamic_context *_cxt_)
{
	PPFtHighlight *res;
	if (index.op)
	{
		res = se_new PPFtHighlight(_cxt_, info, seq, query, index, hl_fragment);
	    res->seq.op = seq.op->copy(_cxt_);
	    res->query.op = query.op->copy(_cxt_);
		res->index.op = index.op->copy(_cxt_);
	}
	else
	{
		res = se_new PPFtHighlight(_cxt_, info, seq, query, hl_fragment);
	    res->seq.op = seq.op->copy(_cxt_);
	    res->query.op = query.op->copy(_cxt_);
	}
	return res;
}

void PPFtHighlight::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    seq.op->accept(v);
    query.op->accept(v);
    if(index.op) index.op->accept(v);
    v.pop();
}
