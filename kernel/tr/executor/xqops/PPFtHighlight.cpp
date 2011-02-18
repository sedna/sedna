/*
 * File:  PPFtHighlight.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFtHighlight.h"
#include "tr/ft/ft_util.h"
#include "tr/executor/base/visitor/PPVisitor.h"

PPFtHighlight::PPFtHighlight(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _seq_,
                             PPOpIn _query_,
                             bool _hl_fragment_) : PPIterator(_cxt_, _info_, "PPFtHighlight"),
                                                   query(_query_),
                                                   seq(_seq_),
                                                   hl_fragment(_hl_fragment_),
                                                   fthl(NULL)
{
}

PPFtHighlight::PPFtHighlight(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _seq_,
                             PPOpIn _query_,
                             PPOpIn _options_,
                             bool _hl_fragment_) : PPIterator(_cxt_, _info_, "PPFtHighlight"),
                                                   query(_query_),
                                                   seq(_seq_),
                                                   options(_options_),
                                                   hl_fragment(_hl_fragment_),
                                                   fthl(NULL)
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
	if (options.op)
    {
        delete options.op;
        options.op = NULL;
    }
	if (fthl)
	{
		delete fthl;
		fthl = NULL;
	}
}

void PPFtHighlight::do_open()
{
	seq.op->open();
    query.op->open();
	if (options.op)
		options.op->open();

    first_time = true;
}

void PPFtHighlight::do_reopen()
{
	seq.op->reopen();
    query.op->reopen();
	if (options.op)
		options.op->reopen();

	if (fthl)
	{
		delete fthl;
		fthl = NULL;
	}

    first_time = true;
}

void PPFtHighlight::do_close()
{
	seq.op->close();
    query.op->close();
	if (options.op)
		options.op->close();
	if (fthl != NULL)
	{
		delete fthl;
		fthl = NULL;
	}
}

void PPFtHighlight::do_next(tuple &t)
{
	if (first_time)
	{
		tuple_cell tc;

		U_ASSERT(fthl == NULL);

		if (options.op)
		{
			options.op->next(t);
			if (t.is_eos())
				throw XQUERY_EXCEPTION(SE1071);
			tc = t.cells[0];
			if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
				throw XQUERY_EXCEPTION(SE1071);

			op_str_buf buf(tc);
			fthl = new FtHighlighter(buf.c_str(), hl_fragment, &seq);
	
			options.op->next(t);
			if (!t.is_eos())
				throw XQUERY_EXCEPTION(SE1071);
		}
		else
			fthl = new FtHighlighter(NULL, hl_fragment, &seq);

		query.op->next(t);
		if (t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw XQUERY_EXCEPTION(SE1071);

		fthl->set_request(tc);
		query.op->next(t);
		if (!t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);

		first_time = false;
	}

	fthl->get_next_result(t);

	if (t.is_eos())
	{
		delete fthl;
		fthl = NULL;
		first_time = true;
	}
}

PPIterator*  PPFtHighlight::do_copy(dynamic_context *_cxt_)
{
	PPFtHighlight *res;
	if (options.op)
	{
		res = se_new PPFtHighlight(_cxt_, info, seq, query, options, hl_fragment);
	    res->seq.op = seq.op->copy(_cxt_);
	    res->query.op = query.op->copy(_cxt_);
		res->options.op = options.op->copy(_cxt_);
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
    if(options.op) options.op->accept(v);
    v.pop();
}
