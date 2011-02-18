/*
 * File:  PPFtIndexDict.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/ft/ft_index_data.h"
#include "tr/ft/ft_cache.h"

#include "tr/executor/xqops/PPFtIndexDict.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/strings/opt_parser.h"

PPFtIndexDict::PPFtIndexDict(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _idx_name_) : PPIterator(_cxt_, _info_, "PPFtIndexDict"),
                                               idx_name(_idx_name_),
                                               ftc_res(NULL)
{
	options.op = NULL;
}
PPFtIndexDict::PPFtIndexDict(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _idx_name_,
                             PPOpIn _options_) : PPIterator(_cxt_, _info_, "PPFtIndexDict"),
                                                 idx_name(_idx_name_),
                                                 options(_options_),
                                                 ftc_res(NULL)
{
}


PPFtIndexDict::~PPFtIndexDict()
{
	if (idx_name.op)
    {
        delete idx_name.op;
        idx_name.op = NULL;
    }
	if (options.op)
	{
		delete options.op;
		options.op = NULL;
	}
	if (ftc_res)
	{
		delete ftc_res;
		ftc_res = NULL;
	}
}

void PPFtIndexDict::do_open()
{
	idx_name.op->open();
	if (options.op)
		options.op->open();

    first_time = true;
}

void PPFtIndexDict::do_reopen()
{
	idx_name.op->reopen();
	if (options.op)
		options.op->reopen();

	if (ftc_res)
	{
		delete ftc_res;
		ftc_res = NULL;
	}

    first_time = true;
}

void PPFtIndexDict::do_close()
{
	idx_name.op->close();
	if (options.op)
		options.op->close();
	if (ftc_res != NULL)
	{
		delete ftc_res;
		ftc_res = NULL;
	}

}

void PPFtIndexDict::do_next(tuple &t)
{
    if (first_time)
	{
		tuple_cell tc;

		idx_name.op->next(t);
		if (t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw XQUERY_EXCEPTION(SE1071);

		ftc_index_t ftc_idx;
		ft_index_cell_cptr ft_idx = find_ft_index(op_str_buf(tc).c_str(), &ftc_idx); //FIXME: op_str_buf may be destroyed too soon
		if (!ft_idx.found())
			throw USER_EXCEPTION(SE1061);
		idx_name.op->next(t);
		if (!t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);

		if (options.op)
		{
			options.op->next(t);
			if (t.is_eos())
				throw XQUERY_EXCEPTION(SE1071);
			tc = t.cells[0];
			if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
				throw XQUERY_EXCEPTION(SE1071);

			OptionsParser opts;
			opts.set_tc(&tc);
			while (opts.next_opt())
			{
				throw USER_EXCEPTION2(SE3022, (std::string("Invalid ftindex-dict option: '") + opts.opt_name() + "'").c_str());
			}

			options.op->next(t);
			if (!t.is_eos())
				throw XQUERY_EXCEPTION(SE1071);
		}

		switch (ft_idx->impl)
		{
		case ft_ind_native:
			ftc_res = new ftc_scan_words_result(ftc_idx);
			break;
		default:
			throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation");
		}

		first_time = false;
	}

	ftc_res->get_next_result(t);
	if (t.is_eos())
	{
		first_time = true;
		delete ftc_res;
		ftc_res = NULL;
	}
}

PPIterator*  PPFtIndexDict::do_copy(dynamic_context *_cxt_)
{
	PPFtIndexDict *res;
	if (options.op)
		res = se_new PPFtIndexDict(_cxt_, info, idx_name, options);
	else
		res = se_new PPFtIndexDict(_cxt_, info, idx_name);
    res->idx_name.op = idx_name.op->copy(_cxt_);
	if (options.op)
		res->options.op = options.op->copy(_cxt_);
	return res;
}

void PPFtIndexDict::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    idx_name.op->accept(v);
    if(options.op)
        options.op->accept(v);
    v.pop();
}
