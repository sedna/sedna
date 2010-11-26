/*
 * File:  PPFtIndexScan.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFtIndexScan.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/strings/opt_parser.h"
#ifdef SE_ENABLE_DTSEARCH
#include "tr/ft/FTsearch.h"
#endif

PPFtIndexScan::PPFtIndexScan(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _idx_name_,
                             PPOpIn _query_) : PPIterator(_cxt_, _info_, "PPFtIndexScan"),
                                               idx_name(_idx_name_),
                                               query(_query_),
#ifdef SE_ENABLE_DTSEARCH
                                               sj(NULL),
#endif
                                               ftq(NULL)
{
	options.op = NULL;
}
PPFtIndexScan::PPFtIndexScan(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _idx_name_,
                             PPOpIn _query_,
                             PPOpIn _options_) : PPIterator(_cxt_, _info_, "PPFtIndexScan"),
                                                 idx_name(_idx_name_),
                                                 query(_query_),
                                                 options(_options_),
#ifdef SE_ENABLE_DTSEARCH
                                                 sj(NULL),
#endif
                                                 ftq(NULL)
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
	if (options.op)
	{
		delete options.op;
		options.op = NULL;
	}
#ifdef SE_ENABLE_DTSEARCH
	if (sj)
	{
		delete sj;
		sj = NULL;
	}
#endif
	if (ftq)
	{
		delete ftq;
		ftq = NULL;
	}
}

void PPFtIndexScan::do_open()
{
	idx_name.op->open();
    query.op->open();
	if (options.op)
		options.op->open();

    first_time = true;
}

void PPFtIndexScan::do_reopen()
{
	idx_name.op->reopen();
    query.op->reopen();
	if (options.op)
		options.op->reopen();

#ifdef SE_ENABLE_DTSEARCH
	if (sj)
	{
		delete sj;
		sj = NULL;
	}
#endif
	if (ftq != NULL)
	{
		delete ftq;
		ftq = NULL;
	}

    first_time = true;
}

void PPFtIndexScan::do_close()
{
	idx_name.op->close();
    query.op->close();
	if (options.op)
		options.op->close();
#ifdef SE_ENABLE_DTSEARCH
	if (sj != NULL)
	{
		delete sj;
		sj = NULL;
	}
#endif
	if (ftq != NULL)
	{
		delete ftq;
		ftq = NULL;
	}
	
}

void PPFtIndexScan::do_next(tuple &t)
{
    if (first_time)
	{
#ifdef SE_ENABLE_DTSEARCH
		bool opt_dtsSearchAnyWords = false;
		bool opt_dtsSearchAllWords = false;
#endif
		tuple_cell tc;

		idx_name.op->next(t);
		if (t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw XQUERY_EXCEPTION(SE1071);

		ftc_index_t ftc_idx;
		op_str_buf tmp_buf(tc);
		const char *index_title = tmp_buf.c_str();
		//TODO: set lock mode
		get_schema_node(find_db_entity_for_object(catobj_ft_indicies, index_title),
						"Unknown database entity for full-text index");
		ft_index_cell_cptr ft_idx = find_ft_index(index_title, &ftc_idx);
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
#ifdef SE_ENABLE_DTSEARCH
				if (!strcmp(opts.opt_name(), "dtsSearchAnyWords"))
					opt_dtsSearchAnyWords = opts.opt_value_as_bool();
				else if (!strcmp(opts.opt_name(), "dtsSearchAllWords"))
					opt_dtsSearchAllWords = opts.opt_value_as_bool();
				else
#endif
				throw USER_EXCEPTION2(SE3022, (std::string("Invalid ftindex-scan option: '") + opts.opt_name() + "'").c_str());
			}

			options.op->next(t);
			if (!t.is_eos())
				throw XQUERY_EXCEPTION(SE1071);
		}

		query.op->next(t);
		if (t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw XQUERY_EXCEPTION(SE1071);

		switch (ft_idx->impl)
		{
#ifdef SE_ENABLE_DTSEARCH
		case ft_ind_dtsearch:
			sj=se_new SednaSearchJob();
			sj->set_dtsSearchAnyWords(opt_dtsSearchAnyWords);
			sj->set_dtsSearchAllWords(opt_dtsSearchAllWords);
			sj->set_index(&(*ft_idx));
			sj->set_request(tc);
			break;
#endif
		case ft_ind_native:
			{
			ftq = new FtQueryProcessor(ftc_idx);
			str_cursor *query_cur = get_text_cursor(text_source_tuple_cell(tc));
			ftq->set_query(query_cur);
			delete query_cur;
			break;
			}
		default:
			throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation");
		}

		query.op->next(t);
		if (!t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);

		first_time = false;
	}

	//FIXME
#ifdef SE_ENABLE_DTSEARCH
	if (sj != NULL)
	{
		sj->get_next_result(t);
		if (t.is_eos())
		{
			delete sj;
			sj = NULL;
			first_time = true;
		}
	}
	else
	{
		ftq->get_next_result(t);
		if (t.is_eos())
		{
			delete ftq;
			ftq = NULL;
			first_time = true;
		}
	}
#else
	ftq->get_next_result(t);
	if (t.is_eos())
	{
		delete ftq;
		ftq = NULL;
		first_time = true;
	}
#endif
}

PPIterator*  PPFtIndexScan::do_copy(dynamic_context *_cxt_)
{
	PPFtIndexScan *res;
	if (options.op)
		res = se_new PPFtIndexScan(_cxt_, info, idx_name, query, options);
	else
		res = se_new PPFtIndexScan(_cxt_, info, idx_name, query);
    res->idx_name.op = idx_name.op->copy(_cxt_);
    res->query.op = query.op->copy(_cxt_);
	if (options.op)
		res->options.op = options.op->copy(_cxt_);
	return res;
}


void PPFtIndexScan::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    idx_name.op->accept(v);
    query.op->accept(v);
    if(options.op)
        options.op->accept(v);
    v.pop();
}



//////////////////////////////////////////////
//// PPFtIndexScan2
//////////////////////////////////////////////

PPFtIndexScan2::PPFtIndexScan2(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _idx_name_,
                               PPOpIn _query_) : PPIterator(_cxt_, _info_, "PPFtIndexScan2"),
                                                 idx_name(_idx_name_),
                                                 query(_query_),
#ifdef SE_ENABLE_DTSEARCH
                                                 sj(NULL),
#endif
                                                 ftc_res(NULL)
{
	max_results.op = NULL;
	field_weights.op = NULL;
}

PPFtIndexScan2::PPFtIndexScan2(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _idx_name_,
                               PPOpIn _query_,
                               PPOpIn _max_results_) : PPIterator(_cxt_, _info_, "PPFtIndexScan2"),
                                                       idx_name(_idx_name_),
                                                       query(_query_),
                                                       max_results(_max_results_),
#ifdef SE_ENABLE_DTSEARCH
                                                       sj(NULL),
#endif
                                                       ftc_res(NULL)
{
	field_weights.op = NULL;
}

PPFtIndexScan2::PPFtIndexScan2(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _idx_name_,
                               PPOpIn _query_,
                               PPOpIn _max_results_,
                               PPOpIn _field_weights_) : PPIterator(_cxt_, _info_, "PPFtIndexScan2"),
                                                         idx_name(_idx_name_),
                                                         query(_query_),
                                                         max_results(_max_results_),
                                                         field_weights(_field_weights_),
#ifdef SE_ENABLE_DTSEARCH
                                                         sj(NULL),
#endif
                                                         ftc_res(NULL)
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
#ifdef SE_ENABLE_DTSEARCH
	if (sj)
	{
		delete sj;
		sj = NULL;
	}
#endif
	if (ftc_res)
	{
		delete ftc_res;
		ftc_res = NULL;
	}
}

void PPFtIndexScan2::do_open()
{
	idx_name.op->open();
    query.op->open();
	if (max_results.op)
		max_results.op->open();
	if (field_weights.op)
		field_weights.op->open();

    first_time = true;
}

void PPFtIndexScan2::do_reopen()
{
	idx_name.op->reopen();
    query.op->reopen();
	if (max_results.op)
		max_results.op->reopen();
	if (field_weights.op)
		field_weights.op->reopen();

#ifdef SE_ENABLE_DTSEARCH
	if (sj)
	{
		delete sj;
		sj = NULL;
	}
#endif
	if (ftc_res)
	{
		delete ftc_res;
		ftc_res = NULL;
	}

    first_time = true;
}

void PPFtIndexScan2::do_close()
{
	idx_name.op->close();
    query.op->close();
	if (max_results.op)
		max_results.op->close();
	if (field_weights.op)
		field_weights.op->close();
#ifdef SE_ENABLE_DTSEARCH
	if (sj != NULL)
	{
		delete sj;
		sj = NULL;
	}
#endif
	if (ftc_res != NULL)
	{
		delete ftc_res;
		ftc_res = NULL;
	}
}

void PPFtIndexScan2::do_next(tuple &t)
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

		query.op->next(t);
		if (t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw XQUERY_EXCEPTION(SE1071);

		query.op->next(t);
		if (!t.is_eos())
			throw XQUERY_EXCEPTION(SE1071);

		switch (ft_idx->impl)
		{
#ifdef SE_ENABLE_DTSEARCH
		case ft_ind_dtsearch:
			sj=se_new SednaSearchJob2();
			sj->set_index(&(*ft_idx));
			sj->set_request(tc);

			//TODO!!: do not ignore these for other implementations!!
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


			break;
#endif
		case ft_ind_native:
			//TODO!
			throw USER_EXCEPTION2(SE1002, "ftwindex-scan not implemented");
			break;
		default:
			throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation");
		}

		first_time = false;
	}

#ifdef SE_ENABLE_DTSEARCH
	if (sj != NULL)
	{
		sj->get_next_result(t);
		if (t.is_eos())
		{
			delete sj;
			sj = NULL;
			first_time = true;
		}
	}
	else
	{
		//TODO!
		throw USER_EXCEPTION2(SE1002, "ftwindex-scan not implemented");
	}
#else
	//TODO!
	throw USER_EXCEPTION2(SE1002, "ftwindex-scan not implemented");
#endif
}


PPIterator*  PPFtIndexScan2::do_copy(dynamic_context *_cxt_)
{
	//FIXME: mb using different constructors is better?
    PPFtIndexScan2 *res = se_new PPFtIndexScan2(_cxt_, info, idx_name, query, max_results, field_weights);
    res->idx_name.op = idx_name.op->copy(_cxt_);
    res->query.op = query.op->copy(_cxt_);
	if (max_results.op)
		res->max_results.op = max_results.op->copy(_cxt_);
	if (field_weights.op)
		res->field_weights.op = field_weights.op->copy(_cxt_);
	
	return res;
}

void PPFtIndexScan2::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    idx_name.op->accept(v);
    query.op->accept(v);
    if(max_results.op)
        max_results.op->accept(v);
    if(field_weights.op)
        field_weights.op->accept(v);
    v.pop();
}
