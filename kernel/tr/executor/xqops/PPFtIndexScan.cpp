/*
 * File:  PPFtIndexScan.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFtIndexScan.h"
#ifdef SE_ENABLE_DTSEARCH
#include "tr/ft/FTsearch.h"
#endif

PPFtIndexScan::PPFtIndexScan(dynamic_context *_cxt_,
                PPOpIn _idx_name_,
				PPOpIn _query_) :
						PPIterator(_cxt_),
						idx_name(_idx_name_),
						query(_query_),
#ifdef SE_ENABLE_DTSEARCH
						sj(NULL),
#endif
						ftc_res(NULL)
{
	options.op = NULL;
}
PPFtIndexScan::PPFtIndexScan(dynamic_context *_cxt_,
            PPOpIn _idx_name_,
            PPOpIn _query_,
			PPOpIn _options_) :
						PPIterator(_cxt_),
						idx_name(_idx_name_),
						query(_query_),
						options(_options_),
#ifdef SE_ENABLE_DTSEARCH
						sj(NULL),
#endif
						ftc_res(NULL)
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
	if (ftc_res)
	{
		delete ftc_res;
		ftc_res = NULL;
	}
}

void PPFtIndexScan::open()
{
	idx_name.op->open();
    query.op->open();
	if (options.op)
		options.op->open();

    first_time = true;
}

void PPFtIndexScan::reopen()
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
	if (ftc_res)
	{
		delete ftc_res;
		ftc_res = NULL;
	}

    first_time = true;
}

void PPFtIndexScan::close()
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
	if (ftc_res != NULL)
	{
		delete ftc_res;
		ftc_res = NULL;
	}
	
}

//TODO: move OptionsParser to strings to strings
//parses options in the form "Name[=Value]{,Name[=Value]}" Name - consinsts of letters, Value - anything except ','; all whitespace is stripped (i.e. "A a=1 1" equiv to "Aa=11")
//FIXME: currenty "Name=" is equivalent to "Name"
class OptionsParser
{
protected:
	static const int opt_name_buf_size = 32;
	char opt_name_buf[opt_name_buf_size];
	static const int opt_value_buf_size = 32;
	char opt_value_buf[opt_value_buf_size];
	unicode_cp_iterator *ucpi;
	int next_char;

	void reset()
	{
		if (ucpi != NULL)
		{
			delete ucpi;
			ucpi = NULL;
		}
		opt_name_buf[0] = 0;
		opt_value_buf[0] = 0;
		next_char = unicode_cp_iterator::EOS;
	}

	void get_next_nonws_char()
	{
		U_ASSERT(ucpi != NULL);
		do
		{
			next_char = ucpi->get_next_char();
		} while (next_char != unicode_cp_iterator::EOS && iswspace(next_char));
		if (next_char == unicode_cp_iterator::EOS)
		{
			delete ucpi;
			ucpi = NULL;
		}
	}

public:
	OptionsParser() : ucpi(NULL)
	{
	}

	void set_tc(const tuple_cell *tc)
	{
		reset();
		ucpi = charset_handler->get_unicode_cp_iterator(tc);
	}

	~OptionsParser()
	{
		reset();
	}

	bool next_opt()
	{
		while (ucpi != NULL && (next_char == unicode_cp_iterator::EOS || next_char == ','))
			get_next_nonws_char();
		if (next_char == unicode_cp_iterator::EOS)
		{
			U_ASSERT(ucpi == NULL);
			opt_name_buf[0] = 0;
			opt_value_buf[0] = 0;
			return false;
		}
		int name_pos = 0;
		int value_pos = 0;

		while (next_char != unicode_cp_iterator::EOS && next_char != ',' && next_char != '=')
		{
			CharsetHandler_utf8::utf8_putch(next_char, opt_name_buf, &name_pos, opt_name_buf_size-1);
			get_next_nonws_char();
		}
		if (next_char == '=')
		{
			while (next_char != unicode_cp_iterator::EOS && next_char != ',')
			{
				CharsetHandler_utf8::utf8_putch(next_char, opt_value_buf, &value_pos, opt_value_buf_size-1);
				get_next_nonws_char();
			}
		}
		U_ASSERT(name_pos < opt_name_buf_size);
		U_ASSERT(value_pos < opt_value_buf_size);
		opt_name_buf[name_pos] = 0;
		opt_value_buf[value_pos] = 0;
	}
	const char *opt_name()
	{
		return opt_name_buf;
	}
	const char *opt_value()
	{
		return opt_value_buf;
	}
	//empty or omiited value results in true
	bool opt_value_as_bool()
	{
		//FIXME: throw exception for invalid args?
		//FIXME: 00 is true
		if (!opt_value_buf[0])
			return true;
		if ( (opt_value_buf[0] == '0' && !opt_value_buf[1])
			|| (toupper(opt_value_buf[0]) == 'N' && toupper(opt_value_buf[1]) == 'O' && !opt_value_buf[2])
			|| (toupper(opt_value_buf[0]) == 'O' && toupper(opt_value_buf[1]) == 'F' && toupper(opt_value_buf[2]) == 'F' && !opt_value_buf[3])
			)
			return false;
		return true;
	}
};

void PPFtIndexScan::next(tuple &t)
{
	SET_CURRENT_PP(this);

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
			ftc_res = se_new ftc_scan_result(ftc_idx);
			ftc_res->scan_word(op_str_buf(tc).c_str()); //FIXME: op_str_buf may be destroyed too soon, pass char* to SednaSearchJob too
			break;
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
		ftc_res->get_next_result(t);
		if (t.is_eos())
		{
			delete ftc_res;
			ftc_res = NULL;
			first_time = true;
		}
	}
#else
	ftc_res->get_next_result(t);
	if (t.is_eos())
	{
		delete ftc_res;
		ftc_res = NULL;
		first_time = true;
	}
#endif

	RESTORE_CURRENT_PP;
}

PPIterator*  PPFtIndexScan::copy(dynamic_context *_cxt_)
{
	PPFtIndexScan *res;
	if (options.op)
		res = se_new PPFtIndexScan(_cxt_, idx_name, query, options);
	else
		res = se_new PPFtIndexScan(_cxt_, idx_name, query);
    res->idx_name.op = idx_name.op->copy(_cxt_);
    res->query.op = query.op->copy(_cxt_);
	if (options.op)
		res->options.op = options.op->copy(_cxt_);
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
#ifdef SE_ENABLE_DTSEARCH
						sj(NULL),
#endif
						ftc_res(NULL)
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
#ifdef SE_ENABLE_DTSEARCH
						sj(NULL),
#endif
						ftc_res(NULL)
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

void PPFtIndexScan2::close()
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

void PPFtIndexScan2::next(tuple &t)
{
	SET_CURRENT_PP(this);

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
