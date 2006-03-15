#include "PPFtHighlight.h"
#include "FTsearch.h"

PPFtHighlight::PPFtHighlight(variable_context *_cxt_,
                PPOpIn _seq_,
				PPOpIn _query_) :
						PPIterator(_cxt_),
						seq(_seq_),
						query(_query_),
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

    first_time = true;
}

void PPFtHighlight::reopen()
{
	seq.op->reopen();
    query.op->reopen();

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

		sj=new SednaSearchJob(&seq, ft_xml_hl, NULL, true);

		query.op->next(t);
		if (t.is_eos())
			throw USER_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !tc.is_string_type())
			throw USER_EXCEPTION(SE1071);

		sj->set_request(tc);
		query.op->next(t);
		if (!t.is_eos())
			throw USER_EXCEPTION(SE1071);

		first_time = false;
	}

	sj->get_next_result(t);
	if (t.is_eos())
	{
		delete sj;
		sj = NULL;
		first_time = true;
	}
}

PPIterator*  PPFtHighlight::copy(variable_context *_cxt_)
{
	PPFtHighlight *res;
	res = new PPFtHighlight(_cxt_, seq, query);
    res->seq.op = seq.op->copy(_cxt_);
    res->query.op = query.op->copy(_cxt_);

	return res;
}

bool PPFtHighlight::result(PPIterator* cur, variable_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFtScan::result");
}