#include "PPFtIndexScan.h"
#include "FTsearch.h"

PPFtIndexScan::PPFtIndexScan(variable_context *_cxt_,
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
	if (first_time)
	{
		tuple_cell tc;

		query.op->next(t);
		if (t.is_eos())
			throw USER_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !tc.is_string_type())
			throw USER_EXCEPTION(SE1071);
		sj=new SednaSearchJob();
		sj->set_request(tc);
		query.op->next(t);
		if (!t.is_eos())
			throw USER_EXCEPTION(SE1071);

		idx_name.op->next(t);
		if (t.is_eos())
			throw USER_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !tc.is_string_type())
			throw USER_EXCEPTION(SE1071);
		sj->set_index(tc);
		idx_name.op->next(t);
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

PPIterator*  PPFtIndexScan::copy(variable_context *_cxt_)
{
	PPFtIndexScan *res = new PPFtIndexScan(_cxt_, idx_name, query);
    res->idx_name.op = idx_name.op->copy(_cxt_);
    res->query.op = query.op->copy(_cxt_);

	return res;
}

bool PPFtIndexScan::result(PPIterator* cur, variable_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFtIndexScan::result");
}