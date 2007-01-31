/*
 * File:  PPFtScan.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFtScan.h"
#include "tr/ft/FTsearch.h"
#include "tr/executor/root/PPCreateFtIndex.h"

PPFtScan::PPFtScan(dynamic_context *_cxt_,
                PPOpIn _seq_,
				PPOpIn _query_,
				PPOpIn _index_type_,
				PPOpIn _cust_rules_) :
						PPIterator(_cxt_),
						seq(_seq_),
						query(_query_),
						index_type(_index_type_),
						cust_rules(_cust_rules_),
						sj(NULL), ptr(NULL)
{
}

PPFtScan::PPFtScan(dynamic_context *_cxt_,
                PPOpIn _seq_,
				PPOpIn _query_,
				PPOpIn _index_type_) :
						PPIterator(_cxt_),
						seq(_seq_),
						query(_query_),
						index_type(_index_type_),
						sj(NULL), ptr(NULL)
{
	cust_rules.op = NULL;
}

PPFtScan::~PPFtScan()
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
	if (index_type.op)
    {
        delete index_type.op;
        index_type.op = NULL;
    }
	if (cust_rules.op)
    {
        delete cust_rules.op;
        cust_rules.op = NULL;
    }
	if (sj)
	{
		delete sj;
		sj = NULL;
	}
}

void PPFtScan::open()
{
	seq.op->open();
    query.op->open();
	index_type.op->open();
	if (cust_rules.op)
		cust_rules.op->open();

    first_time = true;
}

void PPFtScan::reopen()
{
	seq.op->reopen();
    query.op->reopen();
	index_type.op->reopen();
	if (cust_rules.op)
		cust_rules.op->reopen();

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

void PPFtScan::close()
{
	seq.op->close();
    query.op->close();
	index_type.op->close();
	if (cust_rules.op)
		cust_rules.op->close();
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

void PPFtScan::next(tuple &t)
{
	if (first_time)
	{
		tuple_cell tc;

		index_type.op->next(t);
		if (t.is_eos())
			throw USER_EXCEPTION(SE1071);
		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw USER_EXCEPTION(SE1071);
		tc = tuple_cell::make_sure_light_atomic(tc);
		ft_index_type itype = str2index_type(tc.get_str_mem());


		if (cust_rules.op)
		{
		ptr = pers_sset<ft_custom_cell,unsigned short>::init(false);

		std::vector<std::pair<std::pair<xml_ns*,char*>,ft_index_type> > * templ = make_cust_rules_vector(&cust_rules, cxt);
		std::vector<std::pair<std::pair<xml_ns*,char*>,ft_index_type> >::iterator tmp=templ->begin();
		while (tmp!=templ->end())
		{
			
			ptr->put(ft_custom_cell::init((*tmp).first.first,(*tmp).first.second,(*tmp).second,false));
			tmp++;
		}
		delete_cust_rules_vector(templ);
		}
		sj=new SednaSearchJob(&seq, itype, ptr);


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

	sj->get_next_result(t);
	if (t.is_eos())
	{
		delete sj;
		sj = NULL;
		first_time = true;
	}
}

PPIterator*  PPFtScan::copy(dynamic_context *_cxt_)
{
	PPFtScan *res;
	if (cust_rules.op)
		res = new PPFtScan(_cxt_, seq, query, index_type, cust_rules);
	else
		res = new PPFtScan(_cxt_, seq, query, index_type);
    res->seq.op = seq.op->copy(_cxt_);
    res->query.op = query.op->copy(_cxt_);
    res->index_type.op = index_type.op->copy(_cxt_);
	if (cust_rules.op)
		res->cust_rules.op = cust_rules.op->copy(_cxt_);

	return res;
}

bool PPFtScan::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFtScan::result");
}