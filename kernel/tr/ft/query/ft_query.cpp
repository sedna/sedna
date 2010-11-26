/*
 * File:  ft_query.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/ft_types.h"
#include "tr/ft/query/ft_query.h"
#include "tr/ft/query/ftq_lexer.h"
#include "tr/ft/query/ftq_parser.h"


void FtQueryTerm::open()
{
	ftc_scan.scan_word(term_buf);
}
uint64_t FtQueryTerm::get_next_result()
{
	uint64_t res;
	ftc_scan.get_next_result(&res);
	return res;
}
FtQueryTerm::~FtQueryTerm()
{
}



FtQueryAnd::FtQueryAnd(ftc_index_t idx, int _nops) : ftc_idx(idx), nops(_nops)
{
	U_ASSERT(nops > 1);
	ops = (FtQuery **)malloc(nops * sizeof(FtQuery*));
	op_results = (uint64_t *)malloc(nops * sizeof(uint64_t));
}

void FtQueryAnd::set_operand(int op_idx, FtQuery *op)
{
	ops[op_idx] = op;
}

void FtQueryAnd::open()
{
	for (int i = 0; i < nops; i++)
	{
		ops[i]->open();
		op_results[i] = ops[i]->get_next_result();
	}
}

uint64_t FtQueryAnd::get_next_result()
{
	U_ASSERT(nops > 1);
	int i = 1;
	if (op_results[0] == FT_UINT_NULL)
		return FT_UINT_NULL;
	while (i < nops)
	{
		while (op_results[i] < op_results[i-1])
		{
			op_results[i] = ops[i]->get_next_result();
			if (op_results[i] == FT_UINT_NULL)
				return FT_UINT_NULL;
		}
		if (op_results[i] > op_results[i-1])
		{
			op_results[0] = ops[0]->get_next_result();
			if (op_results[0] == FT_UINT_NULL)
				return FT_UINT_NULL;
			i = 1;
		}
		else
			i++;
	}

	uint64_t res = op_results[0];
	for (int i = 0; i < nops; i++)
	{
		U_ASSERT(op_results[i] == res);
		op_results[i] = ops[i]->get_next_result();
	}
	return res;
}

FtQueryAnd::~FtQueryAnd()
{
	for (int i = 0; i < nops; i++)
		delete ops[i];
	delete ops;
}




FtQueryProcessor::~FtQueryProcessor()
{
	if (query != NULL)
		delete query;
}

void FtQueryProcessor::set_query(str_cursor *cur)
{
	query = ft_parse_query(cur, this->ftc_idx);
	query_opened = false;
}

void FtQueryProcessor::get_next_result(tuple &t)
{
	if (!query_opened)
	{
		query->open();
		query_opened = true;
	}
	uint64_t res = query->get_next_result();
	if (res == FT_UINT_NULL)
		t.set_eos();
	else
		t.copy(tuple_cell::node(indirectionDereferenceCP(FT_UINT_TO_XPTR(res))));
}
