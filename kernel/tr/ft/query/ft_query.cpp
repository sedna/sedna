/*
 * File:  ft_query.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/ft_types.h"
#include "tr/ft/query/ft_query.h"
#include "tr/ft/query/ftq_lexer.h"
#include "tr/ft/query/ftq_parser.h"
#include "tr/ft/sequence_sorter.h"

#include <math.h>

ft_float doc_length_norm(int64_t len)
{
	return 1.0f / sqrtf(len);
}

FtQueryTerm::~FtQueryTerm()
{
}

void FtQueryTerm::do_open()
{
	this->score_count = 1;
	ftc_scan.scan_word(term_buf);
}
void FtQueryTerm::init(ft_float *_scores)
{
	this->scores = _scores;
}
uint64_t FtQueryTerm::get_next_result()
{
	uint64_t res;
	int noccurs;
	ftc_scan.get_next_result(&res, &noccurs);
	if (res != FT_UINT_NULL)
	{
		this->scores[0] = sqrtf(noccurs) * doc_length_norm(ftc_scan.get_doc_len(res));
		this->doc_freq++;
	}
	return res;
}
void FtQueryTerm::close()
{
	//count remaining terms to update doc_freq for scoring
	while (this->get_next_result() != FT_UINT_NULL) ;
}
ft_float FtQueryTerm::ft_get_score(ft_float *scores)
{
	int naccs = ftc_get_doc_count(ftc_scan.get_ftc_idx());
	ft_float idf = 1+logf(naccs / (this->doc_freq + 1));
	ft_float tf_and_doc_norm = scores[0];
	return tf_and_doc_norm * idf * idf;
}
void FtQueryTerm::get_next_occur(ft_uint_t *acc, int *word_ind)
{
	ftc_scan.get_next_occur(acc, word_ind);
}


FtQueryAnd::FtQueryAnd(ftc_index_t idx, int _nops) : ftc_idx(idx), nops(_nops)
{
	U_ASSERT(nops > 1);
	ops = (FtQuery **)malloc(nops * sizeof(FtQuery*));
	op_results = (uint64_t *)malloc(nops * sizeof(uint64_t));
}
FtQueryAnd::~FtQueryAnd()
{
	for (int i = 0; i < nops; i++)
		delete ops[i];
	delete ops;
	delete op_results;
}

void FtQueryAnd::set_operand(int op_idx, FtQuery *op)
{
	ops[op_idx] = op;
}

void FtQueryAnd::do_open()
{
	this->score_count = 0;
	for (int i = 0; i < nops; i++)
	{
		ops[i]->open();
		this->score_count += ops[i]->score_count;
	}
}
void FtQueryAnd::init(float *_scores)
{
	int p = 0;
	this->scores = _scores;
	for (int i = 0; i < nops; i++)
	{
		ops[i]->init(&this->scores[p]);
		p += ops[i]->score_count;
		op_results[i] = ops[i]->get_next_result();
	}
	U_ASSERT(p == this->score_count);
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
void FtQueryAnd::close()
{
	//FIXME: FtQueryAnd doesn't update doc_freq, so no need to finish scanning here
	//       need to specify behaviour of FtQuery classes to make sure it's ok
	for (int i = 0; i < nops; i++)
	{
		ops[i]->close();
	}
}
ft_float FtQueryAnd::ft_get_score(ft_float *scores)
{
	int p = 0;
	ft_float res = 0;
	for (int i = 0; i < nops; i++)
	{
		res += ops[i]->ft_get_score(&this->scores[p]);
		p += ops[i]->score_count;
	}
	U_ASSERT(p == this->score_count);
	return res;
}


FtQueryPhrase::FtQueryPhrase(int _nops) : nops(_nops)
{
	U_ASSERT(nops > 1);
	term_ops = (FtQueryTerm **)malloc(nops * sizeof(FtQueryTerm*));
	op_results = (uint64_t *)malloc(nops * sizeof(uint64_t));
	op_word_inds = (int *)malloc(nops * sizeof(int));
}
FtQueryPhrase::~FtQueryPhrase()
{
	for (int i = 0; i < nops; i++)
		delete term_ops[i];
	delete term_ops;
}
void FtQueryPhrase::set_term(int op_idx, FtQueryTerm *t)
{
	term_ops[op_idx] = t;
}

void FtQueryPhrase::do_open()
{
	this->score_count = 1;
	for (int i = 0; i < nops; i++)
	{
		term_ops[i]->open();
		this->score_count += term_ops[i]->score_count;
	}
}

void FtQueryPhrase::init(ft_float *_scores)
{
	int p = 1;
	this->scores = _scores;
	for (int i = 0; i < nops; i++)
	{
		term_ops[i]->init(&this->scores[p]);
		p += term_ops[i]->score_count;
		op_results[i] = FT_UINT_NULL;
		term_ops[i]->get_next_occur(&op_results[i], &op_word_inds[i]);
	}
	U_ASSERT(p == this->score_count);
}

bool FtQueryPhrase::next_occur()
{
	//TODO: set initial value for second argument to get_next_occur to improve performance
	//XXX: try to remove goto-s

	U_ASSERT(nops > 1);
	int i = 1;
	if (op_results[0] == FT_UINT_NULL)
		return false;

	while (i < nops)
	{
		if (op_results[i] < op_results[i-1])
		{
			op_results[i] = op_results[i-1];
			op_word_inds[i] = 0; //FIXME
			term_ops[i]->get_next_occur(&op_results[i], &op_word_inds[i]);
			if (op_results[i] == FT_UINT_NULL)
				return false;
			U_ASSERT(op_results[i] >= op_results[i-1]);
		}
label_a:
		if (op_results[i] > op_results[i-1])
		{
			op_results[0] = op_results[i];
			op_word_inds[0] = 0; //FIXME
			term_ops[0]->get_next_occur(&op_results[0], &op_word_inds[0]);
			if (op_results[0] == FT_UINT_NULL)
				return false;
			i = 1;
			continue;
		}
		U_ASSERT(op_results[i] == op_results[i-1]);
		//i'th term occur is too far left from the rest
		if (op_word_inds[i] < op_word_inds[i-1]+1)
		{
			op_word_inds[i] = op_word_inds[i-1]+1;
			term_ops[i]->get_next_occur(&op_results[i], &op_word_inds[i]);
			if (op_results[i] == FT_UINT_NULL)
				return false;
			goto label_a;
		}
		//i'th term occur is too far right from the rest
		if (op_word_inds[i] > op_word_inds[i-1]+1)
		{
			//FIXME: set op_word_inds[0]
			term_ops[0]->get_next_occur(&op_results[0], &op_word_inds[0]);
			if (op_results[0] == FT_UINT_NULL)
				return false;
			i = 1;
			continue;
		}
		U_ASSERT(op_word_inds[i] == op_word_inds[i-1]+1);
		i++;
	}
	return true;
}

uint64_t FtQueryPhrase::get_next_result()
{
	if (!next_occur())
		return FT_UINT_NULL;

	ft_uint_t res = op_results[0];
	int noccurs = 1;
	while (op_results[0] == res)
	{
		term_ops[0]->get_next_occur(&op_results[0], &op_word_inds[0]);
		if (next_occur() && op_results[0] == res)
			noccurs++;
		else
			break;
	}

	this->scores[0] = sqrtf(noccurs) * doc_length_norm(term_ops[0]->get_doc_len(res));
	this->doc_freq++;
	return res;
}
void FtQueryPhrase::close()
{
	for (int i = 0; i < nops; i++)
	{
		term_ops[i]->close();
	}
}
ft_float FtQueryPhrase::ft_get_score(ft_float *scores)
{
	U_ASSERT(nops > 0);
	int naccs = ftc_get_doc_count(term_ops[0]->get_ftc_idx());
	ft_float idf = 1+logf(naccs / (this->doc_freq + 1));
	ft_float tf_and_doc_norm = scores[0];
	return tf_and_doc_norm * idf * idf;
}


FtQueryProcessor::~FtQueryProcessor()
{
	if (query != NULL)
		delete query;
	if (scores_buf != NULL)
		delete[] scores_buf;
	if (ss != NULL)
		delete ss;
}

void FtQueryProcessor::set_query(str_cursor *cur)
{
	query = ft_parse_query(cur, this->ftc_idx);
	query_opened = false;
}

const int ssr_n = 2;
const sequence_sorter::sort_type ssr_types[ssr_n] = {sequence_sorter::st_uint64_desc, sequence_sorter::st_uint64};
const int ssr_inds[ssr_n] = {0, 1};

void FtQueryProcessor::get_next_result(tuple &t)
{
	//TODO: make ranking optional
	if (!query_opened)
	{
		query->open();
		if (scores_buf != NULL)
			delete[] scores_buf;
		scores_buf = new ft_float[query->score_count];
		query->init(scores_buf);

		query_opened = true;

		//get all results and sort them
		SblobWriter sw(false);
		xptr scores = sw.create_new();
		while (true)
		{
			uint64_t res = query->get_next_result();
			if (res == FT_UINT_NULL)
				break;
			sw.write_uint(res);
			sw.write((char*)scores_buf, sizeof(ft_float) * query->score_count);
		}
		query->close();
		sw.flush();
		SblobReader r;
		r.init(scores);

		ssr.create_sorted_sequence(ssr_n, ssr_types, ssr_inds);
		while (!r.eos())
		{
			tuple t(2);
			uint64_t res = r.read_uint();
			r.read_bytes((char*)scores_buf, sizeof(ft_float) * query->score_count);

			ft_float score = query->ft_get_score(scores_buf);

			double score_d = (double)score;
			int64_t score_i = *(int64_t*)&score_d; //FIXME: add float/double to sequence_sorter
			t.cells[0] = tuple_cell::atomic((int64_t)score_i);
			t.cells[1] = tuple_cell::atomic((int64_t)res);
			ssr.add(t);
		}
		ss = ssr.get_sorted_sequence();
		ss->lazy_sort();
	}


	if (ss != NULL)
	{
		tuple res(ssr_n);
		ss->next(res);
		if (res.is_eos())
		{
			t.set_eos();
			delete ss;
			ss = NULL;
			return;
		}
		int64_t score_i = res.cells[0].get_xs_integer();
		double score_d = *(double*)&score_i;
		uint64_t res_acc_i = res.cells[1].get_xs_integer();
		t.copy(tuple_cell::node(indirectionDereferenceCP(FT_UINT_TO_XPTR(res_acc_i))));
	}
	else
		t.set_eos();


/*	uint64_t res = query->get_next_result();
	if (res == FT_UINT_NULL)
		t.set_eos();
	else
		t.copy(tuple_cell::node(indirectionDereferenceCP(FT_UINT_TO_XPTR(res))));
*/
}
