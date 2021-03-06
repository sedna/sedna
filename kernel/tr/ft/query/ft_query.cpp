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

//value used to fill scores to indicate no match
const ft_float no_score = 0;

FtWordIndexList::~FtWordIndexList()
{
	if (buf != NULL)
	{
		free(buf);
		buf = NULL;
	}
}

void FtWordIndexList::merge(FtWordIndexList *lists, int nlists)
{
	ft_word_ind_t **p = (ft_word_ind_t **)malloc(nlists * sizeof(ft_word_ind_t *));
	size_t *left = (size_t *)malloc(nlists * sizeof(size_t));

	this->clear();

	for (int i = 0; i < nlists; i++)
	{
		p[i] = lists[i].get_inds();
		left[i] = lists[i].get_inds_count();
	}

	//TODO: init limit

	while (true)
	{
		int mini = -1;
		for (int i = 0; i < nlists; i++)
			if (left[i] > 0 && (mini == -1 || *(p[mini]) > *(p[i])))
				mini = i;
		if (mini == -1)
			break;
		const ft_word_ind_t ind = *(p[mini]);
		for (int i = 0; i < nlists; i++)
			if (left[i] > 0 && *(p[i]) == ind)
			{
				left[i]--;
				p[i]++;
			}
			this->add_ind(ind);
	}

	free(p);
	free(left);
}
void FtWordIndexList::merge2(FtWordIndexList *lists, int nlists, ft_acc_uint_t *listv, ft_acc_uint_t v)
{
	ft_word_ind_t **p = (ft_word_ind_t **)malloc(nlists * sizeof(ft_word_ind_t *));
	size_t *left = (size_t *)malloc(nlists * sizeof(size_t));

	this->clear();

	for (int i = 0; i < nlists; i++)
		if (listv[i] == v)
		{
			p[i] = lists[i].get_inds();
			left[i] = lists[i].get_inds_count();
		}

	//TODO: init limit

	while (true)
	{
		int mini = -1;
		for (int i = 0; i < nlists; i++)
			if (listv[i] == v && left[i] > 0 && (mini == -1 || *(p[mini]) > *(p[i])))
				mini = i;
		if (mini == -1)
			break;
		const ft_word_ind_t ind = *(p[mini]);
		for (int i = 0; i < nlists; i++)
			if (listv[i] == v && left[i] > 0 && *(p[i]) == ind)
			{
				left[i]--;
				p[i]++;
			}
			this->add_ind(ind);
	}

	free(p);
	free(left);
}

ft_float doc_length_norm(int64_t len)
{
	return 1.0f / sqrtf(len);
}

FtQueryTerm::~FtQueryTerm()
{
}

void FtQueryTerm::do_init()
{
	this->score_count = 1;
}
void FtQueryTerm::do_open()
{
	if (!stem && ftc_get_fts_data(ftc_idx)->stem_type == ftst_both && !prefix)
	{
		int l = strlen(term_buf);
		if (l >= FT_MAX_WORD_LENGTH)
			l = FT_MAX_WORD_LENGTH-1;
		term_buf[l] = FT_NOSTEM_MARKER;
		term_buf[l+1] = '\x0';
	}
	ftc_scan.scan_word(term_buf, prefix);
	next_acc_i = FT_ACC_UINT_NULL;
	ftc_scan.get_next_occur(&next_acc_i, &next_ind);
}
ft_acc_uint_t FtQueryTerm::get_next_result()
{
	if (next_acc_i == FT_ACC_UINT_NULL)
		return FT_ACC_UINT_NULL;
	uint64_t res = next_acc_i;
	int noccurs = 0;
	if (this->word_list != NULL)
		this->word_list->clear();

	while (next_acc_i == res)
	{
		noccurs++;
		if (this->word_list != NULL)
			this->word_list->add_ind(next_ind);

		ftc_scan.get_next_occur(&next_acc_i, &next_ind);
	}

	this->scores[0] = sqrtf(noccurs * this->score_boost) * doc_length_norm(ftc_scan.get_doc_len(res));
	this->doc_freq++;

	return res;
}
void FtQueryTerm::close()
{
	//count remaining terms to update doc_freq for scoring
	while (this->get_next_result() != FT_ACC_UINT_NULL) ;
}
ft_float FtQueryTerm::ft_get_score(ft_float *scores)
{
	int naccs = ftc_get_doc_count(ftc_scan.get_ftc_idx());
	ft_float idf = 1+logf(naccs / (this->doc_freq + 1));
	ft_float tf_and_doc_norm = scores[0];
	return tf_and_doc_norm * idf * idf;
}
void FtQueryTerm::get_next_occur(ft_acc_uint_t *acc, ft_word_ind_t *word_ind)
{
	if (*acc == FT_ACC_UINT_NULL || next_acc_i > *acc || (*acc == next_acc_i && next_ind >= *word_ind))
	{
		*acc = next_acc_i;
		*word_ind = next_ind;
		ftc_scan.get_next_occur(&next_acc_i, &next_ind);
	}
	else
	{
		ftc_scan.get_next_occur(acc, word_ind);
		next_acc_i = FT_ACC_UINT_NULL;
		ftc_scan.get_next_occur(&next_acc_i, &next_ind);
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////
FtQueryTermInElement::~FtQueryTermInElement()
{
}

void FtQueryTermInElement::do_init()
{
	this->score_count = 1;
}
void FtQueryTermInElement::do_open()
{
	if (!stem && ftc_get_fts_data(ftc_idx)->stem_type == ftst_both && !prefix)
	{
		int l = strlen(term_buf);
		if (l >= FT_MAX_WORD_LENGTH)
			l = FT_MAX_WORD_LENGTH-1;
		term_buf[l] = FT_NOSTEM_MARKER;
		term_buf[l+1] = '\x0';
	}

	ftc_scan.scan_word(term_buf, prefix);
	ftc_scan_opentag.scan_word(opentag_buf, false);
	ftc_scan_closetag.scan_word(closetag_buf, false);

	opentag_acc_i = FT_ACC_UINT_NULL;
	opentag_ind = 0;
	ftc_scan_opentag.get_next_occur(&opentag_acc_i, &opentag_ind);

	closetag_acc_i = opentag_acc_i;
	closetag_ind = opentag_ind;
	ftc_scan_closetag.get_next_occur(&closetag_acc_i, &closetag_ind);

	U_ASSERT(closetag_acc_i == opentag_acc_i); //FIXME: all tags must be closed now

	next_acc_i = FT_ACC_UINT_NULL; //needed for get_next_result()
}
ft_acc_uint_t FtQueryTermInElement::get_next_result()
{
	if (next_acc_i == FT_ACC_UINT_NULL)
		this->get_next_occur(&next_acc_i, &next_ind);
	if (next_acc_i == FT_ACC_UINT_NULL)
		return FT_ACC_UINT_NULL;

	ft_acc_uint_t res = next_acc_i;
	int noccurs = 0;
	if (this->word_list != NULL)
		this->word_list->clear();
	while (next_acc_i == res)
	{
		noccurs++;
		if (this->word_list != NULL)
			this->word_list->add_ind(next_ind);
		this->get_next_occur(&next_acc_i, &next_ind);
	}

	this->scores[0] = sqrtf(noccurs * this->score_boost) * doc_length_norm(ftc_scan.get_doc_len(res));
	this->doc_freq++;

	return res;
}
void FtQueryTermInElement::close()
{
	//count remaining terms to update doc_freq for scoring
	while (this->get_next_result() != FT_ACC_UINT_NULL) ;
}
ft_float FtQueryTermInElement::ft_get_score(ft_float *scores)
{
	int naccs = ftc_get_doc_count(ftc_scan.get_ftc_idx());
	ft_float idf = 1+logf(naccs / (this->doc_freq + 1));
	ft_float tf_and_doc_norm = scores[0];
	return tf_and_doc_norm * idf * idf;
}
void FtQueryTermInElement::get_next_occur(ft_acc_uint_t *acc, ft_word_ind_t *word_ind)
{
	if (opentag_acc_i == FT_ACC_UINT_NULL)
	{
		*acc = FT_ACC_UINT_NULL;
		return;
	}
	ftc_scan.get_next_occur(acc, word_ind);

	while (true)
	{
		if (*acc == FT_ACC_UINT_NULL)
			return;

		//FIXME: assumes (opentag_acc_i == closetag_acc_i)
		if (*acc == opentag_acc_i && *word_ind >= opentag_ind && *word_ind < closetag_ind)
			return;

		if (*acc < opentag_acc_i || (*acc == opentag_acc_i && *word_ind < opentag_ind))
		{
			*acc = opentag_acc_i;
			*word_ind = opentag_ind;
			ftc_scan.get_next_occur(acc, word_ind);

			continue;
		}

		U_ASSERT(*acc > closetag_acc_i || (*acc == closetag_acc_i && *word_ind >= closetag_ind));

		ftc_scan_opentag.get_next_occur(&opentag_acc_i, &opentag_ind);
		closetag_acc_i = opentag_acc_i;
		closetag_ind = opentag_ind;
		ftc_scan_closetag.get_next_occur(&closetag_acc_i, &closetag_ind);

		U_ASSERT(closetag_acc_i == opentag_acc_i); //FIXME: all tags must be closed now
		if (opentag_acc_i == FT_ACC_UINT_NULL)
		{
			*acc = FT_ACC_UINT_NULL;
			return;
		}
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////


FtQueryAnd::FtQueryAnd(ftc_index_t idx, int _nops) : ftc_idx(idx), nops(_nops), op_word_lists(NULL)
{
	U_ASSERT(nops > 1);
	ops = (FtQuery **)malloc(nops * sizeof(FtQuery*));
	op_results = (uint64_t *)malloc(nops * sizeof(uint64_t));
}
FtQueryAnd::~FtQueryAnd()
{
	for (int i = 0; i < nops; i++)
		delete ops[i];
	free(ops);
	free(op_results);
	if (op_word_lists != NULL)
		delete[] op_word_lists;
}

void FtQueryAnd::set_operand(int op_idx, FtQuery *op)
{
	ops[op_idx] = op;
}

void FtQueryAnd::do_init()
{
	this->score_count = 0;
	for (int i = 0; i < nops; i++)
	{
		ops[i]->init();
		this->score_count += ops[i]->score_count;
	}
}
void FtQueryAnd::do_open()
{
	int p = 0;
	if (this->word_list != NULL)
	{
		if (op_word_lists != NULL)
			delete[] op_word_lists;
		op_word_lists = new FtWordIndexList[nops];
	}
	for (int i = 0; i < nops; i++)
	{
		if (this->word_list != NULL)
			ops[i]->open(&this->scores[p], &op_word_lists[i]);
		else
			ops[i]->open(&this->scores[p], NULL);
		p += ops[i]->score_count;
	}
	U_ASSERT(p == this->score_count);
}

ft_acc_uint_t FtQueryAnd::get_next_result()
{
	U_ASSERT(nops > 1);
	int i = 1;

	for (int i = 0; i < nops; i++)
		op_results[i] = ops[i]->get_next_result();

	if (op_results[0] == FT_ACC_UINT_NULL)
		return FT_ACC_UINT_NULL;
	while (i < nops)
	{
		while (op_results[i] < op_results[i-1])
		{
			op_results[i] = ops[i]->get_next_result();
			if (op_results[i] == FT_ACC_UINT_NULL)
				return FT_ACC_UINT_NULL;
		}
		if (op_results[i] > op_results[i-1])
		{
			op_results[0] = ops[0]->get_next_result();
			if (op_results[0] == FT_ACC_UINT_NULL)
				return FT_ACC_UINT_NULL;
			i = 1;
		}
		else
			i++;
	}

	uint64_t res = op_results[0];

	if (this->word_list != NULL)
		this->word_list->merge(op_word_lists, nops);

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
		res += ops[i]->ft_get_score(&scores[p]);
		p += ops[i]->score_count;
	}
	U_ASSERT(p == this->score_count);
	return res;
}

//////////////////////////////////////////////////////////////////////////////////////////////


//TODO: move all common suff from FtQueryAnd/FtQueryOr to superclass

FtQueryOr::FtQueryOr(ftc_index_t idx, int _nops) : ftc_idx(idx), nops(_nops), op_word_lists(NULL), scores_buf(NULL)
{
	U_ASSERT(nops > 1);
	ops = (FtQuery **)malloc(nops * sizeof(FtQuery*));
	op_results = (uint64_t *)malloc(nops * sizeof(uint64_t));
}
FtQueryOr::~FtQueryOr()
{
	for (int i = 0; i < nops; i++)
		delete ops[i];
	free(ops);
	free(op_results);
	if (op_word_lists != NULL)
		delete[] op_word_lists;
	if (this->scores_buf != NULL)
		free(this->scores_buf);
}

void FtQueryOr::set_operand(int op_idx, FtQuery *op)
{
	ops[op_idx] = op;
}

void FtQueryOr::do_init()
{
	this->score_count = 0;
	for (int i = 0; i < nops; i++)
	{
		ops[i]->init();
		this->score_count += ops[i]->score_count;
	}
}
void FtQueryOr::do_open()
{
	int p = 0;
	if (this->scores_buf != NULL)
		free(this->scores_buf);
	this->scores_buf = (ft_float *)malloc(sizeof(ft_float) * this->score_count);
	if (this->word_list != NULL)
	{
		if (op_word_lists != NULL)
			delete[] op_word_lists;
		op_word_lists = new FtWordIndexList[nops];
	}
	for (int i = 0; i < nops; i++)
	{
		if (this->word_list != NULL)
			ops[i]->open(&this->scores_buf[p], &op_word_lists[i]);
		else
			ops[i]->open(&this->scores_buf[p], NULL);
		p += ops[i]->score_count;
		op_results[i] = ops[i]->get_next_result();
	}
	U_ASSERT(p == this->score_count);
}

ft_acc_uint_t FtQueryOr::get_next_result()
{
	U_ASSERT(nops > 1);
	ft_acc_uint_t res = op_results[0];

	for (int i = 1; i < nops; i++)
		if (op_results[i] != FT_ACC_UINT_NULL && (res == FT_ACC_UINT_NULL || res > op_results[i]))
			res = op_results[i];

	if (res == FT_ACC_UINT_NULL)
		return FT_ACC_UINT_NULL;

	if (this->word_list != NULL)
		this->word_list->merge2(op_word_lists, nops, op_results, res);

	int p = 0;
	for (int i = 0; i < nops; i++)
	{
		if (op_results[i] == res)
		{
			for (int t=0; t < ops[i]->score_count; t++)
				this->scores[p+t] = this->scores_buf[p+t];
			op_results[i] = ops[i]->get_next_result();
		}
		else
		{
			for (int t=0; t < ops[i]->score_count; t++)
				this->scores[p+t] = no_score;
		}
		p += ops[i]->score_count;
	}
	U_ASSERT(p == this->score_count);
	return res;
}
void FtQueryOr::close()
{
	//FIXME: FtQueryOr doesn't update doc_freq, so no need to finish scanning here
	//       need to specify behaviour of FtQuery classes to make sure it's ok
	for (int i = 0; i < nops; i++)
	{
		ops[i]->close();
	}
}
ft_float FtQueryOr::ft_get_score(ft_float *scores)
{
	int p = 0;
	ft_float res = 0;
	bool skip;
	for (int i = 0; i < nops; i++)
	{
		skip = true;
		for (int t = 0; t < ops[i]->score_count; t++)
			if (scores[p+t] != no_score)
			{
				skip = false;
				break;
			}
		if (!skip)
			res += ops[i]->ft_get_score(&scores[p]);
		p += ops[i]->score_count;
	}
	U_ASSERT(p == this->score_count);
	return res;
}


/////////////////////////////////////////////////////////////////////////////////////


FtQueryPhrase::FtQueryPhrase(int _nops) : nops(_nops)
{
	U_ASSERT(nops > 1);
	term_ops = (FtQueryTermBase **)malloc(nops * sizeof(FtQueryTermBase*));
	op_results = (uint64_t *)malloc(nops * sizeof(uint64_t));
	op_word_inds = (int *)malloc(nops * sizeof(int));
}
FtQueryPhrase::~FtQueryPhrase()
{
	for (int i = 0; i < nops; i++)
		delete term_ops[i];
	free(term_ops);
	free(op_results);
	free(op_word_inds);
}
void FtQueryPhrase::set_term(int op_idx, FtQueryTermBase *t)
{
	term_ops[op_idx] = t;
}

void FtQueryPhrase::do_init()
{
	this->score_count = 1;
	for (int i = 0; i < nops; i++)
	{
		term_ops[i]->init();
		this->score_count += term_ops[i]->score_count;
	}
}

void FtQueryPhrase::do_open()
{
	int p = 1;
	for (int i = 0; i < nops; i++)
	{
		term_ops[i]->open(&this->scores[p], NULL);
		p += term_ops[i]->score_count;
		op_results[i] = FT_ACC_UINT_NULL;
		term_ops[i]->get_next_occur(&op_results[i], &op_word_inds[i]);
	}
	U_ASSERT(p == this->score_count);
}

//scan term_ops until next phrase occur is found, if op_results/op_word_inds already match a phrase - nothing is changed
bool FtQueryPhrase::next_occur()
{
	//TODO: set initial value for second argument to get_next_occur to improve performance
	//XXX: try to remove goto-s

	U_ASSERT(nops > 1);
	int i = 1;
	if (op_results[0] == FT_ACC_UINT_NULL)
		return false;

	while (i < nops)
	{
		if (op_results[i] < op_results[i-1])
		{
			op_results[i] = op_results[i-1];
			op_word_inds[i] = 0; //FIXME
			term_ops[i]->get_next_occur(&op_results[i], &op_word_inds[i]);
			if (op_results[i] == FT_ACC_UINT_NULL)
				return false;
			U_ASSERT(op_results[i] >= op_results[i-1]);
		}
label_a:
		if (op_results[i] > op_results[i-1])
		{
			op_results[0] = op_results[i];
			op_word_inds[0] = 0; //FIXME
			term_ops[0]->get_next_occur(&op_results[0], &op_word_inds[0]);
			if (op_results[0] == FT_ACC_UINT_NULL)
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
			if (op_results[i] == FT_ACC_UINT_NULL)
				return false;
			goto label_a;
		}
		//i'th term occur is too far right from the rest
		if (op_word_inds[i] > op_word_inds[i-1]+1)
		{
			//FIXME: set op_word_inds[0]
			term_ops[0]->get_next_occur(&op_results[0], &op_word_inds[0]);
			if (op_results[0] == FT_ACC_UINT_NULL)
				return false;
			i = 1;
			continue;
		}
		U_ASSERT(op_word_inds[i] == op_word_inds[i-1]+1);
		i++;
	}
	return true;
}

ft_acc_uint_t FtQueryPhrase::get_next_result()
{
	if (!next_occur())
		return FT_ACC_UINT_NULL;

	ft_acc_uint_t res = op_results[0];
	int noccurs = 0;
	if (this->word_list != NULL)
		this->word_list->clear();

	while (op_results[0] == res)
	{
		noccurs++;
		//XXX: this relies on the fact that op_word_inds are sorted and contain no gaps
		//     (ok for phrases but will fail if next_occur() is changed to match something more fuzzy)
		if (this->word_list != NULL)
			for (int i = 0; i < nops; i++)
				this->word_list->add_ind_if_last(op_word_inds[i]); //phrases may overlap so add_ind() won't do

		term_ops[0]->get_next_occur(&op_results[0], &op_word_inds[0]);
		if (!(next_occur() && op_results[0] == res))
			break;
	}

	this->scores[0] = sqrtf(noccurs * this->score_boost) * doc_length_norm(term_ops[0]->get_doc_len(res));
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
	if (wl != NULL)
		delete wl;
	if (ss != NULL)
		delete ss;
}

void FtQueryProcessor::set_query(str_cursor *cur)
{
	query = ft_parse_query(cur, this->ftc_idx);
	query_opened = false;
}

const int ssr_n = 2;
tc_sort_type *ssr_types[ssr_n] = {&st_uint64_desc::inst, &st_uint64::inst};
const int ssr_inds[ssr_n] = {0, 1};

void FtQueryProcessor::get_next_result(xqp_tuple &t)
{
	if (query == NULL)
	{
		t.set_eos();
		return;
	}
	//TODO: make scoring optional
	if (!query_opened)
	{
		query->init();
		if (scores_buf != NULL)
			delete[] scores_buf;
		scores_buf = new ft_float[query->score_count];
		query->open(scores_buf, NULL);

		query_opened = true;

		if (this->filter_el && !this->sort)
			throw USER_EXCEPTION2(SE3022, "ftindex-scan: filter_el is not supported without sort");

		if (this->sort)
		{
			//get all results and sort them
			SblobWriter sw(false);
			xptr scores = sw.create_new();
			while (true)
			{
				ft_acc_uint_t res = query->get_next_result();
				if (res == FT_ACC_UINT_NULL)
					break;
				sw.write_uint(res);
				sw.write((char*)scores_buf, sizeof(ft_float) * query->score_count);
			}
			query->close();
			sw.flush();
			SblobReader r;
			r.init(scores);

			if (filter_el)
			{
				sequence_sorter ssr1;
				tc_sort_type *ss1_t[3] = {&st_xptr_do::inst, &st_uint64::inst, &st_uint64::inst};
				const int ss1_i[3] = {0, 1, 2};
				ssr1.create_sorted_sequence(3, ss1_t, ss1_i);

				while (!r.eos())
				{
					xqp_tuple t(3);
					uint64_t res = r.read_uint();
					r.read_bytes((char*)scores_buf, sizeof(ft_float) * query->score_count);

					ft_float score = query->ft_get_score(scores_buf);

					double score_d = (double)score;
					int64_t score_i = *(int64_t*)&score_d; //FIXME: add float/double to sequence_sorter
					t.cells[0] = tuple_cell::node(indirectionDereferenceCP(FT_UINT_TO_XPTR(res)));
					t.cells[1] = tuple_cell::atomic((int64_t)score_i);
					t.cells[2] = tuple_cell::atomic((int64_t)res);
					ssr1.add(t);
				}
				sorted_sequence *ss1 = ssr1.get_sorted_sequence();
				ss1->lazy_sort();

				ssr.create_sorted_sequence(ssr_n, ssr_types, ssr_inds);
				xqp_tuple pred_rt(3);
				pred_rt.set_eos();
				while (true)
				{
					xqp_tuple rt(3);
					ss1->next(rt);

					if (!pred_rt.is_eos())
					{
                                               //add pred_rt to results(ssr) if rt isn't pred_rt's descendant-or-self or if pred_rt's score is higher
                                               if (rt.is_eos()
                                                       || !(pred_rt.cells[0].get_node() == rt.cells[0].get_node() || nid_cmp_effective(pred_rt.cells[0].get_node(),rt.cells[0].get_node())==-2)
                                                       || pred_rt.cells[1].get_xs_integer() > rt.cells[1].get_xs_integer())
						{
							xqp_tuple t(2);
							t.cells[0] = pred_rt.cells[1];
							t.cells[1] = pred_rt.cells[2];
							ssr.add(t);
						}
					}
					if (rt.is_eos())
						break;
					pred_rt = rt;
				}
				delete ss1;
				ss = ssr.get_sorted_sequence();
				ss->lazy_sort();
			}
			else
			{
				ssr.create_sorted_sequence(ssr_n, ssr_types, ssr_inds);
				while (!r.eos())
				{
					xqp_tuple t(2);
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
		}
	}

	if (this->sort)
	{
		if (ss != NULL)
		{
			xqp_tuple res(ssr_n);
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
	}
	else
	{
		ft_acc_uint_t res = query->get_next_result();
		if (res == FT_ACC_UINT_NULL)
			t.set_eos();
		else
			t.copy(tuple_cell::node(indirectionDereferenceCP(FT_UINT_TO_XPTR(res))));
	}
}

int64_t FtQueryProcessor::count_results()
{
	int64_t count = 0;

	if (query == NULL)
		return 0;

	query->init();
	if (scores_buf != NULL)
		delete[] scores_buf;
	scores_buf = new ft_float[query->score_count];
	query->open(scores_buf, NULL);

	query_opened = true;

	while (true)
	{
		ft_acc_uint_t res = query->get_next_result();
		if (res == FT_ACC_UINT_NULL)
			return count;
		count++;
	}
}


void FtQueryProcessor::open()
{
	if (query == NULL)
		return;
	query->init();
	if (scores_buf != NULL)
		delete[] scores_buf;
	scores_buf = new ft_float[query->score_count];
	if (wl == NULL)
		wl = new FtWordIndexList();
	query->open(scores_buf, wl);

	query_opened = true;
}
void FtQueryProcessor::close()
{
	if (query == NULL)
		return;
	query->close();

	query_opened = false;
}
void FtQueryProcessor::get_next_result_hl(xptr *r_acc, FtWordIndexList **r_wl)
{
	if (query == NULL)
	{
		*r_acc = XNULL;
		return;
	}
	ft_acc_uint_t res = query->get_next_result();
	if (res == FT_ACC_UINT_NULL)
	{
		*r_acc = XNULL;
		return;
	}
	*r_acc = FT_UINT_TO_XPTR(res);
	*r_wl = this->wl;
}
