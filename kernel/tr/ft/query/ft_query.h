/*
 * File:  ft_query.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_QUERY_H
#define _FT_QUERY_H

#include "tr/strings/strings_base.h"
#include "tr/ft/ft_cache.h"
#include "tr/ft/sequence_sorter.h"

class FtQuery
{
private:
	virtual void do_open() = 0;
public:
	int doc_freq; //number of found acc-s
	int score_count; //initialized by open(), number of scores returned by get_next_result()
	ft_float *scores; //buffer for storing scores, it's contens are updated by get_next_result
	//start a search
	void open() { doc_freq = 0; do_open(); }
	//init is called after open
	virtual void init(ft_float *_scores) = 0;
	//called at the end of searching
	virtual void close() = 0;
	//get score for some document, based on scores returned by get_next_result, called after close() i.e. when scanning is done
	virtual ft_float ft_get_score(ft_float *scores) = 0;
	virtual uint64_t get_next_result() = 0;
	virtual ~FtQuery() {}
};
class FtQueryTermBase : public FtQuery
{
public:
	char term_buf[FT_MAX_WORD_LENGTH+1];

	virtual ftc_index_t get_ftc_idx() = 0; //XXX: dirty hack for FtQueryPhrase
	virtual uint64_t get_doc_len(ft_uint_t acc_i) = 0;

	//get next word occur, if initial *acc != FT_UINT_NULL, resulting *acc >= initial *acc
	//if resulting *acc == initial *acc, resulting *word_ind >= *initial word_ind
	//if no more acceptable results - resulting *acc is set to FT_UINT_NULL
	//if get_next_occur is used then get_next_result can't be used and vice versa
	virtual void get_next_occur(ft_uint_t *acc, int *word_ind) = 0;
};

class FtQueryTerm : public FtQueryTermBase
{
private:
	ftc_index_t ftc_idx;
	ftc_scan_result ftc_scan;
	virtual void do_open();
public:
	virtual ftc_index_t get_ftc_idx() {return this->ftc_idx; }
	virtual uint64_t get_doc_len(ft_uint_t acc_i) { return ftc_scan.get_doc_len(acc_i); }

	FtQueryTerm(ftc_index_t idx) : ftc_idx(idx), ftc_scan(idx) {}
	virtual ~FtQueryTerm();

	virtual void init(ft_float *_scores);
	virtual uint64_t get_next_result();
	virtual void close();
	virtual ft_float ft_get_score(ft_float *scores);

	void get_next_occur(ft_uint_t *acc, int *word_ind);
};

class FtQueryTermInElement: public FtQueryTermBase
{
private:
	ftc_index_t ftc_idx;
	ftc_scan_result ftc_scan, ftc_scan_opentag, ftc_scan_closetag;
	virtual void do_open();
	ft_uint_t opentag_acc_i, closetag_acc_i;
	int opentag_ind, closetag_ind;

	ft_uint_t next_acc_i; //current result (it will be returned by next get_next_result() call), FIXME: remove
	int next_ind;
public:
	char opentag_buf[FT_MAX_WORD_LENGTH+1];
	char closetag_buf[FT_MAX_WORD_LENGTH+1];
	virtual ftc_index_t get_ftc_idx() {return this->ftc_idx; }
	virtual uint64_t get_doc_len(ft_uint_t acc_i) { return ftc_scan.get_doc_len(acc_i); }

	FtQueryTermInElement(ftc_index_t idx) : ftc_idx(idx), ftc_scan(idx), ftc_scan_opentag(idx), ftc_scan_closetag(idx) {}
	virtual ~FtQueryTermInElement();

	virtual void init(ft_float *_scores);
	virtual uint64_t get_next_result();
	virtual void close();
	virtual ft_float ft_get_score(ft_float *scores);

	void get_next_occur(ft_uint_t *acc, int *word_ind);
};

class FtQueryAnd : public FtQuery
{
private:
	ftc_index_t ftc_idx;
	int nops;
	FtQuery **ops;
	uint64_t *op_results;

	virtual void do_open();
public:
	FtQueryAnd(ftc_index_t idx, int nops);
	virtual ~FtQueryAnd();

	void set_operand(int op_idx, FtQuery *op);

	virtual void init(ft_float *_scores);
	virtual uint64_t get_next_result();
	virtual void close();
	virtual ft_float ft_get_score(ft_float *scores);
};
//TODO: FtQueryPhrase should not ignore term scores
class FtQueryPhrase : public FtQuery
{
private:
	int nops;
	FtQueryTermBase **term_ops;
	ft_uint_t *op_results;
	int *op_word_inds;

	virtual void do_open();

	bool next_occur();
public:
	FtQueryPhrase(int _nops);
	virtual ~FtQueryPhrase();

	void set_term(int op_idx, FtQueryTermBase *t);

	virtual void init(ft_float *_scores);
	virtual uint64_t get_next_result();
	virtual void close();
	virtual ft_float ft_get_score(ft_float *scores);
};

class FtQueryProcessor
{
private:
	ftc_index_t ftc_idx;
	FtQuery *query;
	bool query_opened;
	ft_float *scores_buf;
	sequence_sorter ssr;
	sorted_sequence *ss;
public:
	FtQueryProcessor(ftc_index_t idx) : ftc_idx(idx), query(NULL), scores_buf(NULL), ss(NULL) {}
	~FtQueryProcessor();
	void set_query(str_cursor *cur);
	void get_next_result(tuple &t);
};


#endif
