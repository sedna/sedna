/*
 * File:  ft_query.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_QUERY_H
#define _FT_QUERY_H

#include "tr/strings/strings_base.h"
#include "tr/ft/ft_cache.h"

class FtQuery
{
public:
	virtual void open() = 0;
	virtual uint64_t get_next_result() = 0;
	virtual ~FtQuery() {}
};

class FtQueryTerm : public FtQuery
{
private:
	ftc_index_t ftc_idx;
	ftc_scan_result ftc_scan;
public:
	char term_buf[FT_MAX_WORD_LENGTH+1];
	FtQueryTerm(ftc_index_t idx) : ftc_idx(idx), ftc_scan(idx) {}
	virtual ~FtQueryTerm();

	virtual void open();
	virtual uint64_t get_next_result();

	//get next word occur, if initial *acc != FT_UINT_NULL, resulting *acc >= initial *acc
	//if resulting *acc == initial *acc, resulting *word_ind >= *initial word_ind
	//if no more acceptable results - resulting *acc is set to FT_UINT_NULL
	//if get_next_occur is used then get_next_result can't be used and vice versa
	void get_next_occur(ft_uint_t *acc, int *word_ind);
};
class FtQueryAnd : public FtQuery
{
private:
	ftc_index_t ftc_idx;
	int nops;
	FtQuery **ops;
	uint64_t *op_results;
public:
	FtQueryAnd(ftc_index_t idx, int nops);
	virtual ~FtQueryAnd();

	void set_operand(int op_idx, FtQuery *op);

	virtual void open();
	virtual uint64_t get_next_result();
};
class FtQueryPhrase : public FtQuery
{
private:
	int nops;
	FtQueryTerm **term_ops;
	uint64_t *op_results;
	int *op_word_inds;
public:
	FtQueryPhrase(int _nops);
	virtual ~FtQueryPhrase();

	void set_term(int op_idx, FtQueryTerm *t);

	virtual void open();
	virtual uint64_t get_next_result();
};

class FtQueryProcessor
{
private:
	ftc_index_t ftc_idx;
	FtQuery *query;
	bool query_opened;
public:
	FtQueryProcessor(ftc_index_t idx) : ftc_idx(idx), query(NULL) {}
	~FtQueryProcessor();
	void set_query(str_cursor *cur);
	void get_next_result(tuple &t);
};


#endif
