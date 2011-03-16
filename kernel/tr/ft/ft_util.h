/*
 * File:  ft_util.h
 * Copyright (C) 2011 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_UTIL_H
#define _FT_UTIL_H

#include "tr/executor/base/tuple.h"
#include "tr/ft/ft_types.h"

#ifdef SE_ENABLE_DTSEARCH
#include "tr/ft/FTsearch.h"
#endif

struct PPOpIn; //don't want PPBase.h here
class FtQueryProcessor; //don't want ft_query.h either
class FtcTempIndex; //or ft_cache.h

class FtHighlighter
{
private:
	PPOpIn* seq;

	bool hl_fragment;

	ft_index_impl impl;
	char *stemming;
	ft_stem_type ftst;
	char *use_index;

	FtQueryProcessor *ftqp;
	FtcTempIndex *ftc_ind;
#ifdef SE_ENABLE_DTSEARCH
	SednaSearchJob *sj;
#endif
public:
	FtHighlighter(bool hl_fragment, PPOpIn* _seq_);
	~FtHighlighter();
	void set_options(const char *options);
	void set_request(tuple_cell &tc);
	void get_next_result(tuple &t);
};

#endif
