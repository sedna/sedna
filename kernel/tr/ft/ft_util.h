/*
 * File:  ft_util.h
 * Copyright (C) 2011 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_UTIL_H
#define _FT_UTIL_H

#include "tr/executor/base/tuple.h"

#ifdef SE_ENABLE_DTSEARCH
#include "tr/ft/FTsearch.h"
#endif

class PPOpIn; //don't want PPBase.h here

class FtHighlighter
{
private:
	PPOpIn* seq;
#ifdef SE_ENABLE_DTSEARCH
	SednaSearchJob *sj;
	bool use_index;
#endif
public:
	FtHighlighter(const char *options, bool hl_fragment, PPOpIn* _seq_);
	void set_request(tuple_cell &tc);
	void get_next_result(tuple &t);
};

#endif
