/*
 * File:  PPFtHighlight.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFTHIGHLIGHT_H
#define _PPFTHIGHLIGHT_H

#include "sedna.h"

#include "PPBase.h"
#include "FTsearch.h"

class PPFtHighlight : public PPIterator
{
protected:
    // given parameters
    PPOpIn query, seq;

    // obtained parameters and local data
	bool first_time;
	SednaSearchJob *sj;
	pers_sset<ft_custom_cell,unsigned short>* ptr;
	bool hl_fragment;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPFtHighlight(dynamic_context *_cxt_,
                PPOpIn _seq_,
                PPOpIn _query_,
				bool _hl_fragment_);

    virtual ~PPFtHighlight();

	static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
};


#endif