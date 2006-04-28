/*
 * File:  PPFtIndexScan.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFTINDEXSCAN_H
#define _PPFTINDEXSCAN_H

#include "sedna.h"

#include "PPBase.h"
#include "ft_index_data.h"
#include "FTsearch.h"

class PPFtIndexScan : public PPIterator
{
protected:
    // given parameters
    PPOpIn idx_name, query;

    // obtained parameters and local data
	bool first_time;
	SednaSearchJob *sj;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPFtIndexScan(variable_context *_cxt_,
                PPOpIn _idx_name_,
                PPOpIn _query_);

    virtual ~PPFtIndexScan();

	static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};


#endif