/*
 * File:  PPFtIndexScan.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFTINDEXSCAN_H
#define _PPFTINDEXSCAN_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/ft/ft_index_data.h"
#include "tr/ft/FTsearch.h"

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

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPFtIndexScan(dynamic_context *_cxt_,
                PPOpIn _idx_name_,
                PPOpIn _query_);

    virtual ~PPFtIndexScan();

	static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
};


#endif