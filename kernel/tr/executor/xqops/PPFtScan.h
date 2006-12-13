/*
 * File:  PPFtScan.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFTSCAN_H
#define _PPFTSCAN_H

#include "sedna.h"

#include "PPBase.h"
#include "FTsearch.h"

class PPFtScan : public PPIterator
{
protected:
    // given parameters
    PPOpIn query, seq;
	PPOpIn index_type;
	PPOpIn cust_rules;

    // obtained parameters and local data
	bool first_time;
	SednaSearchJob *sj;
	pers_sset<ft_custom_cell,unsigned short>* ptr;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPFtScan(dynamic_context *_cxt_,
                PPOpIn _seq_,
                PPOpIn _query_,
				PPOpIn _index_type_);

    PPFtScan(dynamic_context *_cxt_,
                PPOpIn _seq_,
                PPOpIn _query_,
				PPOpIn _index_type_,
				PPOpIn _cust_rules_);

    virtual ~PPFtScan();

	static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
};


#endif