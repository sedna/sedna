/*
 * File:  PPFtScan.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFTSCAN_H
#define _PPFTSCAN_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/ft/FTsearch.h"

class PPFtScan : public PPIterator
{
protected:
    PPOpIn query, seq;
	PPOpIn index_type;
	PPOpIn cust_rules;

	bool first_time;
	SednaSearchJob *sj;
	ft_custom_tree_t* ptr;

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFtScan(dynamic_context *_cxt_,
             operation_info _info_,
             PPOpIn _seq_,
             PPOpIn _query_,
             PPOpIn _index_type_);

    PPFtScan(dynamic_context *_cxt_,
             operation_info _info_,
             PPOpIn _seq_,
             PPOpIn _query_,
             PPOpIn _index_type_,
             PPOpIn _cust_rules_);

    virtual ~PPFtScan();
};


#endif
