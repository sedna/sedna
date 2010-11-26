/*
 * File:  PPFtIndexScan.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFTINDEXSCAN_H
#define _PPFTINDEXSCAN_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/ft/ft_index_data.h"
#include "tr/ft/ft_cache.h"
#include "tr/ft/query/ft_query.h"
#ifdef SE_ENABLE_DTSEARCH
#include "tr/ft/FTsearch.h"
#endif

//TODO: remove PPFtIndexScan2

class PPFtIndexScan : public PPIterator
{
protected:
    PPOpIn idx_name, query, options;

	bool first_time;
	//FIXME: use union?
#ifdef SE_ENABLE_DTSEARCH
	SednaSearchJob *sj;
#endif
	FtQueryProcessor *ftq;

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFtIndexScan(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _idx_name_,
                  PPOpIn _query_);

    PPFtIndexScan(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _idx_name_,
                  PPOpIn _query_,
                  PPOpIn _options_);

    virtual ~PPFtIndexScan();
};

class PPFtIndexScan2 : public PPIterator
{
protected:
    PPOpIn idx_name, query;
    PPOpIn max_results, field_weights;

	bool first_time;
	//FIXME: use union?
#ifdef SE_ENABLE_DTSEARCH
	SednaSearchJob2 *sj;
#endif
	ftc_scan_result *ftc_res;

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFtIndexScan2(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _idx_name_,
                   PPOpIn _query_);
    
    PPFtIndexScan2(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _idx_name_,
                   PPOpIn _query_,
				   PPOpIn _max_results_);

    PPFtIndexScan2(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _idx_name_,
                   PPOpIn _query_,
				   PPOpIn _max_results_,
				   PPOpIn _field_weights_);

    virtual ~PPFtIndexScan2();
};


#endif
