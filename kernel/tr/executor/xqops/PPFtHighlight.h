/*
 * File:  PPFtHighlight.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFTHIGHLIGHT_H
#define _PPFTHIGHLIGHT_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/ft/FTsearch.h"

class PPFtHighlight : public PPIterator
{
protected:
    PPOpIn query, seq, index;

	bool first_time;
	SednaSearchJob *sj;
	ft_custom_tree_t* ptr;
	bool hl_fragment;

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFtHighlight(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _seq_,
                  PPOpIn _query_,
                  bool _hl_fragment_);

    PPFtHighlight(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _seq_,
                  PPOpIn _query_,
                  PPOpIn _index_,
                  bool _hl_fragment_);

    virtual ~PPFtHighlight();
};


#endif
