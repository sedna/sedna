/*
 * File:  PPFtHighlight.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFTHIGHLIGHT_H
#define _PPFTHIGHLIGHT_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"

class FtHighlighter;
class PPFtHighlight : public PPIterator
{
protected:
    PPOpIn query, seq, options;

	bool first_time;
	bool hl_fragment;

	FtHighlighter *fthl;

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
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
                  PPOpIn _options_,
                  bool _hl_fragment_);

    virtual ~PPFtHighlight();

    inline bool is_highlight_fragment() { return hl_fragment; } 
};


#endif /* _PPFTHIGHLIGHT_H */
