/*
 * File:  PPExplain.h
 * Copyright (C);2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

 
#ifndef __PPEXPLAIN_H
#define __PPEXPLAIN_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

/* 
 * Operation caches result. It returns the same document node.
 *
 * NOTE: PPExplain operates with two contexts: cxt and data_cxt. It can do
 * whatever it wants with cxt, since it's explain-query cxt. But it mustn't
 * touch data_cxt since this is the query-context and it should be used only
 * to obtain the data about the query.
 */
class PPExplain : public PPIterator
{
private:
    PPQueryEssence* qep_tree;
    bool first_time;
    doc_schema_node_cptr scm;
    bool profiler_mode;
    dynamic_context *data_cxt;
    
private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPExplain(dynamic_context *_cxt_,
              operation_info _info_,
              PPQueryEssence* _qep_tree_,
              dynamic_context *data_context,
              bool _profiler_mode_ = false);

    virtual ~PPExplain();
};

#endif /* __PPEXPLAIN_H */
