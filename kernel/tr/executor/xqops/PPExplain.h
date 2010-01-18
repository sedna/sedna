/*
 * File:  PPExplain.h
 * Copyright (C);2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

 
#ifndef __PPEXPLAIN_H
#define __PPEXPLAIN_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

/* 
 * Operation caches result. It returns the same document node
 */
class PPExplain : public PPIterator
{
private:
    PPQueryEssence* qep_tree;
    bool first_time;
    doc_schema_node_cptr scm;
    
private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPExplain(dynamic_context *_cxt_,
              operation_info _info_,
              PPQueryEssence* _qep_tree_);
    virtual ~PPExplain();
};

#endif /* __PPEXPLAIN_H */
