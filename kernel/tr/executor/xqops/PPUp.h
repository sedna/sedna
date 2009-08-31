/*
 * File:  PPUp.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPUP_H
#define _PPUP_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPUp : public PPIterator
{
protected:
    PPOpIn child;
    schema_node_xptr scm_node;

    xptr previous;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPUp(dynamic_context *_cxt_,
         operation_info _info_,
         PPOpIn _child_,
         schema_node_xptr _scm_node_);
    virtual ~PPUp();
};


#endif
