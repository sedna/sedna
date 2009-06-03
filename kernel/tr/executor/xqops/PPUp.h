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
    // given parameters
    PPOpIn child;
    schema_node_xptr scm_node;

    // obtained parameters and local data
    xptr previous;

    void children(PPOpIn& _child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPUp(dynamic_context *_cxt_,
         PPOpIn _child_,
         schema_node_xptr _scm_node_);
    virtual ~PPUp();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
};


#endif
