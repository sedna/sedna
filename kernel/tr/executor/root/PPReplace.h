/*
 * File:  PPReplace.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPREPLACE_H
#define _PPREPLACE_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPReplace : public PPUpdate
{
    PPOpIn child;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPReplace(PPOpIn _child_, 
              dynamic_context *_cxt_);
    ~PPReplace();
};


#endif

