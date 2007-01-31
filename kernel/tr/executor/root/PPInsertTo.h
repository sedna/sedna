/*
 * File:  PPInsertTo.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPINSERTTO_H
#define _PPINSERTTO_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPInsertTo : public PPUpdate
{
    PPOpIn child1, child2;
    dynamic_context *cxt1, *cxt2;
public:
    void open();
    void close();
    void execute();

    PPInsertTo(PPOpIn _child1_, 
               dynamic_context *_cxt1_,
               PPOpIn _child2_,
               dynamic_context *_cxt2_);
    ~PPInsertTo();
};

#endif

