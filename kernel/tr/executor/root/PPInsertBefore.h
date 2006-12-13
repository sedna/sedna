/*
 * File:  PPInsertBefore.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPINSERTBEFORE_H
#define _PPINSERTBEFORE_H

#include "sedna.h"
#include "PPBase.h"

class PPInsertBefore : public PPUpdate
{
    PPOpIn child1, child2;
    dynamic_context *cxt1, *cxt2;
public:
    void open();
    void close();
    void execute();

    PPInsertBefore(PPOpIn _child1_, 
                   dynamic_context *_cxt1_,
                   PPOpIn _child2_,
                   dynamic_context *_cxt2_);
    ~PPInsertBefore();
};

#endif

