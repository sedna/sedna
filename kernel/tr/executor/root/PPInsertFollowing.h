/*
 * File:  PPInsertFollowing.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPINSERTFOLLOWING_H
#define _PPINSERTFOLLOWING_H

#include "sedna.h"
#include "PPBase.h"

class PPInsertFollowing : public PPUpdate
{
    PPOpIn child1, child2;
    variable_context *cxt1, *cxt2;
public:
    void open();
    void close();
    void execute();

    PPInsertFollowing(PPOpIn _child1_, 
                      variable_context *_cxt1_,
                      PPOpIn _child2_,
                      variable_context *_cxt2_);
    ~PPInsertFollowing();
};


#endif

