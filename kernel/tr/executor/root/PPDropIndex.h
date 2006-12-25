/*
 * File:  PPDropIndex.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDROPINDEX_H
#define _PPDROPINDEX_H

#include "sedna.h"
#include "PPBase.h"

class PPDropIndex : public PPUpdate
{
    // given parameters
    PPOpIn index_name;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPDropIndex(PPOpIn _index_name_, dynamic_context *_cxt_);

    ~PPDropIndex();
};

#endif

