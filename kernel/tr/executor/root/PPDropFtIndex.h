/*
 * File:  PPDropFtIndex.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPDROPFTINDEX_H
#define _PPDROPFTINDEX_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/ft/ft_index_data.h"

class PPDropFtIndex : public PPUpdate
{
    // given parameters
    PPOpIn index_name;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPDropFtIndex(PPOpIn _index_name_, dynamic_context *_cxt_);
    ~PPDropFtIndex();
};


#endif