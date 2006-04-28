/*
 * File:  PPDropFtIndex.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPDROPFTINDEX_H
#define _PPDROPFTINDEX_H

#include "sedna.h"

#include "PPBase.h"
#include "XPathOnSchema.h"
#include "ft_index_data.h"

class PPDropFtIndex : public PPUpdate
{
    // given parameters
    PathExpr *object_path;
	ft_index_type index_type;
    PPOpIn index_name;
public:
    void open();
    void close();
    void execute();

    PPDropFtIndex(PPOpIn _index_name_);

    ~PPDropFtIndex();
};


#endif