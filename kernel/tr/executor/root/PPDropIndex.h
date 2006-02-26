/*
 * File:  PPDropIndex.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDROPINDEX_H
#define _PPDROPINDEX_H

#include <string>
#include "PPBase.h"

class PPDropIndex : public PPUpdate
{
    // given parameters
    PPOpIn index_name;

public:
    void open();
    void close();
    void execute();

    PPDropIndex(PPOpIn _index_name_);

    ~PPDropIndex();
};

#endif

