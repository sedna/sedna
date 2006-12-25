/*
 * File:  PPDropIndex.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDROPMODULE_H
#define _PPDROPMODULE_H

#include "sedna.h"
#include "PPBase.h"

class PPDropModule : public PPUpdate
{
    // given parameters
    PPOpIn module_name;

public:
    void open();
    void close();
    void execute();

    PPDropModule(PPOpIn _module_name_);

    ~PPDropModule();
};

#endif

