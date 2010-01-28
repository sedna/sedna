/*
 * File:  PPDropIndex.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDROPMODULE_H
#define _PPDROPMODULE_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPDropModule : public PPUpdate
{
    PPOpIn module_name;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);

    PPDropModule(PPOpIn _module_name_, dynamic_context *_cxt_);

    ~PPDropModule();
};

#endif

