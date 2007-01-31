/*
 * File:  PPDeleteUndeep.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDELETEUNDEEP_H
#define _PPDELETEUNDEEP_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPDeleteUndeep : public PPUpdate
{
    PPOpIn child;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPDeleteUndeep(PPOpIn _child_, 
                   dynamic_context *_cxt_);
    ~PPDeleteUndeep();
};


#endif

