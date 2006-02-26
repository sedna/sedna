/*
 * File:  PPDeleteDeep.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDELETEDEEP_H
#define _PPDELETEDEEP_H

#include "PPBase.h"

class PPDeleteDeep : public PPUpdate
{
    PPOpIn child;
    variable_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPDeleteDeep(PPOpIn _child_, 
                 variable_context *_cxt_);
    ~PPDeleteDeep();
};


#endif

