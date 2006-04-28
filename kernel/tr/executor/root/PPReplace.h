/*
 * File:  PPReplace.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPREPLACE_H
#define _PPREPLACE_H

#include "sedna.h"
#include "PPBase.h"

class PPReplace : public PPUpdate
{
    PPOpIn child;
    variable_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPReplace(PPOpIn _child_, 
              variable_context *_cxt_);
    ~PPReplace();
};


#endif

