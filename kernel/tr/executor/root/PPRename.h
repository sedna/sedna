/*
 * File:  PPRename.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPRENAME_H
#define _PPRENAME_H

#include "sedna.h"

#include "PPBase.h"
#include "XPath.h"

class PPRename : public PPUpdate
{
    PPOpIn child;
    dynamic_context *cxt;
    const char *ncname_prefix;
    const char *ncname_local;

public:
    void open();
    void close();
    void execute();

    PPRename(PPOpIn _child_, 
             dynamic_context *_cxt_,
             const char *_ncname_prefix_,
             const char *_ncname_local_);
    ~PPRename();
};


#endif

