/*
 * File:  PPDeleteDeep.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPDELETEDEEP_H
#define _PPDELETEDEEP_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPDeleteDeep : public PPUpdate
{
    PPOpIn child;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);

    PPDeleteDeep(PPOpIn _child_, 
                 dynamic_context *_cxt_);
    ~PPDeleteDeep();
};

#endif /* _PPDELETEDEEP_H */
