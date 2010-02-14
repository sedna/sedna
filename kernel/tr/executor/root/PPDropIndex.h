/*
 * File:  PPDropIndex.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPDROPINDEX_H
#define _PPDROPINDEX_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPDropIndex : public PPUpdate
{
private:
    PPOpIn index_name;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:
    PPDropIndex(PPOpIn _index_name_, dynamic_context *_cxt_);

    ~PPDropIndex();
};

#endif /* _PPDROPINDEX_H */

