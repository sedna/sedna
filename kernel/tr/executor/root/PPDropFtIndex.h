/*
 * File:  PPDropFtIndex.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPDROPFTINDEX_H
#define _PPDROPFTINDEX_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPathOnSchema.h"

class PPDropFtIndex : public PPUpdate
{
private:
    PPOpIn index_name;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPDropFtIndex(PPOpIn _index_name_, dynamic_context *_cxt_);
    ~PPDropFtIndex();
};

#endif /* _PPDROPFTINDEX_H */
