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
private:
    PPOpIn module_name;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPDropModule(PPOpIn _module_name_, dynamic_context *_cxt_);

    ~PPDropModule();
};

#endif /* _PPDROPMODULE_H */

