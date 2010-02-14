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
private:
    PPOpIn child;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPDeleteUndeep(PPOpIn _child_, 
                   dynamic_context *_cxt_);
    ~PPDeleteUndeep();
};

#endif /* _PPDELETEUNDEEP_H */

