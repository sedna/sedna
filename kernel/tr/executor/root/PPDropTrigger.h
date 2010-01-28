/*
 * File:  PPDropTrigger.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPDROPTRIGGER_H
#define _PPDROPTRIGGER_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/triggers/triggers_data.h"

class PPDropTrigger : public PPUpdate
{
    PathExpr *trigger_path;
    PPOpIn trigger_name;
    dynamic_context *cxt;
    
public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);

    PPDropTrigger(PPOpIn _trigger_name_, dynamic_context *_cxt_);

    ~PPDropTrigger();
};


#endif
