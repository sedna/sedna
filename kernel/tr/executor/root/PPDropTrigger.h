/*
 * File:  PPDropTrigger.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPDROPTRIGGER_H
#define _PPDROPTRIGGER_H

#include "sedna.h"

#include "PPBase.h"
#include "XPathOnSchema.h"
#include "triggers_data.h"

class PPDropTrigger : public PPUpdate
{
    // given parameters
    PathExpr *trigger_path;
//	ft_index_type index_type;
    PPOpIn trigger_name;
public:
    void open();
    void close();
    void execute();

    PPDropTrigger(PPOpIn _trigger_name_);

    ~PPDropTrigger();
};


#endif