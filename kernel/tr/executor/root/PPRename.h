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
    variable_context *cxt;
    QName name;

public:
    void open();
    void close();
    void execute();

    PPRename(PPOpIn _child_, 
             variable_context *_cxt_,
             QName _name_);
    ~PPRename();
};


#endif

