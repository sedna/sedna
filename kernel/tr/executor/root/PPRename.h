/*
 * File:  PPRename.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPRENAME_H
#define _PPRENAME_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/XPath.h"

enum pp_rename_type {
    PP_RENAME_NODE,
    PP_RENAME_COLLECTION,
};


class PPRename : public PPUpdate
{
    PPOpIn child;
    dynamic_context *cxt;
    const char *ncname_prefix;
    const char *ncname_local;
    pp_rename_type type;

public:
    void open();
    void close();
    void execute();

    PPRename(PPOpIn _child_, 
             dynamic_context *_cxt_,
             const char *_ncname_prefix_,
             const char *_ncname_local_,
             pp_rename_type _type_);
    ~PPRename();
};


#endif

