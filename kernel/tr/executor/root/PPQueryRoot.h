/*
 * File:  PPQueryRoot.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPQUERYROOT_H
#define _PPQUERYROOT_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/crmutils/crmbase.h"

class PPQueryRoot : public PPQueryEssence
{
private:
    PPOpIn child;
    tuple data;
    dynamic_context *cxt;
    bool first;
    t_print print_mode;
    se_ostream* output_stream;

public:
    PPQueryRoot(dynamic_context *_cxt_,
                PPOpIn _child_);
    virtual ~PPQueryRoot();

    void open();
    void close();
    void execute();

    /* Returns true if successfuly got next item,
     * false - if result is over.
     */
    bool next();
    bool supports_next() { return true; }
    bool is_update() { return false; }


    // use it before destroying PPQueryRoot when it is used as a carrier for trigger/module query-action
    void detachChild(PPOpIn *poi, dynamic_context **dc);
};


#endif /* _PPQUERYROOT_H */
