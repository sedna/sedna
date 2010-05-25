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

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);
    virtual bool do_next();

public:
    PPQueryRoot(dynamic_context *_cxt_,
                PPOpIn _child_);
    virtual ~PPQueryRoot();

    /* Returns true if successfuly got next item,
     * false - if result is over. */
    bool next();
    bool supports_next() { return true; }
    bool is_update() { return false; }

    /* Use it before destroying PPQueryRoot when it is
     * used as a carrier for trigger/module query-action */
    void detachChild(PPOpIn *poi, dynamic_context **dc);
};

#endif /* _PPQUERYROOT_H */
