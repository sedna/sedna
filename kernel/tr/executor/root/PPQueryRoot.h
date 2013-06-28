/*
 * File:  PPQueryRoot.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPQUERYROOT_H
#define _PPQUERYROOT_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/crmutils/crmbase.h"
#include "common/lm_base.h"

class PPQueryRoot : public PPQueryEssence
{
private:
    PPOpIn child;
    xqp_tuple data;
    dynamic_context *cxt;
    bool first;

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
};

/*
 * PPSubQuery encapsulates logic behind subqueries such as trigger queries or
 * module requests
 */
class PPSubQuery : public PPQueryEssence
{
private:
    PPOpIn child;
    lock_mode lmode;
    xqp_tuple data;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);
    virtual bool do_next();

public:
    PPSubQuery(dynamic_context *_cxt_,
                PPOpIn _child_);
    virtual ~PPSubQuery();

    /* Returns true if successfuly got next item,
     * false - if result is over. */
    bool next(xqp_tuple &t);
    bool supports_next() { return true; }
    bool is_update() { return false; }
};

#endif /* _PPQUERYROOT_H */
