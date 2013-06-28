/*
 * File: PPDigest.h
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef __PPDIGEST_H
#define __PPDIGEST_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/PPUtils.h"

class PPDigest : public PPIterator
{
private:
    const char* digest_name;
    PPOpIn name_child;
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPDigest(dynamic_context *_cxt_,
                    operation_info _info_,
                    const char* _digest_name_,
                    PPOpIn _child_);
    PPDigest(dynamic_context *_cxt_,
                    operation_info _info_,
                    PPOpIn _name_child_,
                    PPOpIn _child_);
    virtual ~PPDigest();
    const char* get_function_name();
};

#endif /* __PPDIGEST_H */
