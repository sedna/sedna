/*
 * File: PPFnDocAvailable.h
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _PPFNCOLAVAILABLE_H
#define _PPFNCOLAVAILABLE_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


class PPFnColAvailable : public PPIterator
{
protected:
    PPOpIn col_name_op;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnColAvailable(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPOpIn _col_name_op_);
    virtual ~PPFnColAvailable();
};


#endif /* _PPFNCOLAVAILABLE_H */
