/*
 * File: PPFnIndexKeys.h
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */


#ifndef _PPFNINDEXKEYS_H
#define _PPFNINDEXKEYS_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/idx/indeximpl.h"


class PPFnIndexKeys : public PPIterator
{
protected:
    PPOpIn index_name;

    scoped_ptr<idx::KeyValueIterator> cursor;
    idx::KeyValueMultimap * index;

    xptr res;
    bool first_time;

private:

    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

    void initialize   ();

public:
    PPFnIndexKeys(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _index_name_);

    virtual ~PPFnIndexKeys();
};


#endif /* _PPFNINDEXKEYS_H */
