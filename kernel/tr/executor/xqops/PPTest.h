/*
 * File:  PPTest.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPTEST_H
#define _PPTEST_H

#include "common/sedna.h"
#include "tr/executor/base/tuple.h"
#include "tr/executor/base/PPUtils.h"

#define DESC_CONSIST
#define PSTR_CONSIST
///////////////////////////////////////////////////////////////////////////////
/// PPTest
///////////////////////////////////////////////////////////////////////////////


class PPTest : public PPIterator
{
protected:
    typedef void (*t_test_fun)(xptr node);
    PPOpIn seq;

    t_test_fun test_fun;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    static void checkTreeConsistency(xptr node);

    PPTest(dynamic_context *_cxt_,
           operation_info _info_,
           PPOpIn _seq_);
    virtual ~PPTest();
};

#endif

