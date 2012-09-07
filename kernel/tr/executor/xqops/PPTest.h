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

class PPInternalFunction : public PPIterator {
protected:
    PPOpIn inputString;
public:
    virtual void do_accept(PPVisitor& v);
    virtual void do_close();
    virtual void do_open();
    virtual void do_reopen();

    PPInternalFunction(dynamic_context *_cxt_, operation_info _info_, PPOpIn _seq_);
    virtual ~PPInternalFunction();
};

class PPAbsPathExec : public PPInternalFunction {
protected:
    virtual void do_next(tuple& t);
    virtual PPIterator* do_copy(dynamic_context* _cxt_);
public:
    PPAbsPathExec(dynamic_context* _cxt_, operation_info _info_, PPOpIn _seq_);
};

class PPSchemaScan : public PPInternalFunction {
    void * data;
protected:
    virtual void do_next(tuple& t);
    virtual PPIterator* do_copy(dynamic_context* _cxt_);
public:
    PPSchemaScan(dynamic_context* _cxt_, operation_info _info_, PPOpIn _seq_);
};


class PPDataGraph : public PPInternalFunction {
    void * data;
    unsigned idx;

    u_timeb t_start, t_opt, t_end;
public:
    virtual PPIterator* do_copy(dynamic_context* _cxt_);
    virtual void do_next(tuple& t);

    PPDataGraph(dynamic_context *_cxt_, operation_info _info_, PPOpIn _seq_);
};

class PPTest : public PPIterator
{
protected:
    typedef void (*t_test_fun)(xptr node);
    PPOpIn seq;
    void * data;

    t_test_fun test_fun;
private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
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

