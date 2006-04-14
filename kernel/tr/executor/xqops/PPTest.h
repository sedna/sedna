/*
 * File:  PPTest.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPTEST_H
#define _PPTEST_H

#include "tuple.h"
#include "PPUtils.h"
#define DESC_CONSIST
#define PSTR_CONSIST
///////////////////////////////////////////////////////////////////////////////
/// PPTest
///////////////////////////////////////////////////////////////////////////////


class PPTest : public PPIterator
{
protected:
    typedef void (PPTest::*t_test_fun)(xptr node);

    // given parameters
    PPOpIn seq;
	
	
    // obtained parameters and local data
    t_test_fun test_fun;
	void checkTreeConsistency(xptr node);
	/*bool checkFT(xptr node);
	int checkFT(PPOpIn _seq_);*/
	void children(PPOpIn &_seq_) { _seq_ = seq; }
public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t) ;

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPTest(variable_context *_cxt_,
		PPOpIn _seq_);
    virtual ~PPTest();
};

#endif

