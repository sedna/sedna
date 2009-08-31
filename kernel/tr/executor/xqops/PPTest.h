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
    typedef void (PPTest::*t_test_fun)(xptr node);
    PPOpIn seq;
	
    t_test_fun test_fun;
	void checkTreeConsistency(xptr node);
	/*bool checkFT(xptr node);
	int checkFT(PPOpIn _seq_);*/

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPTest(dynamic_context *_cxt_,
           operation_info _info_,
           PPOpIn _seq_);
    virtual ~PPTest();
};

#endif

