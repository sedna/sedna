/*
 * File:  PPRange.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPRANGE_H
#define __PPRANGE_H

#include "sedna.h"
#include "PPBase.h"

class PPRange : public PPIterator
{
protected:
    PPOpIn start_op;
	PPOpIn end_op;
    int start,end;
	int cur;
	bool is_emp;
	

    

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
	int getIntFromOp(PPOpIn & op);
    PPRange(dynamic_context *_cxt_,
               const PPOpIn &_start_,const PPOpIn &_end_);
    virtual ~PPRange();
};


#endif
