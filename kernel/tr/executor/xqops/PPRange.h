/*
 * File:  PPRange.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPRANGE_H
#define __PPRANGE_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPRange : public PPIterator
{
protected:
    PPOpIn start_op;
	PPOpIn end_op;
    int64_t start, end, cur;
	bool is_emp;
    bool first_time;

private:
	int64_t getIntFromOp(PPOpIn & op);

    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPRange(dynamic_context *_cxt_,
            operation_info _info_,
            const PPOpIn &_start_,
            const PPOpIn &_end_);
    virtual ~PPRange();
};


#endif /* __PPRANGE_H */
