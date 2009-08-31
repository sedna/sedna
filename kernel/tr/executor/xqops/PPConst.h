/*
 * File:  PPConst.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPCONST_H
#define __PPCONST_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPConst : public PPIterator
{
protected:
    bool first_time;
    tuple_cell c;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t) ; 

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPConst(dynamic_context *_cxt_,
            operation_info _info_,
            const tuple_cell& _c_);
    virtual ~PPConst();
};

#endif
