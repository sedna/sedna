/*
 * File:  PPConst.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPCONST_H
#define __PPCONST_H

#include "sedna.h"
#include "PPBase.h"

class PPConst : public PPIterator
{
private:
    bool first_time;
    tuple_cell c;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; }
	virtual bool is_const() { return true; }
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPConst(variable_context *_cxt_,
            const tuple_cell& _c_);
    virtual ~PPConst();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};

#endif
