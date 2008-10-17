/*
 * File:  PPFnGetProperty.h
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPFNGETPROPERTY_H
#define __PPFNGETPROPERTY_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPFnGetProperty : public PPIterator
{
private:
    PPOpIn child;
    bool first_time;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; }
	virtual bool is_const() { return true; }
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPFnGetProperty(dynamic_context *_cxt_, PPOpIn _child_);
    virtual ~PPFnGetProperty();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
};

#endif
