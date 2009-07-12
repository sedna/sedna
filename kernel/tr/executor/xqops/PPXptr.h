/*
 * File:  PPXptr.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPXPTR_H
#define __PPXPTR_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


enum trigger_parameter_type
{
    TRIGGER_PARAMETER_NEW,
    TRIGGER_PARAMETER_OLD,
    TRIGGER_PARAMETER_WHERE
};


class PPXptr : public PPIterator
{
private:
    xptr p;
    bool first_time;
    trigger_parameter_type var_type;

    PPXptr(dynamic_context *_cxt_, trigger_parameter_type _var_type_, const xptr &_p_);

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPXptr(dynamic_context *_cxt_, trigger_parameter_type _var_type_);
    virtual ~PPXptr();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    void set_xptr(const xptr& _p_) { p = _p_; }
    trigger_parameter_type get_type() const { return var_type; }
};

#endif
