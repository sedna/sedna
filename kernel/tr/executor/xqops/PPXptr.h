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
protected:
    xptr p;
    bool first_time;
    trigger_parameter_type var_type;

    PPXptr(dynamic_context *_cxt_, 
           operation_info _info_,
           trigger_parameter_type _var_type_,
           const xptr &_p_);

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPXptr(dynamic_context *_cxt_,
           operation_info _info_,
           trigger_parameter_type _var_type_);

    virtual ~PPXptr();

    void set_xptr(const xptr& _p_) { p = _p_; }
    trigger_parameter_type get_type() const { return var_type; }
};

#endif
