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

inline const char*
trigger_parameter_type2c_string(trigger_parameter_type t)
{
    switch(t)
    {
    case TRIGGER_PARAMETER_NEW: return "$NEW";
    case TRIGGER_PARAMETER_OLD: return "$OLD";
    case TRIGGER_PARAMETER_WHERE: return "$WHERE";
    default: throw USER_EXCEPTION2(SE1003, "Impossible case in trigger parameter type to string conversion");
    }
}

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
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
public:
    PPXptr(dynamic_context *_cxt_,
           operation_info _info_,
           trigger_parameter_type _var_type_);

    virtual ~PPXptr();

    void set_xptr(const xptr& _p_) { p = _p_; }
    inline trigger_parameter_type get_type() const { return var_type; }
};

#endif /* __PPXPTR_H */
