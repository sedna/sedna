/*
 * File:  PPExtFunCall.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPEXTFUNCALL_H
#define _PPEXTFUNCALL_H

#include <string>

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/por2qep/ext.h"

class PPExtFunCall: public PPIterator
{
private:
	arr_of_PPOpIn	arr;
	bool			first_time;
	ExtFunction		*func;
    std::string     name;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPExtFunCall(dynamic_context *_cxt_, 
                 operation_info _info_,
                 const arr_of_PPOpIn &_arr_, 
                 ExtFunction *_func_,
                 const std::string& _name_);

    virtual ~PPExtFunCall();
    inline const std::string& get_name() const { return name; }
};


#endif /*_PPEXTFUNCALL_H */
