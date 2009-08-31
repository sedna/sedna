/*
 * File:  PPExtFunCall.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPEXTFUNCALL_H
#define _PPEXTFUNCALL_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "common/sedna_ef.h"
#include "tr/executor/por2qep/ext.h"

class PPExtFunCall: public PPIterator
{
private:
	arr_of_PPOpIn	arr;
	bool			first_time;
	ExtFunction		*func;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t) ; 

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPExtFunCall(dynamic_context *_cxt_, 
                 operation_info _info_,
                 const arr_of_PPOpIn &_arr_, 
                 ExtFunction *_func_);

    virtual ~PPExtFunCall();
};


#endif /*_PPEXTFUNCALL_H */
