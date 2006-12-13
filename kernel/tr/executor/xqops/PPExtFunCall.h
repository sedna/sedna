/*
 * File:  PPExtFunCall.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPEXTFUNCALL_H
#define _PPEXTFUNCALL_H

#include "sedna.h"

#include "PPBase.h"
#include "sedna_ef.h"
#include "ext.h"


class PPExtFunCall : public PPIterator
{
private:
	arr_of_PPOpIn	arr;
	bool			first_time;
	ExtFunction		*func;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return NULL; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPExtFunCall(dynamic_context *_cxt_, const arr_of_PPOpIn &_arr_, ExtFunction *_func_);

    virtual ~PPExtFunCall();
};


#endif //_PPEXTFUNCALL_H
