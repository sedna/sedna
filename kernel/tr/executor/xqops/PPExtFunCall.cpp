/*
 * File:  PPExtFunCall.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPExtFunCall.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPExtFunCall::PPExtFunCall(dynamic_context *_cxt_,
                           operation_info _info_,
                           const arr_of_PPOpIn &_arr_,
                           ExtFunction *_func_,
                           const std::string& _name_) : PPIterator(_cxt_, _info_, "PPExtFunCall"),
                                                        arr(_arr_),
                                                        func(_func_),
                                                        name(_name_)
{
}

PPExtFunCall::~PPExtFunCall()
{
	delete func;
}

void PPExtFunCall::do_open ()
{
	for (unsigned int i = 0; i < arr.size(); i++)
		arr[i].op->open();

	first_time = true;
}

void PPExtFunCall::do_reopen()
{
	func->result_clear();

	for (unsigned int i = 0; i < arr.size(); i++)
		arr[i].op->reopen();

	first_time = true;
}

void PPExtFunCall::do_close ()
{
	for (unsigned int i = 0; i < arr.size(); i++)
		arr[i].op->close();

	func->result_clear();
}

void PPExtFunCall::do_next(xqp_tuple &t)
{
	if (first_time)
	{
		func->invoke(arr);
		first_time = false;
	}

	func->result_next(t);
	if (t.is_eos())
		first_time = true;
}

PPIterator* PPExtFunCall::do_copy(dynamic_context *_cxt_)
{
	PPExtFunCall *res = se_new PPExtFunCall(_cxt_, info, arr, func->copy(), name);

	for (unsigned int it = 0; it < arr.size(); it++)
		res->arr[it].op = arr[it].op->copy(_cxt_);
	return res;
}

void PPExtFunCall::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    for (unsigned int it = 0; it < arr.size(); it++)
        arr[it].op->accept(v);
    v.pop();
}

