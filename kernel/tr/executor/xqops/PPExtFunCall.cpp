/*
 * File:  PPExtFunCall.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "PPExtFunCall.h"


void PPExtFunCall::open   ()
{
	for (unsigned int i = 0; i < arr.size(); i++)
		arr[i].op->open();

	first_time = true;
}

void PPExtFunCall::reopen ()
{
	func->result_clear();

	for (unsigned int i = 0; i < arr.size(); i++)
		arr[i].op->reopen();

	first_time = true;
}

void PPExtFunCall::close  ()
{
	for (unsigned int i = 0; i < arr.size(); i++)
		arr[i].op->close();

	func->result_clear();
}

void PPExtFunCall::next(tuple &t)
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

PPIterator* PPExtFunCall::copy(dynamic_context *_cxt_)
{
	PPExtFunCall *res = new PPExtFunCall(_cxt_, arr, func->copy());

	for (int it = 0; it < arr.size(); it++)
		res->arr[it].op = arr[it].op->copy(_cxt_);

	return res;
}

PPExtFunCall::PPExtFunCall(dynamic_context *_cxt_, const arr_of_PPOpIn &_arr_, ExtFunction *_func_)
		: PPIterator(_cxt_), arr(_arr_), func(_func_)
{
}

PPExtFunCall::~PPExtFunCall()
{
	delete func;
}


