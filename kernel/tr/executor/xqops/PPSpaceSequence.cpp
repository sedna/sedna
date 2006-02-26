/*
 * File:  PPSpaceSequence.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <vector>
#include "PPSLStub.h"
#include "PPSpaceSequence.h"

using namespace std;
tuple_cell PPSpaceSequence::space_tup = tuple_cell::atomic_deep(xs_string, " ");
PPSpaceSequence::PPSpaceSequence(variable_context *_cxt_,
                       const arr_of_PPOpIn &_ch_arr_) : PPSequence(_cxt_,_ch_arr_)
{
	space=false;
	int_tup=tuple(1);
	int_tup.set_eos();
}
PPSpaceSequence::~PPSpaceSequence()
{
    for (it = 0; it < ch_arr.size(); it++) 
    {
        delete (ch_arr[it].op);
        ch_arr[it].op = NULL;
    }
}
PPIterator* PPSpaceSequence::copy(variable_context *_cxt_)
{
    PPSpaceSequence *res = new PPSpaceSequence(_cxt_, ch_arr);

    for (it = 0; it < ch_arr.size(); it++)
        res->ch_arr[it].op = ch_arr[it].op->copy(_cxt_);

    return res;
}
void PPSpaceSequence::next(tuple &t)
{
	if (!int_tup.is_eos())
	{
		t.copy(int_tup);
		int_tup.set_eos();
		space=true;
		return;
	}
	while (it < ch_arr.size())
    {
        ch_arr[it].op->next(int_tup);

        if (int_tup.is_eos()) it++;
        else 
		{
			if (space)
			{
				t.copy(space_tup);
				space=false;
			}
			else
			{
				t.copy(int_tup);
				int_tup.set_eos();
				space=true;
			}
			return;
		}
    }

    t.set_eos();
    it = 0;
	space=false;
}
