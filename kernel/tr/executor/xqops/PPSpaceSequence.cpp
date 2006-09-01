/*
 * File:  PPSpaceSequence.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <vector>

#include "sedna.h"

#include "PPSLStub.h"
#include "PPSpaceSequence.h"

using namespace std;
tuple_cell PPSpaceSequence::space_tup = tuple_cell::atomic_deep(xs_string, " ");
PPSpaceSequence::PPSpaceSequence(variable_context *_cxt_,
                       const arr_of_PPOpIn &_ch_arr_, bool _isAtomized_) : PPSequence(_cxt_,_ch_arr_),isAtomized(_isAtomized_)
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
	PPSpaceSequence *res = new PPSpaceSequence(_cxt_, ch_arr,isAtomized);

    for (it = 0; it < ch_arr.size(); it++)
        res->ch_arr[it].op = ch_arr[it].op->copy(_cxt_);

    return res;
}
void PPSpaceSequence::next(tuple &t)
{
	if (!int_tup.is_eos())
	{
		t.copy(int_tup);
		space=int_tup.cells[0].is_atomic()||isAtomized;
		int_tup.set_eos();
		
		return;
	}
	while (it < ch_arr.size())
    {
        ch_arr[it].op->next(int_tup);

        if (int_tup.is_eos()) it++;
        else 
		{
			if (space && (int_tup.cells[0].is_atomic()||isAtomized))
			{
				t.copy(space_tup);
				space=false;
			}
			else
			{
				t.copy(int_tup);
				space=int_tup.cells[0].is_atomic()||isAtomized;
				int_tup.set_eos();
				
			}
			return;
		}
    }

    t.set_eos();
    it = 0;
	space=false;
}
