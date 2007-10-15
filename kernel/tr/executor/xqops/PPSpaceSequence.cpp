/*
 * File:  PPSpaceSequence.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <vector>

#include "common/sedna.h"

#include "tr/executor/xqops/PPSLStub.h"
#include "tr/executor/xqops/PPSpaceSequence.h"

using namespace std;
tuple_cell PPSpaceSequence::space_tup = tuple_cell::atomic_deep(xs_string, " ");
PPSpaceSequence::PPSpaceSequence(dynamic_context *_cxt_,
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
PPIterator* PPSpaceSequence::copy(dynamic_context *_cxt_)
{
	PPSpaceSequence *res = se_new PPSpaceSequence(_cxt_, ch_arr,isAtomized);

    for (it = 0; it < ch_arr.size(); it++)
        res->ch_arr[it].op = ch_arr[it].op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);

    return res;
}
void PPSpaceSequence::next(tuple &t)
{
	SET_XQUERY_LINE(__xquery_line);
	
	if (!int_tup.is_eos())
	{
		t.copy(int_tup);
		space=int_tup.cells[0].is_atomic()||isAtomized;
		int_tup.set_eos();
		
		{UNDO_XQUERY_LINE; return;}
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
			{UNDO_XQUERY_LINE; return;}
		}
    }

    t.set_eos();
    it = 0;
	space=false;

	UNDO_XQUERY_LINE;
}
