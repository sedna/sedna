/*
 * File:  PPFilterEL.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>

#include "common/sedna.h"

#include "tr/executor/xqops/PPFilterEL.h"
#include "tr/executor/xqops/PPDDO.h"
#include "tr/nid/numb_scheme.h"

using namespace std;


PPFilterEL::PPFilterEL(dynamic_context *_cxt_,
             PPOpIn _child_) : PPIterator(_cxt_),
                               s(NULL),
                               child(_child_)
{

}

PPFilterEL::~PPFilterEL()
{

	delete child.op;
    child.op = NULL;
}

void PPFilterEL::open  ()
{

    child.op->open();
    pos = 0;
	s = se_new sorted_sequence(PPDDO::compare_less,PPDDO::get_size,PPDDO::serialize,PPDDO::serialize_2_blks,PPDDO::deserialize,PPDDO::deserialize_2_blks,NULL);
	
}

void PPFilterEL::reopen()
{

    child.op->reopen();
    pos = 0;
    s->clear();

}

void PPFilterEL::close ()
{

    child.op->close();
    pos = 0;
    delete s;
    s = NULL;
}

void PPFilterEL::next  (tuple &t)
{
    SET_CURRENT_PP(this);
    
    if (!pos)
    {
        // accumulate nodes and sort them
        while (true)
        {
            child.op->next(t);
            if (t.is_eos()) break;
            else
            {
                tuple_cell tc = child.get(t);
                if (!tc.is_node()) throw USER_EXCEPTION2(SE1003, "Argument of PPFilterEL is not a node");
                s->add(t);
            }
        }

       /* u_timeb t_sort1, t_sort2;
        d_printf1("Before sorting: \n");
        u_ftime(&t_sort1);
        //s->sort();*/
        s->sort();
        /*u_ftime(&t_sort2);
        d_printf3("After sorting: time = %s size= %d\n", to_string(t_sort2 - t_sort1).c_str(),s->size());*/
    }
	if (pos < s->size())
	{
		s->get(t,pos);
		if (pos+1==s->size()) 
		{
			pos++;
			RESTORE_CURRENT_PP; return;
		}
		else
		{
			while(true)
			{
				xptr p=(t.cells[0]).get_node();
				if (pos+1==s->size())
				{
					pos++;
					RESTORE_CURRENT_PP; return;
				}
				s->get(t,pos+1);
				if ((p==(t.cells[0]).get_node()||nid_cmp_effective(p,(t.cells[0]).get_node())==-2))
					pos++;
				else
				{
					(t.cells[0]).set_node(p);
					pos++;
					RESTORE_CURRENT_PP; return;
				}
			}
		}
		
	}
	else 
	{
		t.set_eos();
		pos = 0;
		s->clear();
		RESTORE_CURRENT_PP; return;
	}

	RESTORE_CURRENT_PP;
}

PPIterator* PPFilterEL::copy(dynamic_context *_cxt_)
{
    PPFilterEL *res = se_new PPFilterEL(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPFilterEL::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
return true;
}
