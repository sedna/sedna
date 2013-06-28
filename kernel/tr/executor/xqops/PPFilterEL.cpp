/*
 * File:  PPFilterEL.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>

#include "common/sedna.h"

#include "tr/executor/xqops/PPFilterEL.h"
#include "tr/executor/xqops/PPDDO.h"
#include "tr/nid/numb_scheme.h"
#include "tr/executor/base/visitor/PPVisitor.h"

using namespace std;

PPFilterEL::PPFilterEL(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFilterEL"),
                                         s(NULL),
                                         child(_child_)
{

}

PPFilterEL::~PPFilterEL()
{

	delete child.op;
    child.op = NULL;
}

void PPFilterEL::do_open ()
{

    child.op->open();
    pos = 0;
	s = se_new sorted_sequence(PPDDO::compare_less,
                               PPDDO::get_size,
                               PPDDO::serialize,
                               PPDDO::serialize_2_blks,
                               PPDDO::deserialize,
                               PPDDO::deserialize_2_blks,
                               NULL);
	
}

void PPFilterEL::do_reopen()
{

    child.op->reopen();
    pos = 0;
    s->clear();

}

void PPFilterEL::do_close()
{

    child.op->close();
    pos = 0;
    delete s;
    s = NULL;
}

void PPFilterEL::do_next (xqp_tuple &t)
{
        
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
        s->sort();
    }
	if (pos < s->size())
	{
		s->get(t,pos);
		if (pos+1==s->size()) 
		{
			pos++;
		}
		else
		{
			while(true)
			{
				xptr p=(t.cells[0]).get_node();
				if (pos+1==s->size())
				{
					pos++;
					return;
				}
				s->get(t,pos+1);
				if ((p==(t.cells[0]).get_node()||nid_cmp_effective(p,(t.cells[0]).get_node())==-2))
					pos++;
				else
				{
					(t.cells[0]).set_node(p);
					pos++;
					return;
				}
			}
		}
		
	}
	else 
	{
		t.set_eos();
		pos = 0;
		s->clear();
	}
}

PPIterator* PPFilterEL::do_copy(dynamic_context *_cxt_)
{
    PPFilterEL *res = se_new PPFilterEL(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFilterEL::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}
