/*
 * File:  PPDDO.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPDDO.h"
#include <iostream>

using namespace std;


PPDDO::PPDDO(variable_context *_cxt_,
             PPOpIn _child_) : PPIterator(_cxt_),
                               child(_child_)
{
}

PPDDO::~PPDDO()
{
#ifdef TURN_ON_DDO
#else
    delete child.op;
    child.op = NULL;
#endif
}

void PPDDO::open  ()
{
#ifdef TURN_ON_DDO
    child.op->open();
    pos = 0;
    s = new sorted_sequence(compare_less,get_size,serialize,serialize_2_blks,deserialize);
#else
    child.op->open();
#endif
}

void PPDDO::reopen()
{
#ifdef TURN_ON_DDO
    child.op->reopen();
    pos = 0;
    s->clear();
#else
    child.op->reopen();
#endif
}

void PPDDO::close ()
{
#ifdef TURN_ON_DDO
    child.op->close();
    pos = 0;
    delete s;
#else
    child.op->close();
#endif
}

void PPDDO::next  (tuple &t)
{
#ifdef TURN_ON_DDO
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
                if (!tc.is_node()) throw USER_EXCEPTION2(SE1003, "Argument of PPDDO is not a node");
                s->add(t);
            }
        }

        u_timeb t_sort1, t_sort2;
        d_printf2("Before sorting: size = %d\n", s->size());
        u_ftime(&t_sort1);
        //s->sort();
        s->sort();
        u_ftime(&t_sort2);
        d_printf2("After sorting: time = %s\n", to_string(t_sort2 - t_sort1).c_str());
    }

    if (pos < s->size()) t.copy(s->get(pos++));
    else 
    {
        t.set_eos();
        pos = 0;
        s->clear();
    }
#else
    child.op->next(t);
#endif
}

PPIterator* PPDDO::copy(variable_context *_cxt_)
{
    PPDDO *res = new PPDDO(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPDDO::result(PPIterator* cur, variable_context *cxt, void*& r)
{
#ifdef TURN_ON_DDO
    return true;
#else
    PPOpIn child;
    ((PPDDO*)cur)->children(child);

    void *child_r;
    bool child_s = (child.op->res_fun())(child.op, cxt, child_r);

    if (!child_s) // if expression is not strict
    { // create PPDDO and transmit state
        child.op = (PPIterator*)child_r;
        PPDDO *res_op = new PPDDO(cxt, child);

        r = res_op;
        return false;
    }

    return strict_op_result(cur, (sequence*)child_r, cxt, r);
#endif
}
int PPDDO::compare_less (xptr& v1,xptr& v2)
{
	return 0;
}
int PPDDO::get_size (tuple& t)
{
	xptr node=t.cells[0].get_node();
	CHECKP(node);
	int sz=((n_dsc*)XADDR(node))->nid.size;
	if (!sz)sz=*(shft*)(((n_dsc*)XADDR(node))->nid.prefix+sizeof(xptr));
	sz+=(sizeof(xptr)+sizeof(shft));
	return (sz>DATA_BLK_SIZE)?2*sizeof(xptr)+sizeof(shft):sz;	
}
void PPDDO::serialize (tuple& t,xptr v1)
{
}
void PPDDO::serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2)
{
}
tuple PPDDO::deserialize (xptr& v1)
{
	return tuple(1);
}