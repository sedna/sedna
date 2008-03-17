/*
 * File:  PPSXptr.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPSXptr.h"

using namespace std;


PPSXptr::PPSXptr(dynamic_context *_cxt_,
                 PPOpIn _child_) : PPIterator(_cxt_),
                                   pos(0),
                                   s(NULL),
                                   child(_child_)
{
    ret_val = XNULL;
}

PPSXptr::~PPSXptr()
{
	delete child.op;
    child.op = NULL;
}

void PPSXptr::open  ()
{
    s = se_new sorted_sequence(compare_less, get_size, serialize, serialize_2_blks, deserialize, deserialize_2_blks, NULL);
    child.op->open();
    pos = 0;
    ret_val = XNULL;
}

void PPSXptr::reopen()
{
    child.op->reopen();
    pos = 0;
    s->clear();
    ret_val = XNULL;
}

void PPSXptr::close ()
{
    child.op->close();
    pos = 0;
    ret_val = XNULL;
    delete s;
    s = NULL;
}

void PPSXptr::next  (tuple &t)
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
                if (!tc.is_node()) throw USER_EXCEPTION2(SE1003, "Argument of PPSXptr is not a node");
                s->add(t);
            }
        }

		s->lazy_sort();
		pos = 1;
        ret_val = XNULL;
    }

    while (true)
    {
        s->next(t);
        if (t.is_eos())
        {
           pos = 0;
           s->clear();
           {RESTORE_CURRENT_PP; return;}
        }
        else
        {
           if (t.cells[0].get_node() != ret_val)
           {
               ret_val = t.cells[0].get_node();
               {RESTORE_CURRENT_PP; return;}
           }
		}
    }
    
    RESTORE_CURRENT_PP;
}

PPIterator* PPSXptr::copy(dynamic_context *_cxt_)
{
    PPSXptr *res = se_new PPSXptr(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPSXptr::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPSXptr::result");
}

static inline void restore_serialized_xptr(const xptr &serialized, xptr &result)
{
    CHECKP(serialized);
    int size1 = GET_FREE_SPACE(serialized);
    if(size1 < sizeof(xptr))
    {
        memcpy( &result, XADDR(serialized), size1);
        xptr data = ((seq_blk_hdr*)XADDR(BLOCKXPTR(serialized)))->nblk + sizeof(seq_blk_hdr);
        CHECKP(data);
        memcpy( ((char*)&result) + size1, XADDR(data), sizeof(xptr) - size1);
    }
    else
    {
#ifdef ALIGNMENT_REQUIRED
        memcpy(&result, XADDR(serialized), sizeof(xptr));
#else
        result = *((xptr*)XADDR(serialized)); 
#endif /* ALIGNMENT_REQUIRED */
    }
}


int PPSXptr::compare_less (xptr v1, xptr v2, const void *Udata)
{
    xptr res1, res2;
    restore_serialized_xptr(v1, res1);
    restore_serialized_xptr(v2, res2);
    if     (res1 <  res2)  return 1;
    else if(res1 == res2)  return 0;
    else                   return -1;
}

int PPSXptr::get_size (tuple& t, const void * Udata)
{
    return sizeof(xptr);
}

void PPSXptr::serialize (tuple& t, xptr v1, const void *Udata)
{
    xptr node = t.cells[0].get_node();
    CHECKP(v1);
    VMM_SIGNAL_MODIFICATION(v1);

#ifdef ALIGNMENT_REQUIRED
    memcpy(XADDR(v1), &node, sizeof(xptr));
#else
    *((xptr*)XADDR(v1)) = node;	
#endif /* ALIGNMENT_REQUIRED */
}

void PPSXptr::serialize_2_blks (tuple& t, xptr& v1, shft size1, xptr& v2, const void *Udata)
{
    xptr node = t.cells[0].get_node();
    CHECKP(v1);
    VMM_SIGNAL_MODIFICATION(v1);
    memcpy(XADDR(v1), &node, size1);
    CHECKP(v2);
    VMM_SIGNAL_MODIFICATION(v2);
    memcpy(XADDR(v2), ((char*)&node) + size1, sizeof(xptr) - size1);
}

void PPSXptr::deserialize (tuple& t, xptr& v1, const void *Udata)
{
    CHECKP(v1);

#ifdef ALIGNMENT_REQUIRED
    memcpy(&node, XADDR(v1), sizeof(xptr));
#else
    xptr node = *((xptr*)XADDR(v1)); 
#endif /* ALIGNMENT_REQUIRED */

    t.copy(tuple_cell::node(node));
}

void PPSXptr::deserialize_2_blks (tuple& t, xptr& v1, shft size1, xptr& v2, const void *Udata)
{
    xptr node;
    CHECKP(v1);
    memcpy( &node, XADDR(v1), size1);
    CHECKP(v2);
    memcpy( ((char*)&node) + size1, XADDR(v2), sizeof(xptr) - size1);
    t.copy(tuple_cell::node(node));
}

