/*
 * File:  PPSXptr.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPSXptr.h"
#include "tr/executor/base/visitor/PPVisitor.h"


using namespace std;


PPSXptr::PPSXptr(dynamic_context *_cxt_,
                 operation_info _info_,
                 PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPSXptr"),
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

void PPSXptr::do_open ()
{
    s = se_new sorted_sequence(compare_less, get_size, serialize, serialize_2_blks, deserialize, deserialize_2_blks, NULL);
    child.op->open();
    pos = 0;
    atomic_mode = false;
    ret_val = XNULL;
}

void PPSXptr::do_reopen()
{
    child.op->reopen();
    pos = 0;
    atomic_mode = false;
    s->clear();
    ret_val = XNULL;
}

void PPSXptr::do_close()
{
    child.op->close();
    pos = 0;
    ret_val = XNULL;
    delete s;
    s = NULL;
}

void PPSXptr::do_next (xqp_tuple &t)
{
    if(atomic_mode) {
        child.op->next(t);
        if(!t.is_eos()) {
            tuple_cell tc = child.get(t); 
            if (tc.is_node())
                throw XQUERY_EXCEPTION2(SE1003, "Atomic or node sequence is expected. Sequence checker missed?");
        }
        else {
            atomic_mode = false;
        }
        return;
    }
    
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
                if (tc.is_node()) {
                    s->add(t);
                }
                else {
                    if(s->size() != 0)
                        throw XQUERY_EXCEPTION2(SE1003, "Atomic or node sequence is expected. Sequence checker missed?");
                    atomic_mode = true;
                    return;
                }
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
           break;
        }
        else
        {
           if (t.cells[0].get_xptr() != ret_val)
           {
               ret_val = t.cells[0].get_xptr();
               break;
           }
        }
    }
}

PPIterator* PPSXptr::do_copy(dynamic_context *_cxt_)
{
    PPSXptr *res = se_new PPSXptr(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPSXptr::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

static inline void restore_serialized_xptr(const xptr &serialized, xptr &result)
{
    CHECKP(serialized);
    int size1 = GET_FREE_SPACE(serialized);
    if(size1 < (int) sizeof(xptr))
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
    CHECK_TIMER_FLAG;
    xptr res1, res2;
    restore_serialized_xptr(v1, res1);
    restore_serialized_xptr(v2, res2);
    return xptr_compare(res1, res2);
}

int PPSXptr::get_size (xqp_tuple& t, const void * Udata)
{
    return sizeof(xptr);
}

/* Temporary bugfix */
inline static
xptr get_xptr(const tuple_cell & c) {
    return isTmpBlock(c.get_xptr()) ? c.get_node_inderection() : c.get_node();
}

inline static
void get_tuple(xqp_tuple & c, xptr x) {
    c.copy(isTmpBlock(x) ? tuple_cell::node_indir(x) : tuple_cell::node(x));
}

void PPSXptr::serialize (xqp_tuple& t, xptr v1, const void *Udata)
{
    CHECK_TIMER_FLAG;
    xptr node = get_xptr(t.cells[0]);

    CHECKP(v1);
    VMM_SIGNAL_MODIFICATION(v1);

#ifdef ALIGNMENT_REQUIRED
    memcpy(XADDR(v1), &node, sizeof(xptr));
#else
    *((xptr*)XADDR(v1)) = node;	
#endif /* ALIGNMENT_REQUIRED */
}

void PPSXptr::serialize_2_blks (xqp_tuple& t, xptr& v1, shft size1, xptr& v2, const void *Udata)
{
    xptr node = get_xptr(t.cells[0]);
    CHECKP(v1);
    VMM_SIGNAL_MODIFICATION(v1);
    memcpy(XADDR(v1), &node, size1);
    CHECKP(v2);
    VMM_SIGNAL_MODIFICATION(v2);
    memcpy(XADDR(v2), ((char*)&node) + size1, sizeof(xptr) - size1);
}

void PPSXptr::deserialize (xqp_tuple& t, xptr& v1, const void *Udata)
{
    CHECK_TIMER_FLAG;
    CHECKP(v1);

#ifdef ALIGNMENT_REQUIRED
    memcpy(&node, XADDR(v1), sizeof(xptr));
#else
    xptr node = *((xptr*)XADDR(v1)); 
#endif /* ALIGNMENT_REQUIRED */

    get_tuple(t, node);
}

void PPSXptr::deserialize_2_blks (xqp_tuple& t, xptr& v1, shft size1, xptr& v2, const void *Udata)
{
    xptr node;
    CHECKP(v1);
    memcpy( &node, XADDR(v1), size1);
    CHECKP(v2);
    memcpy( ((char*)&node) + size1, XADDR(v2), sizeof(xptr) - size1);

    get_tuple(t, node);
}

