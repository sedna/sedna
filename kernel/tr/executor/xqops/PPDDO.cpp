/*
* File:  PPDDO.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <iostream>

#include "common/sedna.h"

#include "tr/executor/xqops/PPDDO.h"
#include "tr/nid/numb_scheme.h"
#include "tr/executor/base/visitor/PPVisitor.h"

using namespace std;

char* PPDDO::temp_buffer = NULL;
int PPDDO::buf_lgth = 0;


PPDDO::PPDDO(dynamic_context *_cxt_,
             operation_info _info_,
             PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPDDO"),
             child(_child_)
{
    ret_val = XNULL;
}

PPDDO::~PPDDO()
{
    delete child.op;
    child.op = NULL;
}

void PPDDO::do_open ()
{
    s = new sorted_sequence(compare_less, get_size, serialize,
                               serialize_2_blks, deserialize,
                               deserialize_2_blks, NULL);
    child.op->open();
    pos = 0;
    ret_val=XNULL;
    atomic_mode = false;
}

void PPDDO::do_reopen()
{
    child.op->reopen();
    pos = 0;
    s->clear();
    ret_val=XNULL;
    atomic_mode = false;
}

void PPDDO::do_close()
{
    child.op->close();
    pos = 0;
    delete s;
    ret_val=XNULL;
}

void PPDDO::do_next (tuple &t)
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
        pos=1;
        ret_val=XNULL;
    }

    while (true)
    {
        s->next(t);
        if (t.is_eos())
        {
            pos = 0;
            s->clear();
            return;
        }
        else
        {
            if (t.cells[0].get_node()!=ret_val)
            {
                ret_val=t.cells[0].get_node();
                return;
            }
        }
    }
}

PPIterator* PPDDO::do_copy(dynamic_context *_cxt_)
{
    PPDDO *res = new PPDDO(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPDDO::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}



/*
 * Returns size of the nid which was serialized
 */
int PPDDO::get_size_ser(xptr& v1)
{
    CHECKP(v1);
    xptr ptr=(GET_FREE_SPACE(v1)<=sizeof(xptr))?
        ((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+(sizeof(seq_blk_hdr)+sizeof(xptr)-GET_FREE_SPACE(v1)):v1+sizeof(xptr);
    CHECKP(ptr);
    if (GET_FREE_SPACE(ptr)<sizeof(shft))
    {
        copy_to_buffer(XADDR(ptr),GET_FREE_SPACE(ptr));
        xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(ptr)))->nblk+sizeof(seq_blk_hdr);
        CHECKP(nblk);
        copy_to_buffer(XADDR(nblk),GET_FREE_SPACE(ptr),sizeof(shft)-GET_FREE_SPACE(ptr));
        return *((shft*)temp_buffer);
    }
    else
    {
        return *((shft*)XADDR(ptr));
    }
}
/*
 * Returns ptr to the nid serialized:
 * Either ptr to PSTRDEREF (if sz + sizeof(xptr) + sizeof(shft) > DATA_BLK_SIZE
 * or sorted sequence data block.
 */
xptr PPDDO::get_ptr_ser(xptr& v1, int sz)
{
    CHECKP(v1);
    xptr ptr = (GET_FREE_SPACE(v1) <= sizeof(xptr)+sizeof(shft)) ?
        ((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+(sizeof(seq_blk_hdr)+sizeof(xptr)+sizeof(shft)-GET_FREE_SPACE(v1)) :
        v1+(sizeof(xptr)+sizeof(shft));
    bool nc = (sz + sizeof(xptr) + sizeof(shft))>DATA_BLK_SIZE;
    if (nc)
    {
        CHECKP(ptr);
        if (GET_FREE_SPACE(ptr) < sizeof(xptr))
        {
            copy_to_buffer(XADDR(ptr),GET_FREE_SPACE(ptr));
            xptr nblk = ((seq_blk_hdr*)XADDR(BLOCKXPTR(ptr)))->nblk+sizeof(seq_blk_hdr);
            CHECKP(nblk);
            copy_to_buffer(XADDR(nblk),GET_FREE_SPACE(ptr),sizeof(xptr)-GET_FREE_SPACE(ptr));
            return *((xptr*)temp_buffer);

        }
        else
        {
            return *((xptr*)XADDR(ptr));
        }
    }
    else
        return ptr;
}

/* Copy serialized nid to the buffer */
void PPDDO::copy_data_ser_to_buffer(xptr v1, shft shift, int sz)
{
    CHECKP(v1);
    if (sz > GET_FREE_SPACE(v1))
    {
        U_ASSERT( sz + sizeof(xptr) + sizeof(shft) <= DATA_BLK_SIZE );

        copy_to_buffer(v1, shift, GET_FREE_SPACE(v1));
        xptr nblk = ((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
        CHECKP(nblk);
        copy_to_buffer(XADDR(nblk), shift + GET_FREE_SPACE(v1), sz - GET_FREE_SPACE(v1));
    }
    else
    {
        copy_to_buffer(v1, shift, sz);
    }
}


int PPDDO::compare_less (xptr v1s, xptr v2s, const void* Udata)
{
    CHECK_TIMER_FLAG;

    int s1s = get_size_ser(v1s);
    int s2s = get_size_ser(v2s);
    int s1, s2, modifier;
    xptr v1, v2;

    if (s1s <= s2s) {
        modifier = 1;
        s1 = s1s; s2 = s2s; v1 = v1s; v2 = v2s;
    }
    else {
        modifier = -1;
        s1 = s2s; s2 = s1s; v1 = v2s; v2 = v1s;
    }

    /* We always have s1 <= s2. Copy smaller value to the buffer ... */
    copy_data_ser_to_buffer(get_ptr_ser(v1, s1), s1);
    xptr data = get_ptr_ser(v2, s2);
    int s2_p1 = MIN(GET_FREE_SPACE(data), s2);
    CHECKP(data);
    int res = sign(memcmp(temp_buffer, XADDR(data), MIN(s1,s2_p1)));

    /* Common prefix is different or nids are equal */
    if (res || (s1 == s2 && s2_p1 == s2)) return modifier * res;

    /* s1 <= s2_p1 <= s2 and s1 == s2_p1.substring(0, s1) */
    if (s1 <= s2_p1) return -1 * modifier;

    /* s2_p1 < s1 && s1 <= s2 */
    data = ((seq_blk_hdr*)XADDR(BLOCKXPTR(v2)))->nblk + sizeof(seq_blk_hdr);
    CHECKP(data);
    res = sign(memcmp(temp_buffer+s2_p1, XADDR(data), s1-s2_p1));
    if (res || s1 == s2) return modifier * res;
    else return -1 * modifier;
}

int PPDDO::get_size (tuple& t, const void * Udata)
{
    xptr node = t.cells[0].get_node();
    CHECKP(node);
    shft sz = nid_get_size((t_nid *) XADDR(nodeGetNIDPtr(node)));

    U_ASSERT(sz <= PSTRMAXSIZE);

    sz += (sizeof(xptr) + sizeof(shft));
    return (sz > DATA_BLK_SIZE) ? 2*sizeof(xptr)+sizeof(shft) : sz;
}

void PPDDO::serialize (tuple& t,xptr v1, const void * Udata)
{
    CHECK_TIMER_FLAG;

    xptr node=t.cells[0].get_node();
    xptr node_to_save = isTmpBlock(node) ? t.cells[0].get_node_inderection() : t.cells[0].get_node();
    CHECKP(node);

    shft sz;
    xptr addr;

    nid_parse(nodeGetNIDPtr(node), &addr, &sz);

    CHECKP(v1);
    VMM_SIGNAL_MODIFICATION(v1);
    *((xptr*)XADDR(v1)) = node_to_save;
    *((shft*)((char*)XADDR(v1) + sizeof(xptr))) = sz;

    /*
     * Seems that this case is impossible now since
     * PSTRMAXSIZE + sizeof(xptr) + sizeof(shft) < DATA_BLK_SIZE and
     * MAX_NIDE_SIZE (i.e. sz value) <= PSTRMAXSIZE
     */
    if ((sz + (sizeof(xptr) + sizeof(shft))) > DATA_BLK_SIZE)
    {
        *((xptr*)((char*)XADDR(v1) + sizeof(xptr) + sizeof(shft))) = addr;
    }
    else
    {
        copy_to_buffer(addr, sz);
        copy_from_buffer(v1 + sizeof(xptr) + sizeof(shft), 0, sz);
    }
}

void PPDDO::serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
    xptr node=t.cells[0].get_node();
    xptr node_to_save = isTmpBlock(node) ? t.cells[0].get_node_inderection() : t.cells[0].get_node();
    CHECKP(node);

    shft sz;
    xptr addr;

    nid_parse(nodeGetNIDPtr(node), &addr, &sz);

    copy_to_buffer(&node_to_save,sizeof(xptr));
    copy_to_buffer(&sz,sizeof(xptr),sizeof(shft));

    /*
     * Seems that this case is impossible now since
     * PSTRMAXSIZE + sizeof(xptr) + sizeof(shft) < DATA_BLK_SIZE and
     * MAX_NIDE_SIZE (i.e. sz value) <= PSTRMAXSIZE
     */
    if ((sz + (sizeof(xptr) + sizeof(shft))) > DATA_BLK_SIZE)
    {
        copy_to_buffer(&addr,sizeof(xptr)+sizeof(shft),sizeof(xptr));
        copy_from_buffer(v1,0,size1);
        copy_from_buffer(v2,size1,2*sizeof(xptr)+sizeof(shft)-size1);
    }
    else
    {
        copy_to_buffer(addr,sizeof(xptr)+sizeof(shft),sz);
        copy_from_buffer(v1, 0,size1);
        copy_from_buffer(v2,size1,sz+sizeof(xptr)+sizeof(shft)-size1);
    }
}
void PPDDO::deserialize (tuple& t,xptr& v1, const void * Udata)
{
    CHECK_TIMER_FLAG;

    if (GET_FREE_SPACE(v1)<sizeof(xptr))
    {
        copy_to_buffer(v1,GET_FREE_SPACE(v1));
        xptr v2=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk;
        copy_to_buffer(v2+sizeof(seq_blk_hdr),GET_FREE_SPACE(v1),sizeof(xptr)-GET_FREE_SPACE(v1));

        xptr x = *((xptr*)temp_buffer);
        t.copy(isTmpBlock(x) ? tuple_cell::node_indir(x) : tuple_cell::node(x));
    }
    else
    {
        CHECKP(v1);

        xptr x = *((xptr*)XADDR(v1));
        t.copy(isTmpBlock(x) ? tuple_cell::node_indir(x) : tuple_cell::node(x));
    }
}

void PPDDO::deserialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
    deserialize (t,v1, Udata);
}


///////////////////////////////////////////////////////////////////////////////
/// Work with temporary buffer
///////////////////////////////////////////////////////////////////////////////

void PPDDO::copy_to_buffer(const void* addr, shft size)
{
    if (size > buf_lgth)
    {
        if (buf_lgth)
        {
            delete [] temp_buffer;
        }
        temp_buffer = new char[size];
        buf_lgth = size;
    }
    memcpy(temp_buffer, addr, size);
}

void PPDDO::copy_from_buffer(xptr addr, shft shift,shft size)
{
    CHECKP(addr);
    VMM_SIGNAL_MODIFICATION(addr);
    memcpy(XADDR(addr),temp_buffer+shift,size);
}

void PPDDO::copy_to_buffer(const void* addr, shft shift,shft size)
{
    if (size+shift>buf_lgth)
    {
        if (buf_lgth)
        {
            char* buf=new char[size+shift];
            memcpy(buf,temp_buffer,shift);
            delete [] temp_buffer;
            temp_buffer=buf;
        }
        else
            temp_buffer=new char[size+shift];
        buf_lgth=size+shift;
    }
    memcpy(temp_buffer+shift,addr,size);
}
