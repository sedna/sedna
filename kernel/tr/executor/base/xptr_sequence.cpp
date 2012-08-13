/*
* File:  xptr_sequence.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"

#include "tr/executor/base/xptr_sequence.h"
#include "tr/executor/base/seq_common.h"
#include "tr/nid/numb_scheme.h"


#define SEQ_NUMBER_OF_XPTRS_IN_MEMORY		100

#define XPTRS_IN_BLOCK                      ((PAGE_SIZE - sizeof(seq_blk_hdr)) / sizeof(xptr));

//FIXME: int is not the best return value here
struct nid_comparator
{
	nid_comparator() {}
	int on_less(xptr_sequence *s, int a, int b) { return nid_cmp_effective(s->get(a),s->get(b)); }
	int on_less_lt(xptr_sequence *s, int a, xptr b) {return nid_cmp_effective(s->get(a),b); }
	int on_less_rt(xptr_sequence *s, xptr a, int b) { return nid_cmp_effective(a,s->get(b)); }
	int med3(xptr_sequence *s, int a, int b, int c)
	{
		return (on_less(s,a,b)<0 ?
	        (on_less(s,b,c)<0 ? b : on_less(s,a,c)<0 ? c : a) :
			(on_less(s,c,b)<0 ? b : on_less(s,c,a)<0 ? c : a));
	}
};
struct xptr_comparator
{
	xptr_comparator() {}
	int xptr_cmp(const xptr a, const xptr b) { return (b < a) - (a < b); }
	int on_less(xptr_sequence *s, int a, int b) { return xptr_cmp(s->get(a),s->get(b)); }
	int on_less_lt(xptr_sequence *s, int a, xptr b) {return xptr_cmp(s->get(a),b); }
	int on_less_rt(xptr_sequence *s, xptr a, int b) { return xptr_cmp(a,s->get(b)); }
	int med3(xptr_sequence *s, int a, int b, int c)
	{
		return (on_less(s,a,b)<0 ?
	        (on_less(s,b,c)<0 ? b : on_less(s,a,c)<0 ? c : a) :
			(on_less(s,c,b)<0 ? b : on_less(s,c,a)<0 ? c : a));
	}
};

xptr_sequence::xptr_sequence() : seq_size(0), 
                                 bblk(XNULL), 
                                 eblk(XNULL), 
                                 blks_num(0)
{
}

xptr_sequence::~xptr_sequence()
{
    clear();
}

void xptr_sequence::clear()
{
    xptr p;

    p = bblk;
    while (p != XNULL)
    {
        CHECKP(p);
        xptr tmp = p;
        p = SEQ_BLK_HDR(p)->nblk;
        vmm_delete_block(tmp);
    }

    mem_xptrs.clear();
    blk_arr.clear();

    seq_size = 0;       // size of the sequence in xptrs
    bblk = XNULL;       // pointer to the first block of the block chain
    eblk = XNULL;       // pointer to the last block of the block chain
    blks_num = 0;       // number of blocks bound to this node (in chain)
}

void xptr_sequence::init_blks()
{
    vmm_alloc_tmp_block(&bblk);
    seq_blk_hdr::init(XADDR(bblk));

    eblk = bblk;
    blks_num = 1;
    blk_arr.push_back(eblk);
}

void xptr_sequence::add(const xptr &p)
{
    if (seq_size++ < SEQ_NUMBER_OF_XPTRS_IN_MEMORY)
    {
        mem_xptrs.push_back(p);
        return;
    }

    if (eblk == XNULL) init_blks();
    CHECKP(eblk);

    // Allocate new block?
    if (SEQ_BLK_FREE_SPACE(XADDR(eblk)) < sizeof(xptr))
    { 
        xptr new_blk;
        vmm_alloc_tmp_block(&new_blk);
        seq_blk_hdr::init(XADDR(new_blk));

        blks_num++;
        blk_arr.push_back(new_blk);

        CHECKP(eblk);
        VMM_SIGNAL_MODIFICATION(eblk);

        SEQ_BLK_HDR(eblk)->nblk = new_blk;

        eblk = new_blk;

        CHECKP(eblk);
    }

    xptr* dest_xptr = (xptr*)(SEQ_BLK_CURSOR(XADDR(eblk)));
    VMM_SIGNAL_MODIFICATION(eblk);
    *dest_xptr = p;
    SEQ_BLK_HDR(eblk)->cursor += sizeof(xptr);
}

xptr xptr_sequence::get(const iterator& it)
{
    if (this != it.s) throw USER_EXCEPTION2(SE1003, "Wrong iterator passed to xptr_sequence::get");
    return get(it.pos);
}

xptr xptr_sequence::get(int pos)
{
    if (pos < SEQ_NUMBER_OF_XPTRS_IN_MEMORY) return mem_xptrs[pos];

    int b_ind = (pos - SEQ_NUMBER_OF_XPTRS_IN_MEMORY) / XPTRS_IN_BLOCK;
    int o_ind = (pos - SEQ_NUMBER_OF_XPTRS_IN_MEMORY) % XPTRS_IN_BLOCK;
    xptr p = blk_arr[b_ind] + sizeof(seq_blk_hdr) + o_ind * sizeof(xptr);

    CHECKP(p);

    return *(xptr*)(XADDR(p));
}

void xptr_sequence::set(const xptr& p, const iterator& it)
{
    if (this != it.s) throw USER_EXCEPTION2(SE1003, "Wrong iterator passed to xptr_sequence::set");
    set(p, it.pos);
}

void xptr_sequence::set(const xptr& p, int pos)
{
    if (pos < SEQ_NUMBER_OF_XPTRS_IN_MEMORY)
    {
        mem_xptrs[pos] = p;
        return;
    }

    int b_ind = (pos - SEQ_NUMBER_OF_XPTRS_IN_MEMORY) / XPTRS_IN_BLOCK;
    int o_ind = (pos - SEQ_NUMBER_OF_XPTRS_IN_MEMORY) % XPTRS_IN_BLOCK;
    xptr pp = blk_arr[b_ind] + sizeof(seq_blk_hdr) + o_ind * sizeof(xptr);

    CHECKP(pp);
    VMM_SIGNAL_MODIFICATION(pp);
    *(xptr*)(XADDR(pp)) = p;
}

void xptr_sequence::sort()
{
    sort1(0,seq_size);
}

template<class Comparator>
void sort_template(xptr_sequence *xs, int off, int len)
{
	Comparator comp;
    // Insertion sort on smallest arrays
    if (len < 7) {
        for (int i=off; i<len+off; i++)
            for (int j=i; j>off && comp.on_less(xs, j,j-1)<0; j--)
                xs->swap(j, j-1);
        return;
    }

    // Choose a partition element, v
    int m = off + (len >> 1);  // Small arrays, middle element
    if (len > 7) {
        int l = off;
        int n = off + len - 1;
        if (len > 40) {        // Big arrays, pseudomedian of 9
            int s = len/8;
            l = comp.med3(xs, l,     l+s, l+2*s);
            m = comp.med3(xs, m-s,   m,   m+s);
            n = comp.med3(xs, n-2*s, n-s, n);
        }
        m = comp.med3(xs, l, m, n);   // Mid-size, med of 3
    }
    xptr v = xs->get(m);

    // Establish Invariant: v* (<v)* (>v)* v*
    int a = off, b = a, c = off + len - 1, d = c;
    while(true) {
        while (b <= c && comp.on_less_lt(xs, b, v)<=0) {
            if (xs->get(b) == v)
                xs->swap(a++, b);
            b++;
        }
        while (c >= b && comp.on_less_rt(xs,v,c)<=0) {
            if (xs->get(c) == v)
                xs->swap(c, d--);
            c--;
        }
        if (b > c)
            break;
        xs->swap(b++, c--);
    }

    // Swap partition elements back to middle
    int s, n = off + len;
    s = s_min(a-off, b-a  );  xs->vecswap( off, b-s, s);
    s = s_min(d-c,   n-d-1);  xs->vecswap( b,   n-s, s);

    // Recursively sort non-partition-elements
    if ((s = b-a) > 1)
        sort_template<Comparator>(xs, off, s);
    if ((s = d-c) > 1)
        sort_template<Comparator>(xs, n-s, s);
}

void xptr_sequence::sort_by_xptr()
{
    sort_template<xptr_comparator>(this, 0, seq_size);
}

void xptr_sequence::sort1(int off, int len)
{
	sort_template<nid_comparator>(this, off, len);
}

void xptr_sequence::swap(int a, int b)
{
    if(a == b) return;
    xptr t = get(a);
    set(get(b), a);
    set(t, b);
}

void xptr_sequence::vecswap(int a, int b, int n)
{
    for (int i=0; i<n; i++, a++, b++) swap(a, b);
}
