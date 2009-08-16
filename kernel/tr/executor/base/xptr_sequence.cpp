/*
 * File:  xptr_sequence.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <algorithm>

#include "common/sedna.h"

#include "tr/executor/base/xptr_sequence.h"
#include "tr/executor/base/seq_common.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/merge.h"


xptr_sequence::xptr_sequence() : seq_size(0), blks_num(0)
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

    seq_size = 0;		// size of the sequence in tuples

    bblk = XNULL;		// pointer to the first block of the block chain
    eblk = XNULL;		// pointer to the last block of the block chain
    blks_num = 0;		// number of blocks bound to this node (in chain)
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
    if (seq_size++ < SEQ_NUMBER_OF_TUPLES_IN_MEMORY)
    {
        mem_xptrs.push_back(p);
        return;
    }

    if (eblk == XNULL) init_blks();
    CHECKP(eblk);

    if (SEQ_BLK_FREE_SPACE(XADDR(eblk)) < sizeof(xptr))
    { // we must allocate new block
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

#define XPTRS_IN_BLOCK		((PAGE_SIZE - sizeof(seq_blk_hdr)) / sizeof(xptr));

xptr xptr_sequence::get(int pos)
{
    if (pos < SEQ_NUMBER_OF_TUPLES_IN_MEMORY) return mem_xptrs[pos];

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
    if (pos < SEQ_NUMBER_OF_TUPLES_IN_MEMORY) 
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


//#include <functional>

using namespace std;

struct nodes_document_order_less : public binary_function<xptr, xptr, bool>
{
    bool operator()(const xptr& _Left, const xptr& _Right) const
    {	
        return true;//(_Left < _Right);
    }
};

void xptr_sequence::sort() 
{ 
	sort1(0,seq_size);
	//    std::sort(begin(), end(), nodes_document_order_less());
} 
void xptr_sequence::sort1(int off, int len)
{
	// Insertion sort on smallest arrays
	if (len < 7) {
	    for (int i=off; i<len+off; i++)
		for (int j=i; j>off && ON_LESS(j,j-1)<0; j--)
		    swap( j, j-1);
	    return;
	}

	// Choose a partition element, v
	int m = off + (len >> 1);       // Small arrays, middle element
	if (len > 7) {
	    int l = off;
	    int n = off + len - 1;
	    if (len > 40) {        // Big arrays, pseudomedian of 9
		int s = len/8;
		l = med3(l,     l+s, l+2*s);
		m = med3( m-s,   m,   m+s);
		n = med3( n-2*s, n-s, n);
	    }
	    m = med3( l, m, n); // Mid-size, med of 3
	}
	xptr v = get(m);

	// Establish Invariant: v* (<v)* (>v)* v*
	int a = off, b = a, c = off + len - 1, d = c;
	while(true) {
	    while (b <= c && ON_LESS_LT( b, v)<=0) {
		if (get(b) == v)
		    swap(a++, b);
		b++;
	    }
	    while (c >= b && ON_LESS_RT(v,c)<=0) {
		if (get(c) == v)
		    swap(c, d--);
		c--;
	    }
	    if (b > c)
		break;
	    swap(b++, c--);
	}

	// Swap partition elements back to middle
	int s, n = off + len;
	s = min(a-off, b-a  );  vecswap( off, b-s, s);
	s = min(d-c,   n-d-1);  vecswap( b,   n-s, s);

	// Recursively sort non-partition-elements
	if ((s = b-a) > 1)
	    sort1( off, s);
	if ((s = d-c) > 1)
	    sort1(n-s, s);
}

void xptr_sequence::swap(int a, int b)
{
	xptr t = get(a);
	set(get(b),a);
	set(t,b);
}
 
/**
 * Returns the index of the median of the three indexed longs.
 */
int xptr_sequence::med3( int a, int b, int c) 
{
	return (ON_LESS(a,b)<0 ?
		(ON_LESS(b,c)<0 ? b : ON_LESS(a,c)<0 ? c : a) :
		(ON_LESS(c,b)<0 ? b : ON_LESS(c,a)<0 ? c : a));
}

 /**
  * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
 */
void xptr_sequence::vecswap(int a, int b, int n) 
{
	for (int i=0; i<n; i++, a++, b++) swap(a, b);
}
/*
void xptr_sequence::merge_sort(int prefixes_gen_size)
{
    xptr *array = se_new xptr[seq_size];
    int i = 0;

    for (i = 0; i < seq_size; i++) array[i] = get(i);

    qsort(array, seq_size, sizeof(xptr), doc_order_merge_cmp);

    for (i = 0; i < seq_size; i++) set(array[i], i);

    delete [] array;
}
*/

struct merge_sort_elem
{
    xptr p;
    char *s;
};

void xptr_sequence::merge_sort(int prefixes_gen_size)
{
/*
    int i = 0;
    int str_len = 0;
    xptr p;
    n_dsc* dsc;
    xptr *array = se_new xptr[merge_sort_elem];
    bool got_prefixes_gen_size = (prefixes_gen_size < 0);
    if (got_prefixes_gen_size) 
    {
    }
    else prefixes_gen_size = 0;

    for (i = 0; i < seq_size; i++)
    {
        p = array[i].p = get(i);

        CHECKP(p);
        dsc = (n_dsc*)XADDR(p);
        str_len = (dsc->nid.size == 0) ? *(shft*)(dsc->nid.prefix + sizeof(xptr)) : dsc->nid.size;

        if (got_prefixes_gen_size) 
        {
            array[i].s = s + cur_str_pos;
            memcpy(s + cur_str_pos, ___, size);
            cur_str_pos += size;
            s[cur_str_pos++] = '\0';
        }
        else
            prefixes_gen_size += str_len + 1;

        get(i)

    }

struct t_nid {
	unsigned char			prefix[11]; 
	unsigned char	size; 
};




    for (i = 0; i < seq_size; i++) array[i] = get(i);

    qsort(array, seq_size, sizeof(xptr), doc_order_merge_cmp);

    for (i = 0; i < seq_size; i++) set(array[i], i);

    delete [] array;
*/
}

