/*
 * File:  sequence.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/vmm/vmm.h"
#include "tr/executor/base/sequence.h"
#include "tr/executor/base/seq_common.h"
#include "tr/strings/e_string.h"
#include "tr/structures/nodeoperations.h"

//////////////////////////////////////////////////////////////////////////////
/// sequence
//////////////////////////////////////////////////////////////////////////////
sequence::sequence(int _tuple_size_,
                   int _tuples_in_memory_,
                   int _max_block_amount_,
                   bool _copy_vmm_strings_) :
                   seq_size(0),
                   tuple_size(_tuple_size_),
                   tuple_sizeof(_tuple_size_ * sizeof(tuple_cell)),
                   bblk(XNULL),
                   eblk(XNULL),
                   blks_num(0),
                   tuples_in_memory(_tuples_in_memory_),
                   max_block_amount(_max_block_amount_),
                   copy_vmm_strings(_copy_vmm_strings_)
{
}

sequence::sequence(const tuple_cell &tc,
                   int _tuples_in_memory_,
                   int _max_block_amount_,
                   bool _copy_vmm_strings_) :
                   seq_size(1),
                   tuple_size(1),
                   tuple_sizeof(sizeof(tuple_cell)),
                   bblk(XNULL),
                   eblk(XNULL),
                   blks_num(0),
                   tuples_in_memory(_tuples_in_memory_),
                   max_block_amount(_max_block_amount_),
                   copy_vmm_strings(_copy_vmm_strings_)
{
    if (tuples_in_memory < 1)
        throw USER_EXCEPTION2(SE1003, "Wrong combination of arguments in call to sequence constructor");
    tuple_cell *mem = new tuple_cell[1];
    mem[0] = tc;
    mem_tuples.push_back(mem);
}

void sequence::init_blks()
{
    vmm_alloc_tmp_block(&bblk);
    seq_blk_hdr::init(XADDR(bblk));

    eblk = bblk;
    blks_num = 1;
    blk_arr.push_back(eblk);
}

int sequence::add(const tuple &t)
{
    if (seq_size++ < tuples_in_memory)
    {
        tuple_cell *mem = new tuple_cell[tuple_size];
        for (int i = 0; i < tuple_size; i++) mem[i] = t.cells[i];
        mem_tuples.push_back(mem);
        return 0;
    }

    if (eblk == XNULL) init_blks();
    CHECKP(eblk);

    if (max_block_amount > 0)
    {
        int blocks_to_allocate_for_tuple = 0;
        if (SEQ_BLK_FREE_SPACE(XADDR(eblk)) < tuple_sizeof)
            blocks_to_allocate_for_tuple += 1;

        int tuple_str_len = 0;
        for (int i = 0; i < tuple_size; i++)
        {
            if (t.cells[i].get_type() == tc_light_atomic_var_size ||
                (copy_vmm_strings && t.cells[i].is_atomic() && !is_fixed_size_type(t.cells[i].get_atomic_type())))
            {
                tuple_str_len += t.cells[i].get_strlen();
            }
        }

        if (tuple_str_len != 0)
            blocks_to_allocate_for_tuple += txt.blks_to_allocate(tuple_str_len);

        if (blks_num + txt.blks() + blocks_to_allocate_for_tuple > max_block_amount)
        {
            --seq_size;
            return 1;
        }
    }

    if (SEQ_BLK_FREE_SPACE(XADDR(eblk)) < tuple_sizeof)
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

    tuple_cell* dest_addr = (tuple_cell*)(SEQ_BLK_CURSOR(XADDR(eblk)));
    for (int i = 0; i < tuple_size; i++)
    {
        VMM_SIGNAL_MODIFICATION(eblk);
        memcpy(dest_addr + i, &t.cells[i], sizeof(tuple_cell));

        if (t.cells[i].get_type() == tc_light_atomic_var_size ||
            (copy_vmm_strings && t.cells[i].is_atomic() && !is_fixed_size_type(t.cells[i].get_atomic_type())))

        {
            xptr txt_ptr = txt.append(t.cells[i]);
            WRITEP(eblk);
            (dest_addr + i)->_adjust_serialized_tc(txt_ptr);
        }
    }

    VMM_SIGNAL_MODIFICATION(eblk);
    SEQ_BLK_HDR(eblk)->cursor += tuple_sizeof;

    return 0;
}

void sequence::get(tuple &t, const iterator& it)
{
    if (this != it.s) throw USER_EXCEPTION2(SE1003, "Wrong iterator passed to sequence::get");
    get(t, it.pos);
}

void sequence::get(tuple &t, int pos)
{
    t.eos = false;
    bool cleared = false;

    if (t.size() != tuple_size)

    {
        cleared = true;
        t.reinit(tuple_size);
    }

    if (pos < tuples_in_memory)
    {
        for (int i = 0; i < tuple_size; i++)
            t.cells[i] = mem_tuples[pos][i];
        return;
    }

    if(!cleared) /// We MUST release tuple cells before use them! (IS)
    {
        for (int i = 0; i < tuple_size; i++)
            t.cells[i].set_eos();
    }

    int tuples_in_block = (PAGE_SIZE - sizeof(seq_blk_hdr)) / tuple_sizeof;
    int b_ind = (pos - tuples_in_memory) / tuples_in_block;
    int o_ind = (pos - tuples_in_memory) % tuples_in_block;
    xptr p = blk_arr[b_ind] + sizeof(seq_blk_hdr) + o_ind * tuple_sizeof;

    CHECKP(p);

    memcpy(t.cells.get(), XADDR(p), tuple_sizeof);

    for (int i = 0; i < tuple_size; i++)
    {
        if (t.cells[i].get_type() == tc_light_atomic_var_size)
        {
            tuple_cell &c = t.cells[i];
            int strlen = c.get_strlen_vmm();
            char* str = new char[strlen + 1];
            str[strlen] = '\0';
            estr_copy_to_buffer(str, c.get_str_vmm(), strlen);

            c._adjust_restored_tc(str);
        }
    }
}

void sequence::clear()
{
    xptr p = bblk;

    while (p != XNULL)
    {
        CHECKP(p);
        xptr tmp = p;
        p = SEQ_BLK_HDR(p)->nblk;
        vmm_delete_block(tmp);
    }

	txt.clear(true);

    for (unsigned int i = 0; i < mem_tuples.size(); i++)
    {
        delete [] (mem_tuples[i]);
        mem_tuples[i] = NULL;
    }

    mem_tuples.clear();
    blk_arr.clear();

    seq_size = 0;       // size of the sequence in tuples

    bblk = XNULL;       // pointer to the first block of the block chain
    eblk = XNULL;       // pointer to the last block of the block chain
    blks_num = 0;       // number of blocks bound to this node (in chain)

}


void sequence::copy(sequence* s, iterator _begin, iterator _end)
{
    if (s->tuple_size != tuple_size)
        throw USER_EXCEPTION2(SE1003, "Tuple size mismatch in sequence::copy");

    clear();

    tuple t(tuple_size);
    for (iterator it = _begin; it != _end; it++)
    {
        s->get(t, it);
        add(t);
    }
}

void sequence::copy(sequence* s)
{
    copy(s, s->begin(), s->end());
}

//////////////////////////////////////////////////////////////////////////////
/// descript_sequence
//////////////////////////////////////////////////////////////////////////////
xptr descript_sequence::get_xptr(int a)
{
    if (a < tuples_in_memory)
    {
        return mem_tuples[a][0].get_node();
    }
    else
    {
        int tuples_in_block = (PAGE_SIZE - sizeof(seq_blk_hdr)) / tuple_sizeof;
        int b_ind = (a - tuples_in_memory) / tuples_in_block;
        int o_ind = (a - tuples_in_memory) % tuples_in_block;
        xptr p = blk_arr[b_ind] + sizeof(seq_blk_hdr) + o_ind * tuple_sizeof;
        CHECKP(p);
        return ((tuple_cell*)XADDR(p))->get_node();
    }
}

void descript_sequence::sort()
{
    sort1(0,seq_size);
}

void descript_sequence::sort1(int off, int len)
{
    // Insertion sort on smallest arrays
    if (len < 7) {
        for (int i=off; i<len+off; i++)
        for (int j=i; j > off && on_less(j,j-1) < 0; j--)
            swap(j, j-1);
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
        m = med3(m-s,   m,   m+s);
        n = med3(n-2*s, n-s, n);
        }
        m = med3( l, m, n); // Mid-size, med of 3
    }
    xptr v = get_xptr(m);

    // Establish Invariant: v* (<v)* (>v)* v*
    int a = off, b = a, c = off + len - 1, d = c;
    while(true)
    {
        while (b <= c && on_less_lt(b,v)<=0)
        {
            if (get_xptr(b) == v)
                swap(a++, b);
            b++;
        }
        while (c >= b && on_less_rt(v,c)<=0)
        {
            if (get_xptr(c) == v)
                swap(c, d--);
            c--;
        }
        if (b > c) break;
        swap(b++, c--);
    }

    // Swap partition elements back to middle
    int s, n = off + len;
    s = s_min(a-off, b-a  );  vecswap( off, b-s, s);
    s = s_min(d-c,   n-d-1);  vecswap( b,   n-s, s);

    // Recursively sort non-partition-elements
    if ((s = b-a) > 1)
        sort1( off, s);
    if ((s = d-c) > 1)
        sort1(n-s, s);
}

void descript_sequence::swap(int a, int b)
{
    if ( a == b ) return;
    char* p1= new char[tuple_sizeof];

    if (a < tuples_in_memory)
    {
        memcpy(p1,&mem_tuples[a][0],tuple_sizeof);
        if (b < tuples_in_memory)
        {
            memcpy(&mem_tuples[a][0],&mem_tuples[b][0] ,tuple_sizeof);
            memcpy(&mem_tuples[b][0],p1 ,tuple_sizeof);
        }
        else
        {
            int tuples_in_block = (PAGE_SIZE - sizeof(seq_blk_hdr)) / tuple_sizeof;
            int b_ind = (b - tuples_in_memory) / tuples_in_block;
            int o_ind = (b - tuples_in_memory) % tuples_in_block;
            xptr p = blk_arr[b_ind] + sizeof(seq_blk_hdr) + o_ind * tuple_sizeof;
            CHECKP(p);
            memcpy(&mem_tuples[a][0],XADDR(p), tuple_sizeof);
            VMM_SIGNAL_MODIFICATION(p);
            memcpy(XADDR(p),p1 ,tuple_sizeof);
        }
    }
    else
    {
        if (b < tuples_in_memory)
        {
            memcpy(p1,&mem_tuples[b][0],tuple_sizeof);
            int tuples_in_block = (PAGE_SIZE - sizeof(seq_blk_hdr)) / tuple_sizeof;
            int b_ind = (a - tuples_in_memory) / tuples_in_block;
            int o_ind = (a - tuples_in_memory) % tuples_in_block;
            xptr p = blk_arr[b_ind] + sizeof(seq_blk_hdr) + o_ind * tuple_sizeof;
            CHECKP(p);
            memcpy(&mem_tuples[b][0],XADDR(p), tuple_sizeof);
            VMM_SIGNAL_MODIFICATION(p);
            memcpy(XADDR(p),p1 ,tuple_sizeof);
        }
        else
        {
            int tuples_in_block = (PAGE_SIZE - sizeof(seq_blk_hdr)) / tuple_sizeof;
            int b_ind = (a - tuples_in_memory) / tuples_in_block;
            int o_ind = (a - tuples_in_memory) % tuples_in_block;
            xptr p = blk_arr[b_ind] + sizeof(seq_blk_hdr) + o_ind * tuple_sizeof;
            CHECKP(p);
            memcpy(p1,XADDR(p),tuple_sizeof);
            char* p2= new char[tuple_sizeof];
            b_ind = (b - tuples_in_memory) / tuples_in_block;
            o_ind = (b - tuples_in_memory) % tuples_in_block;
            xptr p_b = blk_arr[b_ind] + sizeof(seq_blk_hdr) + o_ind * tuple_sizeof;
            CHECKP(p_b);
            memcpy(p2,XADDR(p_b),tuple_sizeof);
            VMM_SIGNAL_MODIFICATION(p_b);
            memcpy(XADDR(p_b),p1,tuple_sizeof);
            CHECKP(p);
            VMM_SIGNAL_MODIFICATION(p);
            memcpy(XADDR(p),p2,tuple_sizeof);
            delete[]p2;

        }
    }
    delete[]p1;
}

int descript_sequence::med3( int a, int b, int c)
{
    return (on_less(a,b)<0 ?
        (on_less(b,c)<0 ? b : on_less(a,c)<0 ? c : a) :
        (on_less(c,b)<0 ? b : on_less(c,a)<0 ? c : a));
}

void descript_sequence::vecswap(int a, int b, int n)
{
    for (int i=0; i<n; i++, a++, b++) swap(a, b);
}
