/*
 * File:  xptr_sequence.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XPTR_SEQUENCE_H
#define _XPTR_SEQUENCE_H

#include "sedna.h"

#include "tuple.h"
#include "vmm.h"



#define SEQ_NUMBER_OF_XPTRS_IN_MEMORY		100
#define ON_LESS(a,b) nid_cmp_effective(get(a),get(b))
#define ON_LESS_LT(a,b) nid_cmp_effective(get(a),b)
#define ON_LESS_RT(a,b) nid_cmp_effective(a,get(b))

class xptr_sequence
{
public:

    class iterator 
    {
    private:
        friend class xptr_sequence;
        friend bool operator ==(const iterator& it1, const iterator& it2);
        friend bool operator !=(const iterator& it1, const iterator& it2);
        friend bool operator > (const iterator& it1, const iterator& it2);
        friend bool operator >=(const iterator& it1, const iterator& it2);
        friend bool operator < (const iterator& it1, const iterator& it2);
        friend bool operator <=(const iterator& it1, const iterator& it2);
        friend int  operator - (const iterator& it1, const iterator& it2);
        friend iterator operator - (const iterator& it,  int i);
        friend iterator operator + (const iterator& it, int i);
        //friend iterator operator + (const iterator& it1, const iterator& it2);

        int pos;
        xptr_sequence *s;

        iterator(int _pos_, xptr_sequence *_s_) : pos(_pos_), s(_s_) {}


    public:
        iterator() : pos(-1), s(NULL) {}

        xptr operator*() { return s->get(pos); }
        xptr operator[](int i) { return s->get(i); }


        iterator& operator ++() { pos++; return *this; }
        iterator  operator ++(int) { iterator tmp(pos, s); pos++; return tmp; }
        iterator& operator --() { pos--; return *this; }
        iterator  operator --(int) { iterator tmp(pos, s); pos--; return tmp; }
    };


private:
    typedef std::vector<xptr> t_mem_xptrs;		// type for storing tuples in memory
    typedef std::vector<xptr> t_blk_arr;		// stores blocks

    t_mem_xptrs mem_xptrs;
    t_blk_arr   blk_arr;

    int seq_size;		// size of the xptr_sequence in tuples

    xptr bblk;			// pointer to the first block of the block chain
    xptr eblk;			// pointer to the last block of the block chain
    int blks_num;		// number of blocks bound to this node (in chain)

    void init_blks();
	
	void sort1(int off, int len); 
	void swap( int a, int b);
	int med3( int a, int b, int c) ;
	void vecswap(int a, int b, int n);
    
    
	


public:

    xptr_sequence();
    ~xptr_sequence();
	void set(const xptr& p, int pos);
    int size() const { return seq_size; }
    void clear();

    iterator begin() { return iterator(0, this); }
    iterator end() { return iterator(seq_size, this); }

    void add(const xptr &p);
    xptr get(const iterator& it);
    xptr get(int pos);
    void set(const xptr& p, const iterator& it);
    xptr operator[](int i)
    { 
        return get(i);
    }

    void sort();
    void merge_sort(int prefixes_gen_size = -1);
};



inline bool operator ==(const xptr_sequence::iterator& it1, const xptr_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator==");
    return it1.pos == it2.pos;
}

inline bool operator !=(const xptr_sequence::iterator& it1, const xptr_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator!=");
    return it1.pos != it2.pos;
}

inline bool operator < (const xptr_sequence::iterator& it1, const xptr_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator<");
    return it1.pos < it2.pos;
}

inline bool operator <=(const xptr_sequence::iterator& it1, const xptr_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator<=");
    return it1.pos <= it2.pos;
}

inline bool operator > (const xptr_sequence::iterator& it1, const xptr_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator>");
    return it1.pos > it2.pos;
}

inline bool operator >=(const xptr_sequence::iterator& it1, const xptr_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator>=");
    return it1.pos >= it2.pos;
}

inline int operator -(const xptr_sequence::iterator& it1, const xptr_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator-");
    return it1.pos - it2.pos;
}

inline xptr_sequence::iterator operator -(const xptr_sequence::iterator& it, int i)
{
    return xptr_sequence::iterator(it.pos - i, it.s);
}

inline xptr_sequence::iterator operator +(const xptr_sequence::iterator& it, int i)
{
    return xptr_sequence::iterator(it.pos + i, it.s);
}


#endif
