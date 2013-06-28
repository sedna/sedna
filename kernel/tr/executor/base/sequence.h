/*
 * File:  sequence.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SEQUENCE_H
#define _SEQUENCE_H

#include "common/sedna.h"
#include "tr/executor/base/compare.h"
#include "tr/strings/e_string.h"


#define SEQ_NUMBER_OF_TUPLES_IN_MEMORY		100

//////////////////////////////////////////////////////////////////////////////
/// sequence
//////////////////////////////////////////////////////////////////////////////
class sequence
{
public:

    class iterator 
    {
    private:
        friend class sequence;
        friend bool operator ==(const iterator& it1, const iterator& it2);

        int pos;
        sequence *s;

        iterator(int _pos_, sequence *_s_) : pos(_pos_), s(_s_) {}

    public:
        iterator() : pos(-1), s(NULL) {}

        xqp_tuple operator*()
        { 
            xqp_tuple res(s->tuple_size);
            s->get(res, *this);
            return res;
        }

        iterator operator ++() { pos++; return *this; }
		iterator operator --() { pos--; return *this; }
        iterator operator ++(int) { iterator tmp(pos, s); pos++; return tmp; }
    };

    friend class iterator;


protected:
    typedef std::vector<tuple_cell*> t_mem_tuples;	// type for storing tuples in memory
    typedef std::vector<xptr> t_blk_arr;			// stores blocks

    t_mem_tuples mem_tuples;
    t_blk_arr    blk_arr;

    int seq_size;		// size of the sequence in tuples
    int tuple_size;		// number of tuple_cells in tuple
    int tuple_sizeof;	// number of bytes for storing tuple

    xptr bblk;			// pointer to the first block of the block chain
    xptr eblk;			// pointer to the last block of the block chain
    int blks_num;		// number of blocks bound to this node (in chain)

    estr txt;           // stores txt data

    int tuples_in_memory;
    int max_block_amount;
    bool copy_vmm_strings;

    void init_blks();

public:

    sequence(int _tuple_size_,
             int _tuples_in_memory_ = SEQ_NUMBER_OF_TUPLES_IN_MEMORY,
             int _max_block_amount_ = -1,
             bool _copy_vmm_strings_ = false);
    sequence(const tuple_cell &tc,
             int _tuples_in_memory_ = SEQ_NUMBER_OF_TUPLES_IN_MEMORY,
             int _max_block_amount_ = -1,
             bool _copy_vmm_strings_ = false);
    virtual ~sequence() { clear(); }

    int size() const { return seq_size; }


    iterator begin() { return iterator(0, this); }
    iterator end() { return iterator(seq_size, this); }

    // returns 0 - if success, 1 - if block limit is exhausted
    virtual int  add(const xqp_tuple &t);
    virtual void get(xqp_tuple &t, const iterator& it);
    virtual void get(xqp_tuple &t, int pos);

    int  get_tuple_size() const { return tuple_size; }

    virtual xqp_tuple operator[](int i)
    { 
        xqp_tuple res(tuple_size);
        get(res, i);
        return res;
    }

    virtual void clear();
    sequence(const sequence&) { throw USER_EXCEPTION2(SE1003, "Copy constructor for sequence is not implemented"); }
    sequence& operator=(const sequence&) { throw USER_EXCEPTION2(SE1003, "Assign operator for sequence is not implemented"); }
    virtual void copy(sequence* s);
    virtual void copy(sequence* s, iterator _begin, iterator _end);

    virtual tuple_cell get_00() const
    {
        if (seq_size == 0) throw USER_EXCEPTION2(SE1003, "Empty sequence passed to sequence::get_00");
        return ((tuple_cell*)(mem_tuples[0]))[0];
    }
};

inline bool operator ==(const sequence::iterator& it1, const sequence::iterator& it2)
{
    return it1.s == it2.s && it1.pos == it2.pos;
}

inline bool operator !=(const sequence::iterator& it1, const sequence::iterator& it2)
{
    return !(it1 == it2);
}


//////////////////////////////////////////////////////////////////////////////
/// A special kind of sequence such that first tuple_cell in each tuple
/// contains node descriptor's pointers
//////////////////////////////////////////////////////////////////////////////
class descript_sequence : public sequence
{
private:
	void sort1(int off, int len); 
	void swap( int a, int b);
	int med3( int a, int b, int c) ;
	void vecswap(int a, int b, int n);
	xptr get_xptr(int a);
	inline int on_less(int a, int b)
	{
		return nid_cmp(get_xptr(a),get_xptr(b));
	}
	inline int on_less_lt(int a, xptr b)
	{
		return nid_cmp(get_xptr(a),b);
	}
	inline int on_less_rt(xptr a, int b)
	{
		return nid_cmp(a,get_xptr(b));
	}
public:
    descript_sequence(int _tuple_size_) : sequence(_tuple_size_) {}
    descript_sequence(const tuple_cell &tc) : sequence(tc) {}
    ~descript_sequence() {}
	void sort();

   
};

#endif /* _SEQUENCE_H */

