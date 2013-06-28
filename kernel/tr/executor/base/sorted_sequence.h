/*
 * File:  sorted_sequence.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SORTED_SEQUENCE_H
#define _SORTED_SEQUENCE_H

#include "common/sedna.h"
#include "common/commutil.h"
#include "tr/executor/base/tuple.h"
#include "tr/vmm/vmm.h"
#include "tr/executor/base/seq_common.h"
#include "tr/structures/rbtree.h"

#define MAX_BLOCKS_IN_CHAIN 500 //should be changed after meeting with Andrey
#define GET_FREE_SPACE(p) (shft)((uint32_t)PAGE_SIZE - ((XADDR_INT(p)) & PAGE_REVERSE_BIT_MASK))

typedef int (*compare_fn)(xptr v1,xptr v2, const void *);
typedef int (*get_size_fn)(xqp_tuple& t, const void *);
typedef void (*serialize_fn)(xqp_tuple& t, xptr v1, const void *);
typedef void (*serialize_2_blks_fn)(xqp_tuple& t, xptr& v1,shft size1,xptr& v2, const void *);
typedef void (*deserialize_fn)(xqp_tuple&,xptr& v1, const void *);
typedef void (*deserialize_2_blks_fn)(xqp_tuple& t,xptr& v1,shft size1,xptr& v2, const void *);

#ifndef _data_ptr_
#define _data_ptr_
struct data_ptr
{
	xptr value;
	shft size;
};
#endif

static inline void  get_val(xptr ptr,xptr& val)
{
	CHECKP(ptr);
	val=((data_ptr*)XADDR(ptr))->value;
}
struct merge_cell
{
	xptr node;
	compare_fn compareFN;
	const void * Udata;
	xptr in_node;
	static inline merge_cell* init(xptr node,compare_fn fn,const void * Udata )
	{
		merge_cell* nc=(merge_cell*) malloc(sizeof(merge_cell));
		nc->node=node;
		nc->Udata=Udata;
		nc->compareFN=fn;
		get_val(node,nc->in_node);
		return nc;
	}
	static inline void destroy(merge_cell* mc)
	{
		free(mc);
	}
	inline bool less( merge_cell *p1)
	{
		return compareFN(in_node,p1->in_node,Udata)<0;
	}
	inline bool equals( merge_cell *p1)
	{
		return compareFN(in_node,p1->in_node,Udata)==0;
	}
	inline bool less(const void* p1,const void* p2)
	{
		return true;
	}
	inline bool equals(const void* p1,const void* p2)
	{
		return true;
	}
};

#define PTR_BLK_SIZE ((PAGE_SIZE-sizeof(seq_blk_hdr))/sizeof(data_ptr))
#define DATA_BLK_SIZE (PAGE_SIZE-sizeof(seq_blk_hdr))
class sorted_sequence
{
public:

    class iterator
    {
    private:
        friend class sorted_sequence;
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
        sorted_sequence *s;

        iterator(int _pos_, sorted_sequence *_s_) ;


    public:

        xqp_tuple operator*() { return s->get(pos); }
        xqp_tuple operator[](int i) { return s->get(i); }


        iterator& operator ++() { pos++; return *this; }
        iterator  operator ++(int) { iterator tmp(pos, s); pos++; return tmp; }
        iterator& operator --() { pos--; return *this; }
        iterator  operator --(int) { iterator tmp(pos, s); pos--; return tmp; }
    };


private:
	typedef std::pair<xptr, shft> t_seq_pair;
	typedef std::vector<t_seq_pair> t_sorted_seqs_arr;		// type for storing already sorted sequences in memory
    typedef std::vector<xptr> t_xptr_blk_arr;		// stores empty blocks

    t_sorted_seqs_arr sorted_seqs_arr;
    t_xptr_blk_arr   empty_blk_arr;
	t_xptr_blk_arr   ptr_blk_arr;

    int seq_size;		// size of the xptr_sequence in tuples
	compare_fn compareFN;
	get_size_fn getSizeFN;
	serialize_fn serializeFN;
	serialize_2_blks_fn serialize2FN;
	deserialize_fn deserializeFN;
	deserialize_2_blks_fn deserialize2FN;
	bool finalized;
	xptr ptr_place;
	xptr val_place;
	xptr bblk_in_chain;
	int blk_cnt;
	sedna_rbtree<merge_cell>* merge_tree;
	sedna_rbtree<merge_cell>::sedna_rbtree_entry* top;


  //  void init_blks();

	void sort1(int off, int len);
	void swap( int a, int b);
	int med3( int a, int b, int c) ;
	void vecswap(int a, int b, int n);
    xptr get_free_block();
	void in_mem_sort();
	void clear_blocks_in_chain(const xptr& begin);
	void in_mem_order_data();
	void merge_stack(bool final);
	void unlock_memory();
	void set_next_ptr_with_free(xptr& ptr, bool free=true);
	xptr merge_sequences(xptr s1,xptr s2, bool final);
	xptr get_ptr(int pos);

    xptr get_data(int pos);
	int get_size_in_mem();
	void copy_data_to_new_place(xptr ptr,xptr& place);
	void copy_ptr_to_new_place(xptr ptr,xptr& place,bool marking);
	void set_next_block_in_chain(xptr& place, bool marking=false);
	char* temp_buffer;
	shft buf_length;
	const void * Udata;



public:

    sorted_sequence(compare_fn _compareFN_, get_size_fn _getSizeFN_, serialize_fn _serializeFN_,
	serialize_2_blks_fn _serialize2FN_,	deserialize_fn _deserializeFN_,deserialize_2_blks_fn _deserialize2FN_, const void * _Udata_);
    ~sorted_sequence();

    int size() const { return seq_size; }
    void clear();

    iterator begin()
	{
		if (finalized)
			throw USER_EXCEPTION2(SE1003, "Failed to iterate overy unsorted sequence");
	    return iterator(0, this);
	}
    iterator end()
	{
		if (finalized)
			throw USER_EXCEPTION2(SE1003, "Failed to iterate overy unsorted sequence");
		return iterator(seq_size, this);
	}

    void add(xqp_tuple &p);
    xqp_tuple get(const iterator& it);
    xqp_tuple get(int pos);
	void get(xqp_tuple& t, int pos);
    xqp_tuple operator[](int i)
    {
        return get(i);
    }
    void sort();
	void lazy_sort();
	void next(xqp_tuple& t);
	inline static void  get_val(xptr ptr,xptr& val)
	{
		CHECKP(ptr);
		val=((data_ptr*)XADDR(ptr))->value;
	}
	inline static int  get_size(xptr ptr)
	{
		CHECKP(ptr);
		return ((data_ptr*)XADDR(ptr))->size;
	}

};



inline bool operator ==(const sorted_sequence::iterator& it1, const sorted_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator==");
    return it1.pos == it2.pos;
}

inline bool operator !=(const sorted_sequence::iterator& it1, const sorted_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator!=");
    return it1.pos != it2.pos;
}

inline bool operator < (const sorted_sequence::iterator& it1, const sorted_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator<");
    return it1.pos < it2.pos;
}

inline bool operator <=(const sorted_sequence::iterator& it1, const sorted_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator<=");
    return it1.pos <= it2.pos;
}

inline bool operator > (const sorted_sequence::iterator& it1, const sorted_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator>");
    return it1.pos > it2.pos;
}

inline bool operator >=(const sorted_sequence::iterator& it1, const sorted_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator>=");
    return it1.pos >= it2.pos;
}

inline int operator -(const sorted_sequence::iterator& it1, const sorted_sequence::iterator& it2)
{
    if (it1.s != it2.s) throw USER_EXCEPTION2(SE1003, "Iterators mismatch in xptr_sequence::operator-");
    return it1.pos - it2.pos;
}

inline sorted_sequence::iterator operator -(const sorted_sequence::iterator& it, int i)
{
    return sorted_sequence::iterator(it.pos - i, it.s);
}

inline sorted_sequence::iterator operator +(const sorted_sequence::iterator& it, int i)
{
    return sorted_sequence::iterator(it.pos + i, it.s);
}

#endif /* _SORTED_SEQUENCE_H */

