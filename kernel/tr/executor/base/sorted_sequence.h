#ifndef _SORTED_SEQUENCE_H
#define _SORTED_SEQUENCE_H
#include "tuple.h"
#include "vmm.h"
#include "seq_common.h"

#define MAX_BLOCKS_IN_CHAIN 500 //should be changed after meeting with Andrey
#define GET_FREE_SPACE(p) (shft)(PAGE_SIZE - (uint32)((p).addr) & PAGE_REVERSE_BIT_MASK)
#ifndef min
#define min(x,y) ((x) < (y) ? (x) : (y))
#endif

typedef int (*compare_fn)(xptr v1,xptr v2);
typedef int (*get_size_fn)(tuple& t);
typedef void (*serialize_fn)(tuple& t,xptr v1);
typedef void (*serialize_2_blks_fn)(tuple& t,xptr& v1,shft size1,xptr& v2);
typedef tuple (*deserialize_fn)(xptr& v1);
struct data_ptr
{
	xptr value;
	shft size;
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
        
        tuple operator*() { return s->get(pos); }
        tuple operator[](int i) { return s->get(i); }


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
	bool finalized;
	xptr ptr_place;
	xptr val_place;
	xptr bblk_in_chain;
	int blk_cnt;

   

  //  void init_blks();
	
	void sort1(int off, int len); 
	void swap( int a, int b);
	int med3( int a, int b, int c) ;
	void vecswap(int a, int b, int n);
    xptr get_free_block();
	void in_mem_sort();
	void in_mem_order_data();
	void merge_stack(bool final);
	void unlock_memory();
	void set_next_ptr_with_free(xptr& ptr);
	xptr merge_sequences(xptr s1,xptr s2, bool final);
	xptr get_ptr(int pos);
	inline void  get_val(xptr ptr,xptr& val)
	{
		CHECKP(ptr);
		val=((data_ptr*)XADDR(ptr))->value;
	}
    xptr get_data(int pos);
	int get_size_in_mem();
	void copy_data_to_new_place(xptr ptr,xptr& place);
	void copy_ptr_to_new_place(xptr ptr,xptr& place,bool marking);
	void set_next_block_in_chain(xptr& place, bool marking=false);	
	char* temp_buffer;
	shft buf_length;
	


public:

    sorted_sequence(compare_fn _compareFN_, get_size_fn _getSizeFN_, serialize_fn _serializeFN_,
	serialize_2_blks_fn _serialize2FN_,	deserialize_fn _deserializeFN_);
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

    void add(tuple &p);
    tuple get(const iterator& it);
    tuple get(int pos);    
    tuple operator[](int i)
    { 
        return get(i);
    }
    void sort();
    
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
#endif