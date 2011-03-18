#ifndef __SortedSequence_h__
#define __SortedSequence_h__

#include "common/sedna.h"
#include "tr/executor/base/seq_common.h"
#include "tr/executor/base/tuple.h"
#include "tr/executor/base/ITupleSerializer.h"
#include <queue>

//TODO: add CHECK_TIMER_FLAG

//FIXME:Update this description:

/* How it works:
 * We store tuples in accumulator, until the number of blocks used to store them will be greater than
 * MAX_BLOCKS_IN_CHAIN or we found end of input sequence
 *
 * We will repeat action described above until end of sequence will be found
 *
 * Then, we build a heap, using chains as nodes.
 */

#define GET_FREE_SPACE(p) (shft)((uint32_t)PAGE_SIZE - ((XADDR_INT(p)) & PAGE_REVERSE_BIT_MASK))
#define MAX_BLOCKS_IN_CHAIN 500

#define PTR_BLK_SIZE ((PAGE_SIZE - sizeof(seq_blk_hdr)) / sizeof(data_ptr))
#define DATA_BLK_SIZE (PAGE_SIZE - sizeof(seq_blk_hdr))

#ifndef _data_ptr_
#define _data_ptr_
struct data_ptr
{
    xptr value;
    shft size;
};
#endif

//Sorted chain

class SortedSequence {

    struct SSChain
    {
    //Chain stores data in blocks in such format:
    //(size of tuple1)(tuple1)(size of tuple2)(tuple2)...(size of last tuple)(last tuple)
    public:
	char *value;	//Value of the current element in chain
	size_t size;	//Size of the current element in chain
	void add(void *buf, size_t size);
	void set_end();	//Ends writing to chain and initializes value and size fields
	void next(); 	//Writing next tuple to value and it's size to field size
	inline bool empty() { return to_read == 0; }

	SortedSequence *parent; //Parent of SSChain, we will use it's block memory
	SSChain() { initialized = false; };
	void init(SortedSequence *_parent_); //Should be called explicitly as a constructor
	~SSChain();

    private:
	bool initialized;
	xptr first_block;	//Pointer to the stored values
	xptr place;		//Where to write new element in adding phase and from where to read in reading phase
	int to_read;	//counts amount of unread elements in chain
    };

    struct SSChainPtr
    {
    //Pointer to SSChain to use with STL containers
    public:
	SSChain *ptr;

	SSChainPtr(SSChain *_ptr_)
	{
	    ptr = _ptr_;
	}

	bool operator<(const SSChainPtr &x) const
	{
	    return ptr -> parent -> serializer -> compare(this -> ptr -> value, this -> ptr -> size, x.ptr -> value, x.ptr -> size) < 0;
	}
    };

private:
    friend struct SSChain;
    friend struct SSChainPtr;

    void init();
    void free();

    //Elements accumulator
    void init_accumulator(); //Initializes elements accumulator
    void reinit_accumulator();
    void free_accumulator(); //Returns used in accumulator blocks
    void finalize_accumulator(); //Sorts it and initializes new chain
    void make_new_chain(int ptrs_in_last);

    xptr first_val_block, first_ptr_block; //Pointer to the first blocks holding values and pointers to them
    xptr val_place, ptr_place; //Places to write a value and pointer to this value, respectively
    int blk_cnt; //Amount of used blocks in accumulator
    int el_cnt; //Amount of stored elements in accumulator

    xptr get_ptr(xptr &block, int ind);
    size_t get_val(void *buf, xptr &block, int ind);
    void swap_pointers(xptr p1, xptr p2);

    xptr merge_blocks(xptr &p1, int size1, xptr &p2, int size2); //Merging two sorted sequence of blocks
    void in_block_sort(xptr &p, int left, int right); //Sorting elements in block
    void blocks_sort(int ptrs_in_last); //Merging already sorted blocks

    //Heap
    std::vector < SSChain > chains;
    std::priority_queue < SSChainPtr , std::vector < SSChainPtr >, std::less < std::vector < SSChainPtr >::value_type > > heap;

    //Buffers used for serialization/deserialization and sorting, always allocated
    //Work with them very carefully
    char *buf1, *buf2;

    //Block memory data and methods
    std::vector < xptr > empty_blocks; //Array of empty blocks
    xptr get_free_block(); //Allocates new block. New block always has XNULL value of nblk pointer
    xptr add_block_to_chain(const xptr &p); //Adds free block after xptr p and returns it's address

    xptr next_block(const xptr &p); //Returns pointer to the next block in sequence

    xptr readData(void *buf, size_t size, const xptr &place);
    //Reads data of given size from block memory. Returns pointer to the place after red data
    xptr writeData(void *buf, size_t size, const xptr &place);
    //Writes data of given size to block memory. Returns pointer to the place after written data

    bool finalized;
    ITupleSerializer *serializer;

public:
    SortedSequence(ITupleSerializer *serializer);
    ~SortedSequence();

    void add(const tuple &t);
    void sort();
    void next(tuple &t);
    void clear();
};

#endif /*__SortedSequence_h__*/
