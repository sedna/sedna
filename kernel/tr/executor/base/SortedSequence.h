/*
 * File:  SortedSequence.h
 * Copyright (C) 2011 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __SortedSequence_h__
#define __SortedSequence_h__

#include "common/sedna.h"
#include "tr/executor/base/tuple.h"
#include "tr/executor/base/ITupleSerializer.h"
#include <queue>

//TODO: add CHECK_TIMER_FLAG

/* How it works:
 * We store tuples in accumulator, until the number of blocks used to store will be greater than
 * MAX_BLOCKS_IN_CHAIN or eos will be found. Then we sort elements in accumulator, and form a new SSChain object.
 *
 * We will repeat action described above until eos will be found
 *
 * Then, we build a heap, using SSChains as nodes.
 * While retrieving elements from SortedSequence, we are merging SSChains in heap.
 *
 */

struct ss_blk_hdr
{
    vmm_sm_blk_hdr sm_vmm;  // sm/vmm parameters
    xptr nblk;              // next block
};

#ifndef _data_ptr_
#define _data_ptr_
struct data_ptr
{
    xptr value;
    shft size;
};
#endif

class SortedSequence
{
    //Comparator for STL sort
    class TupleComparator
    {
    private:
        SortedSequence *parent;

    public:
        TupleComparator(SortedSequence *parent)
        {
            this -> parent = parent;
        }

        bool operator() (const data_ptr ptr1, const data_ptr ptr2);
    };

    //Sorted chain
    struct SSChain
    {
        //Chain stores data in blocks in such format:
        //(size of tuple1)(tuple1)(size of tuple2)(tuple2)...(size of last tuple)(last tuple)
    public:
        char *value; //Value of the current element in chain
        size_t size; //Size of the current element in chain
        void add(void *buf, size_t size);
        void setEnd(); //Ends writing to chain and initializes value and size fields
        void next();  //Writing next tuple to value and it's size to field size
        inline bool empty()
        {
            return toRead == 0;
        }

        SortedSequence *parent; //Parent of SSChain, we will use it's block memory
        SSChain(SortedSequence *_parent_);
        ~SSChain();

    private:
        xptr firstBlock; //Pointer to the stored values
        xptr place;  //Where to write new element in adding phase and from where to read in reading phase
        int toRead; //counts amount of unread elements in chain
    };

private:
    void init();
    void free();

    //Elements accumulator
    void initAccumulator(); //Initializes elements accumulator
    void reinitAccumulator();
    void freeAccumulator(); //Returns used in accumulator blocks
    void finalizeAccumulator(); //Sorts it and initializes new chain
    void makeNewChain(int ptrsInLast);

    xptr firstValBlock, firstPtrBlock; //Pointer to the first blocks holding values and pointers to them
    xptr valPlace, ptrPlace; //Places to write a value and pointer to this value, respectively
    int blocksCount; //Amount of used blocks in accumulator
    int elementsCount; //Amount of stored elements in accumulator

    void getPtr(data_ptr &ptr, xptr block, int ind); //Returns ind-th pointer in given block
    size_t getVal(void *buf, xptr block, int ind);    //Returns tuple, pointed by ind-th pointer in given block

    void inBlockSort(xptr p, int amount);    //Sorting elements in block using QSort

    void blocksSort(int ptrsInLast);    //Merging already sorted blocks
    xptr mergeBlocks(xptr p1, int size1, xptr p2, int size2);    //Merging two sorted sequences of blocks

    //Chains
    std::vector < SSChain* > chains; //Stores all created chains

    //Heap
    std::vector < SSChain* > heap;
    int heapsize;

    //Heap interface
    void heapPush(SSChain *chain);
    SSChain* heapPop();
    inline bool heapEmpty()
    {
        return heapsize == 0;
    }

    void buildHeapFromChains(); //Builds heap from given set of chains

    //Internal heap methods
    void siftUp(int x);
    void siftDown(int x);

    //Buffers used for serialization/deserialization and sorting, always allocated
    //Work with them very carefully
    char *buf1, *buf2;

    //Block memory data and methods
    xptr getFreeBlock(); //Allocates new block. New block always has XNULL value of nblk pointer
    xptr addBlockToChain(xptr p);    //Adds free block after xptr p and returns pointer b to it

    xptr nextBlock(xptr p);    //Returns pointer to the next block in sequence

    xptr readData(void *buf, size_t size, xptr place);
    //Reads data of given size from block memory. Returns pointer to the place after red data
    xptr writeData(void *buf, size_t size, xptr place, bool splittingAllowed);
    //Writes data of given size to block memory. Returns pointer to the place after written data

    bool finalized; //Indicates that the sequence already sorted and we can't add new elements
    ITupleSerializer *serializer;

public:
    SortedSequence(ITupleSerializer *serializer);
    ~SortedSequence();

    static void freeBlockPool();

    void add(const tuple &t);
    void sort();
    void next(tuple &t);
    void clear();
};

#endif /*__SortedSequence_h__*/
