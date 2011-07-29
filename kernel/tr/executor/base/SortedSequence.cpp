/*
 * File:  SortedSequence.cpp
 * Copyright (C) 2011 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/executor/base/SortedSequence.h"

#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <algorithm>

#define GET_FREE_SPACE(p) (shft)((uint32_t)PAGE_SIZE - ((XADDR_INT(p)) & PAGE_REVERSE_BIT_MASK))
#define SS_MAX_BLOCKS_IN_CHAIN 10

//Amount of data_ptrs in one block
#define SS_PTR_BLK_SIZE ((PAGE_SIZE - sizeof(ss_blk_hdr)) / sizeof(data_ptr))
//Size of block
#define SS_DATA_BLK_SIZE (PAGE_SIZE - sizeof(ss_blk_hdr))
//Pointer to the block header
#define SS_BLK_HEADER(p) ((ss_blk_hdr *)(XADDR(BLOCKXPTR(p))))

//Initialization and deinitialization

void SortedSequence::init()
{
    finalized = false;

    //Initializating accumulator
    initAccumulator();

    //Initializating heap
    heap.resize(1);
    heapsize = 0;

    //Initializating buffers;
    buf1 = new char[SS_DATA_BLK_SIZE];
    buf2 = new char[SS_DATA_BLK_SIZE];
}

void SortedSequence::free()
{
    freeAccumulator();
    //Clearing heap
    heap.clear();
    heap.resize(1);
    heapsize = 0;
    //Destruction of chains and block memory data
    for (unsigned int i = 0; i < chains.size(); i++)
    {
        delete chains[i];
    }
    chains.clear();
    for (unsigned int i = 0; i < emptyBlocks.size(); i++)
    {
        vmm_delete_block(emptyBlocks[i]);
    }
    emptyBlocks.clear();
    //Deinitializating buffers;
    delete[] buf1;
    delete[] buf2;
}

SortedSequence::SortedSequence(ITupleSerializer *serializer)
{
    this -> serializer = serializer;
    init();
}

SortedSequence::~SortedSequence()
{
    free();
}

//*******************************************************************
//Interface methods
//*******************************************************************

void SortedSequence::add(const tuple& t)
{
    if (finalized)
    {
        throw USER_EXCEPTION2(SE1003, "Failed to add an item to already sorted sequence");
    }

    elementsCount++;

    //Serializing data
    void *tuple_buf = buf1; //Buffer for serialization
    size_t size = serializer -> serialize(t, tuple_buf);

    //Writing pointer
    data_ptr tmp_data_ptr;
    tmp_data_ptr.value = valPlace;
    tmp_data_ptr.size = size;
    xptr tmp_ptr = ptrPlace;
    ptrPlace = writeData((void *)(&tmp_data_ptr), sizeof(data_ptr), ptrPlace, false);
    if (!same_block(tmp_ptr, ptrPlace))
    {
        blocksCount++;
    }

    //Writing value
    tmp_ptr = valPlace;
    valPlace = writeData(tuple_buf, size, valPlace, true);
    if (!same_block(tmp_ptr, valPlace))
    {
        blocksCount++;
    }

    //Checking if accumulator is full
    if (blocksCount > SS_MAX_BLOCKS_IN_CHAIN)
    {
        finalizeAccumulator();
        reinitAccumulator();
    }
}

void SortedSequence::sort()
{
    if (finalized)
    {
        return;
    }
    finalized = true;
    finalizeAccumulator();
    buildHeapFromChains();
}

void SortedSequence::next(tuple &t)
{
    if (heapEmpty())
    {
        t.set_eos();
        return;
    }
    else
    {
        //Extracting minimum from heap
        SSChain *best = heapPop();
        serializer -> deserialize(t, best -> value, best -> size);
        if (!best -> empty())
        {
            best -> next();
            //Putting chain back to heap
            heapPush(best);
        }
    }
}

void SortedSequence::clear()
{
    free();
    init();
}

//*******************************************************************
//Accumulator methods
//*******************************************************************

void SortedSequence::initAccumulator()
{
    firstPtrBlock = getFreeBlock();
    firstValBlock = getFreeBlock();
    ptrPlace = firstPtrBlock;
    valPlace = firstValBlock;
    blocksCount = 2, elementsCount = 0;
}

void SortedSequence::reinitAccumulator()
{
    freeAccumulator();
    initAccumulator();
}

void SortedSequence::freeAccumulator()
{
    xptr it = firstPtrBlock, tmp;
    do
    {
        tmp = it;
        it = nextBlock(it);
        emptyBlocks.push_back(tmp);
    }
    while (it != XNULL);
    firstPtrBlock = XNULL;

    it = firstValBlock;
    do
    {
        tmp = it;
        it = nextBlock(it);
        emptyBlocks.push_back(tmp);
    }
    while (it != XNULL);
    firstValBlock = XNULL;
}

void SortedSequence::finalizeAccumulator()
{
    //In-block sorting accumulator (qsort)
    if (elementsCount == 0)
    {
        return;
    }
    xptr tmp_ptr, tmp_ptr_next;
    tmp_ptr = firstPtrBlock;
    int ptrsInBlock = elementsCount;
    while (tmp_ptr != XNULL)
    {
        tmp_ptr_next = nextBlock(tmp_ptr);
        if (tmp_ptr_next != XNULL)
        {
            inBlockSort(tmp_ptr, SS_PTR_BLK_SIZE);
            ptrsInBlock -= SS_PTR_BLK_SIZE;
        }
        else
        {
            inBlockSort(tmp_ptr, ptrsInBlock);
        }
        tmp_ptr = tmp_ptr_next;
    }
    //Merging sorted blocks
    blocksSort(ptrsInBlock);
    makeNewChain(ptrsInBlock);
}

void SortedSequence::getPtr(data_ptr &ptr, xptr block, int ind)
{
    readData(&ptr, sizeof(data_ptr), block + ind * sizeof(data_ptr));
    return;
}

size_t SortedSequence::getVal(void *buf, xptr block, int ind)
{
    xptr tmp_ptr = block + ind * sizeof(data_ptr);
    CHECKP(tmp_ptr);
    size_t size = ((data_ptr *) XADDR(tmp_ptr)) -> size;
    readData(buf, size, ((data_ptr *) XADDR(tmp_ptr)) -> value);
    return size;
}

//TupleComparator compare method implementation
bool SortedSequence::TupleComparator::operator() (const data_ptr ptr1, const data_ptr ptr2)
{
    void *tuple_buf1 = parent -> buf1;
    void *tuple_buf2 = parent -> buf2;

    size_t tuple_size1 = ptr1.size;
    parent -> readData(tuple_buf1, tuple_size1, ptr1.value);
    size_t tuple_size2 = ptr2.size;
    parent -> readData(tuple_buf2, tuple_size2, ptr2.value);

    return parent -> serializer -> compare(tuple_buf1, tuple_size1, tuple_buf2, tuple_size2) < 0;
}

void SortedSequence::inBlockSort(xptr p, int amount)
{
    SortedSequence::TupleComparator comparator(this);

    //Tricky: We should copy all the pointers in block to other place to avoid CHECKP on getting any pointer

    CHECKP(p);
    data_ptr * data = (data_ptr *) XADDR(p);
    std::vector <data_ptr> indexes(data, data + amount);

    std::sort(indexes.begin(), indexes.end(), comparator);

    WRITEP(p);
    for (int i = 0; i < amount; i++)
    {
        memcpy(XADDR(p + i * sizeof(data_ptr)), &indexes[i], sizeof(data_ptr));
    }
}

xptr SortedSequence::mergeBlocks(xptr p1, int size1, xptr p2, int size2)
{
    //Pointers to new sequence:
    xptr first_block = getFreeBlock();
    xptr ptr_place = first_block;
    xptr tmp_ptr;

    //Buffers for merging:
    data_ptr ptr1, ptr2;
    void *tuple_buf1 = buf1;
    void *tuple_buf2 = buf2;
    size_t tuple_size1, tuple_size2;

    //Reading pointers and values
    if (size1 > 0)
    {
        getPtr(ptr1, p1, 0);
        tuple_size1 = getVal(tuple_buf1, p1, 0);
    }
    if (size2 > 0)
    {
        getPtr(ptr2, p2, 0);
        tuple_size2 = getVal(tuple_buf2, p2, 0);
    }

    //Counters of already written elements:
    int written1 = 0, written2 = 0;

    while (written1 + written2 < size1 + size2)
    {

        if (written2 == size2 || (written1 < size1 && serializer -> compare(tuple_buf1, tuple_size1, tuple_buf2, tuple_size2) < 0))
        {
            //Element in the first sequence less than element in second
            ptr_place = writeData((void *)(&ptr1), sizeof(data_ptr), ptr_place, false);
            written1++;

            if (written1 % SS_PTR_BLK_SIZE == 0 || size1 == written1)
            {
                //If we red a whole block or wrote all elements we should free block
                xptr tmp_block = p1;
                p1 = nextBlock(tmp_block);
                emptyBlocks.push_back(tmp_block);
            }
            if (written1 < size1)
            {
                getPtr(ptr1, p1, written1 % SS_PTR_BLK_SIZE);
                tuple_size1 = getVal(tuple_buf1, p1, written1 % SS_PTR_BLK_SIZE);
            }
        }
        else
        {
            //Otherwise
            ptr_place = writeData((void *)(&ptr2), sizeof(data_ptr), ptr_place, false);
            written2++;

            if (written2 % SS_PTR_BLK_SIZE == 0 || size2 == written2)
            {
                //If we red a whole block or wrote all elements we should free block
                xptr tmp_block = p2;
                p2 = nextBlock(tmp_block);
                emptyBlocks.push_back(tmp_block);
            }
            if (written2 < size2)
            {
                getPtr(ptr2, p2, written2 % SS_PTR_BLK_SIZE);
                tuple_size2 = getVal(tuple_buf2, p2, written2 % SS_PTR_BLK_SIZE);
            }
        }
    }
    return first_block;
}

void SortedSequence::blocksSort(int ptrsInLast)
{
    //Vector of blocks and number of containing elements
    typedef std::vector < xptr > t_blk_ptr_array;
    typedef std::vector < int > t_blk_size_array;

    t_blk_ptr_array tbpa1, tbpa2;
    t_blk_size_array tbsa1, tbsa2;

    t_blk_ptr_array *blocks = &tbpa1, *old_blocks = &tbpa2;
    t_blk_size_array *sizes = &tbsa1, *old_sizes = &tbsa2;

    t_blk_ptr_array *tmp_ptr;
    t_blk_size_array *tmp_size;

    for (xptr it = firstPtrBlock; it != XNULL; it = nextBlock(it))
    {
        blocks -> push_back(it);
        sizes -> push_back(SS_PTR_BLK_SIZE);
    }

    sizes -> at(sizes -> size() - 1) = ptrsInLast;

    while (blocks -> size() > 1)
    {
        tmp_ptr = blocks;
        blocks = old_blocks;
        old_blocks = tmp_ptr;
        tmp_size = sizes;
        sizes = old_sizes;
        old_sizes = tmp_size;

        for (unsigned i = 1; i < old_blocks -> size(); i += 2)
        {
            blocks -> push_back(mergeBlocks(old_blocks -> at(i - 1), old_sizes -> at(i - 1), old_blocks -> at(i), old_sizes -> at(i)));
            sizes -> push_back(old_sizes -> at(i - 1) + old_sizes -> at(i));
        }
        if (old_blocks -> size() % 2 == 1)
        {
            blocks -> push_back(old_blocks -> back());
            sizes -> push_back(old_sizes -> back());
        }
        old_blocks -> clear();
        old_sizes -> clear();
    }
    firstPtrBlock = blocks -> front();
}

void SortedSequence::makeNewChain(int ptrsInLast)
{
    SSChain *new_chain = new SSChain(this);

    void *tuple_buf = buf1;
    size_t tuple_size;

    xptr next_it;
    for (xptr it = firstPtrBlock; it != XNULL; it = next_it)
    {
        next_it = nextBlock(it);
        for (unsigned i = 0; i < (next_it == XNULL ? ptrsInLast : SS_PTR_BLK_SIZE); i++)
        {
            tuple_size = getVal(tuple_buf, it, i);
            new_chain -> add(tuple_buf, tuple_size);
        }
    }
    new_chain -> setEnd();
    chains.push_back(new_chain);
}

//*******************************************************************
//SSChain structure methods
//*******************************************************************

SortedSequence::SSChain::SSChain(SortedSequence *_parent_)
{
    parent = _parent_;

    value = new char[SS_DATA_BLK_SIZE];
    firstBlock = parent -> getFreeBlock();
    place = firstBlock;
    toRead = 0;
}

SortedSequence::SSChain::~SSChain()
{
    if (!initialized) return;

    delete[] value;
    xptr it = firstBlock, tmp;
    do
    {
        tmp = it;
        it = parent -> nextBlock(it);
        parent -> emptyBlocks.push_back(tmp);
    }
    while (it != XNULL);
}

void SortedSequence::SSChain::add(void *buf, size_t tuple_size)
{
    place = parent -> writeData((void *)(&tuple_size), sizeof(size_t), place, true);
    place = parent -> writeData(buf, tuple_size, place, true);
    toRead++;
}

void SortedSequence::SSChain::setEnd()
{
    place = firstBlock;
    next();
}

void SortedSequence::SSChain::next()
{
    place = parent -> readData((void *)(&size), sizeof(size_t), place);
    place = parent -> readData(value, size, place);
    toRead--;
}

//*******************************************************************
//Working with heap
//*******************************************************************

void SortedSequence::buildHeapFromChains()
{
    heap.resize(1);
    for (unsigned int i = 0; i < chains.size(); i++)
    {
        heap.push_back(chains[i]);
    }
    heapsize = heap.size() - 1;
    for (int i = (heapsize == 0 ? 0 : heapsize / 2); i >= 1; i--)
    {
        siftDown(i);
    }
}

void SortedSequence::heapPush(SSChain *chain)
{
    heap.push_back(chain);
    heapsize++;
    siftUp(heapsize);
}

SortedSequence::SSChain* SortedSequence::heapPop()
{
    SSChain *result = heap[1];
    heap[1] = heap[heapsize];
    heap.resize(heapsize);
    heapsize--;
    siftDown(1);
    return result;
}

void SortedSequence::siftUp(int x)
{
    int pr = x / 2;
    while (pr > 0 && serializer -> compare(heap[x] -> value, heap[x] -> size, heap[pr] -> value, heap[pr] -> size) < 0)
    {
        SSChain *tmp = heap[x];
        heap[x] = heap[pr];
        heap[pr] = tmp;
        x = pr;
        pr = x / 2;
    }
}

void SortedSequence::siftDown(int x)
{
    int left = x * 2, right = x * 2 + 1, min = x;
    if (left <= heapsize && serializer -> compare(heap[left] -> value, heap[left] -> size, heap[min] -> value, heap[min] -> size) < 0)
    {
        min = left;
    }
    if (right <= heapsize && serializer -> compare(heap[right] -> value, heap[right] -> size, heap[min] -> value, heap[min] -> size) < 0)
    {
        min = right;
    }
    if (min != x)
    {
        SSChain *tmp = heap[x];
        heap[x] = heap[min];
        heap[min] = tmp;
        siftDown(min);
    }
}

//*******************************************************************
//Working with block memory
//*******************************************************************

xptr SortedSequence::getFreeBlock()
{
    xptr block = XNULL;
    if (!emptyBlocks.empty())
    {
        block = BLOCKXPTR(emptyBlocks.back());
        emptyBlocks.pop_back();
    }
    else
    {
        vmm_alloc_tmp_block(&block);
    }
    WRITEP(block);
    SS_BLK_HEADER(block) -> nblk = XNULL;
    return block + sizeof(ss_blk_hdr);
}

xptr SortedSequence::addBlockToChain(xptr p)
{
    xptr new_block = getFreeBlock();
    WRITEP(p);
    SS_BLK_HEADER(p) -> nblk = new_block;
    return new_block;
}

xptr SortedSequence::readData(void *buf, size_t size, xptr place)
{
    size_t fspace = GET_FREE_SPACE(place);
    if (fspace <= size)
    {
        //We should read from two blocks
        CHECKP(place);
        xptr next_block = SS_BLK_HEADER(place) -> nblk;
        memcpy((char *) buf, XADDR(place), fspace);
        CHECKP(next_block);
        memcpy(((char *) buf + fspace), XADDR(next_block), size - fspace);
        return next_block + size - fspace;
    }
    else
    {
        //From one block
        CHECKP(place);
        memcpy((char *) buf, XADDR(place), size);
        return place + size;
    }
}

xptr SortedSequence::writeData(void* buf, size_t size, xptr place, bool splittingAllowed)
{
    size_t fspace = GET_FREE_SPACE(place);
    if (fspace <= size)
    {
        //Data doesn't fit to given block
        if (!splittingAllowed)
        {
            xptr new_block = addBlockToChain(place);
            WRITEP(new_block);
            memcpy(XADDR(new_block), ((char *)buf), size);
            return new_block + size;
        }
        else
        {
            WRITEP(place);
            xptr new_block = addBlockToChain(place);
            memcpy(XADDR(place), (char *) buf, fspace);
            WRITEP(new_block);
            memcpy(XADDR(new_block), ((char *) buf + fspace), size - fspace);
            return new_block + size - fspace;
        }
    }
    else
    {
        //Data fits to block
        WRITEP(place);
        memcpy(XADDR(place), (char *) buf, size);
        return place + size;
    }
}

xptr SortedSequence::nextBlock(xptr p)
{
    CHECKP(p);
    return SS_BLK_HEADER(p) -> nblk;
}
