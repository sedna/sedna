#include "tr/executor/base/SortedSequence.h"
#include <ctime>

#include <stdio.h>
#include <stdlib.h>

//Initialization and deinitialization

void SortedSequence::init()
{
    finalized = false;

    //Initializating accumulator
    initAccumulator();

    //Initializating heap
    heap.resize ( 1 );
    heapsize = 0;

    //Initializating buffers;
    buf1 = new char[DATA_BLK_SIZE];
    buf2 = new char[DATA_BLK_SIZE];
}

void SortedSequence::free()
{
    freeAccumulator();
    //Clearing heap
    heap.clear();
    heap.resize ( 1 );
    heapsize = 0;
    //Destruction of chains and block memory data
    for ( unsigned int i = 0; i < chains.size(); i++ )
    {
        delete chains[i];
    }
    chains.clear();
    for ( unsigned int i = 0; i < emptyBlocks.size(); i++ )
    {
        vmm_delete_block ( emptyBlocks[i] );
    }
    emptyBlocks.clear();
    //Deinitializating buffers;
    delete[] buf1;
    delete[] buf2;
}

SortedSequence::SortedSequence ( ITupleSerializer *serializer )
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

void SortedSequence::add ( const tuple& t )
{
    if ( finalized )
    {
        throw USER_EXCEPTION2 ( SE1003, "Failed to add an item to already sorted sequence" );
    }

    //1. Finding place in accumulator to store new tuple
    if ( elementsCount % PTR_BLK_SIZE == 0 && elementsCount != 0 )
    {
        //if pointers block is full
        ptrPlace = addBlockToChain ( ptrPlace );
        blocksCount++;
    }

    elementsCount++;

    //2. Serializing data to this place
    void *tuple_buf = buf1; //Buffer for serialization
    size_t size = serializer -> serialize ( t, tuple_buf );

    //2.a Writing pointer
    data_ptr tmp_data_ptr;
    tmp_data_ptr.value = valPlace;
    tmp_data_ptr.size = size;
    ptrPlace = writeData ( ( void * ) ( &tmp_data_ptr ), sizeof ( data_ptr ), ptrPlace );

    //2.b Writing value
    xptr tmp_ptr = BLOCKXPTR ( valPlace );
    valPlace = writeData ( tuple_buf, size, valPlace );
    if ( tmp_ptr != BLOCKXPTR ( valPlace ) )
    {
        blocksCount++;
    }

    //3. Checking if accumulator is full
    if ( blocksCount > MAX_BLOCKS_IN_CHAIN )
    {
        finalizeAccumulator();
        reinitAccumulator();
    }
}

void SortedSequence::sort()
{
    if ( finalized )
    {
        return;
    }
    finalized = true;
    finalizeAccumulator();
    buildHeapFromChains();
}

void SortedSequence::next ( tuple &t )
{
    if ( heapEmpty() )
    {
        t.set_eos();
        return;
    }
    else
    {
        //Extracting minimum from heap
        SSChain *best = heapPop();
        serializer -> deserialize ( t, best -> value, best -> size );
        if ( !best -> empty() )
        {
            best -> next();
            //Putting chain back to heap
            heapPush ( best );
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
        it = nextBlock ( it );
        emptyBlocks.push_back ( tmp );
    }
    while ( it != XNULL );
    firstPtrBlock = XNULL;

    it = firstValBlock;
    do
    {
        tmp = it;
        it = nextBlock ( it );
        emptyBlocks.push_back ( tmp );
    }
    while ( it != XNULL );
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
    while ( tmp_ptr != XNULL )
    {
        tmp_ptr_next = nextBlock ( tmp_ptr );
        if ( tmp_ptr_next != XNULL )
        {
            inBlockSort ( tmp_ptr, 0, PTR_BLK_SIZE - 1 );
        }
        else
        {
            inBlockSort ( tmp_ptr, 0, elementsCount % PTR_BLK_SIZE - 1 );
        }
        tmp_ptr = tmp_ptr_next;
    }
    //Merging sorted blocks
    blocksSort ( elementsCount % PTR_BLK_SIZE );
    makeNewChain ( elementsCount % PTR_BLK_SIZE );
}

xptr SortedSequence::getPtr ( xptr block, int ind )
{
    return block + ind * sizeof ( data_ptr );
}

size_t SortedSequence::getVal ( void *buf, xptr block, int ind )
{
    xptr tmp_ptr = block + ind * sizeof ( data_ptr );
    CHECKP ( tmp_ptr );
    size_t size = ( ( data_ptr * ) XADDR ( tmp_ptr ) ) -> size;
    readData ( buf, size, ( ( data_ptr * ) XADDR ( tmp_ptr ) ) -> value );
    return size;
}

void SortedSequence::swapPointers ( xptr p1, xptr p2 )
{
    data_ptr *buf1 = new data_ptr;
    data_ptr *buf2 = new data_ptr;

    readData ( buf1, sizeof ( data_ptr ), p1 );
    readData ( buf2, sizeof ( data_ptr ), p2 );

    writeData ( buf1, sizeof ( data_ptr ), p2 );
    writeData ( buf2, sizeof ( data_ptr ), p1 );

    delete buf1;
    delete buf2;
}

void SortedSequence::inBlockSort ( xptr p, int left, int right )
{
    if ( right - left + 1 <= 7 )
    {
        //Insertion sort in small cases
        void *tuple_buf1 = buf1;
        void *tuple_buf2 = buf2;
        size_t tuple_size1, tuple_size2;
        for ( int i = left + 1; i <= right; i++ )
        {
            tuple_size1 = getVal ( tuple_buf1, p, i );
            int j = i - 1;
            do
            {
                tuple_size2 = getVal ( tuple_buf2, p, j );
                if ( serializer -> compare ( tuple_buf1, tuple_size1, tuple_buf2, tuple_size2 ) < 0 )
                {
                    swapPointers ( getPtr ( p, j ), getPtr ( p, j + 1 ) );
                }
                else
                {
                    break;
                }
                j--;
            }
            while ( j >= 0 );
        }
    }
    else
    {
        //Quick sort in other cases
        void *pivot = buf1;
        void *tuple_buf = buf2;
        size_t pivot_size, tuple_size;

        int i = left, j = right;

        //Randomly choosing partition element
        int pivot_ind = (left + right) / 2;
        pivot_size = getVal ( pivot, p, pivot_ind );

        while ( i < j )
        {
            tuple_size = getVal ( tuple_buf, p, i );
            while ( serializer -> compare ( tuple_buf, tuple_size, pivot, pivot_size ) < 0 )
            {
                i++;
                tuple_size = getVal ( tuple_buf, p, i );
            }

            tuple_size = getVal ( tuple_buf, p, j );
            while ( serializer -> compare ( tuple_buf, tuple_size, pivot, pivot_size ) > 0 )
            {
                j--;
                tuple_size = getVal ( tuple_buf, p, j );
            }

            if ( i <= j )
            {
                swapPointers ( getPtr ( p, i ), getPtr ( p, j ) );
                i++, j--;
            }
        }

        if ( i < right ) inBlockSort ( p, i, right );
        if ( left < j ) inBlockSort ( p, left, j );
    }
}

xptr SortedSequence::mergeBlocks ( xptr p1, int size1, xptr p2, int size2 )
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
    readData ( ( void * ) ( &ptr1 ), sizeof ( data_ptr ), getPtr ( p1, 0 ) );
    tuple_size1 = getVal ( tuple_buf1, p1, 0 );
    readData ( ( void * ) ( &ptr2 ), sizeof ( data_ptr ), getPtr ( p2, 0 ) );
    tuple_size2 = getVal ( tuple_buf2, p2, 0 );

    //Counters of already written elements:
    int written1 = 0, written2 = 0;

    while ( written1 + written2 < size1 + size2 )
    {
        if ( ( written1 + written2 ) % PTR_BLK_SIZE == 0 && ( written1 + written2 ) != 0 )
        {
            //if we filled a whole block
            ptr_place = addBlockToChain ( ptr_place );
        }

        if ( written2 == size2 || ( written1 < size1 && serializer -> compare ( tuple_buf1, tuple_size1, tuple_buf2, tuple_size2 ) < 0 ) )
        {
            //Element in the first sequence less than element in second
            ptr_place = writeData ( ( void * ) ( &ptr1 ), sizeof ( data_ptr ), ptr_place );
            written1++;

            if ( written1 % PTR_BLK_SIZE == 0 || size1 == written1 )
            {
                //If we red a whole block or wrote all elements we should free block
                xptr tmp_block = p1;
                p1 = nextBlock ( tmp_block );
                emptyBlocks.push_back ( tmp_block );
            }
            if ( written1 < size1 )
            {
                readData ( ( void * ) ( &ptr1 ), sizeof ( data_ptr ), getPtr ( p1, written1 % PTR_BLK_SIZE ) );
                tuple_size1 = getVal ( tuple_buf1, p1, written1 % PTR_BLK_SIZE );
            }
        }
        else
        {
            //Otherwise
            ptr_place = writeData ( ( void * ) ( &ptr2 ), sizeof ( data_ptr ), ptr_place );
            written2++;

            if ( written2 % PTR_BLK_SIZE == 0 || size2 == written2 )
            {
                //If we red a whole block or wrote all elements we should free block
                xptr tmp_block = p2;
                p2 = nextBlock ( tmp_block );
                emptyBlocks.push_back ( tmp_block );
            }
            if ( written2 < size2 )
            {
                readData ( ( void * ) ( &ptr2 ), sizeof ( data_ptr ), getPtr ( p2, written2 % PTR_BLK_SIZE ) );
                tuple_size2 = getVal ( tuple_buf2, p2, written2 % PTR_BLK_SIZE );
            }
        }
    }
    return first_block;
}

void SortedSequence::blocksSort ( int ptrsInLast )
{
    //Vector of blocks and number of containing elements
    typedef std::vector < xptr > t_blk_ptr_array;
    typedef std::vector < int > t_blk_size_array;
    t_blk_ptr_array *blocks = new t_blk_ptr_array, *old_blocks = new t_blk_ptr_array;
    t_blk_size_array *sizes = new t_blk_size_array, *old_sizes = new t_blk_size_array;
    t_blk_ptr_array *tmp_ptr;
    t_blk_size_array *tmp_size;

    for ( xptr it = firstPtrBlock; it != XNULL; it = nextBlock ( it ) )
    {
        blocks -> push_back ( it );
        sizes -> push_back ( PTR_BLK_SIZE );
    }

    sizes -> at ( sizes -> size() - 1 ) = ptrsInLast;

    while ( blocks -> size() > 1 )
    {
        tmp_ptr = blocks; blocks = old_blocks; old_blocks = tmp_ptr;
        tmp_size = sizes; sizes = old_sizes; old_sizes = tmp_size;

        for ( unsigned i = 1; i < old_blocks -> size(); i += 2 )
        {
            blocks -> push_back ( mergeBlocks ( old_blocks -> at ( i - 1 ), old_sizes -> at ( i - 1 ), old_blocks -> at ( i ), old_sizes -> at ( i ) ) );
            sizes -> push_back ( old_sizes -> at ( i - 1 ) + old_sizes -> at ( i ) );
        }
        if ( old_blocks -> size() % 2 == 1 )
        {
            blocks -> push_back ( old_blocks -> back() );
            sizes -> push_back ( old_sizes -> back() );
        }
        old_blocks -> clear(); old_sizes -> clear();
    }
    firstPtrBlock = blocks -> front();
    delete blocks; delete sizes;
    delete old_blocks; delete old_sizes;
}

void SortedSequence::makeNewChain ( int ptrsInLast )
{
    SSChain *new_chain = new SSChain ( this );

    void *tuple_buf = buf1;
    size_t tuple_size;

    xptr next_it;
    for ( xptr it = firstPtrBlock; it != XNULL; it = next_it )
    {
        next_it = nextBlock ( it );
        for ( unsigned i = 0; i < ( next_it == XNULL ? ptrsInLast : PTR_BLK_SIZE ); i++ )
        {
            tuple_size = getVal ( tuple_buf, it, i );
            new_chain -> add ( tuple_buf, tuple_size );
        }
    }
    new_chain -> setEnd();
    chains.push_back ( new_chain );
}

//*******************************************************************
//SSChain structure methods
//*******************************************************************

SortedSequence::SSChain::SSChain ( SortedSequence *_parent_ )
{
    parent = _parent_;

    value = new char[DATA_BLK_SIZE];
    firstBlock = parent -> getFreeBlock();
    place = firstBlock;
    toRead = 0;
}

SortedSequence::SSChain::~SSChain()
{
    if ( !initialized ) return;

    delete[] value;
    xptr it = firstBlock, tmp;
    do
    {
        tmp = it;
        it = parent -> nextBlock ( it );
        parent -> emptyBlocks.push_back ( tmp );
    }
    while ( it != XNULL );
}

void SortedSequence::SSChain::add ( void *buf, size_t tuple_size )
{
    place = parent -> writeData ( ( void * ) ( &tuple_size ), sizeof ( size_t ), place );
    place = parent -> writeData ( buf, tuple_size, place );
    toRead++;
}

void SortedSequence::SSChain::setEnd()
{
    place = firstBlock;
    next();
}

void SortedSequence::SSChain::next()
{
    place = parent -> readData ( ( void * ) ( &size ), sizeof ( size_t ), place );
    place = parent -> readData ( value, size, place );
    toRead--;
}

//*******************************************************************
//Working with heap
//*******************************************************************

void SortedSequence::buildHeapFromChains()
{
    heap.resize ( 1 );
    for ( unsigned int i = 0; i < chains.size(); i++ )
    {
        heap.push_back ( chains[i] );
    }
    heapsize = heap.size() - 1;
    for ( int i = ( heapsize == 0 ? 0 : heapsize / 2 ); i >= 1; i-- )
    {
        siftDown ( i );
    }
}

void SortedSequence::heapPush ( SSChain *chain )
{
    heap.push_back ( chain );
    heapsize++;
    siftUp ( heapsize );
}

SortedSequence::SSChain* SortedSequence::heapPop()
{
    SSChain *result = heap[1];
    heap[1] = heap[heapsize];
    heap.resize ( heapsize );
    heapsize--;
    siftDown ( 1 );
    return result;
}

void SortedSequence::siftUp ( int x )
{
    int pr = x / 2;
    while ( pr > 0 && serializer -> compare ( heap[x] -> value, heap[x] -> size, heap[pr] -> value, heap[pr] -> size ) < 0 )
    {
        SSChain *tmp = heap[x]; heap[x] = heap[pr]; heap[pr] = tmp;
        x = pr; pr = x / 2;
    }
}

void SortedSequence::siftDown ( int x )
{
    int left = x * 2, right = x * 2 + 1, min = x;
    if ( left <= heapsize && serializer -> compare ( heap[left] -> value, heap[left] -> size, heap[min] -> value, heap[min] -> size ) < 0 )
    {
        min = left;
    }
    if ( right <= heapsize && serializer -> compare ( heap[right] -> value, heap[right] -> size, heap[min] -> value, heap[min] -> size ) < 0 )
    {
        min = right;
    }
    if ( min != x )
    {
        SSChain *tmp = heap[x]; heap[x] = heap[min]; heap[min] = tmp;
        siftDown ( min );
    }
}

//*******************************************************************
//Working with block memory
//*******************************************************************

xptr SortedSequence::getFreeBlock()
{
    xptr block = XNULL;
    if ( !emptyBlocks.empty() )
    {
        block = BLOCKXPTR ( emptyBlocks.back() );
        emptyBlocks.pop_back();
    }
    else
    {
        vmm_alloc_tmp_block ( &block );
    }
    WRITEP ( block );
    BLK_HEADER ( block ) -> nblk = XNULL;
    return block + sizeof ( ss_blk_hdr );
}

xptr SortedSequence::addBlockToChain ( xptr p )
{
    xptr new_block = getFreeBlock();
    WRITEP ( p );
    BLK_HEADER ( p ) -> nblk = new_block;
    return new_block;
}

xptr SortedSequence::readData ( void *buf, size_t size, xptr place )
{
    size_t fspace = GET_FREE_SPACE ( place );
    if ( fspace <= size )
    {
        //We should read from two blocks
        CHECKP ( place );
        xptr next_block = BLK_HEADER ( place ) -> nblk;
        memcpy ( ( char * ) buf, XADDR ( place ), fspace );
        CHECKP ( next_block );
        memcpy ( ( ( char * ) buf + fspace ), XADDR ( next_block ), size - fspace );
        return next_block + size - fspace;
    }
    else
    {
        //From one block
        CHECKP ( place );
        memcpy ( ( char * ) buf, XADDR ( place ), size );
        return place + size;
    }
}

xptr SortedSequence::writeData ( void* buf, size_t size, xptr place )
{
    size_t fspace = GET_FREE_SPACE ( place );
    if ( fspace <= size )
    {
        //Data doesn't fit to given block
        WRITEP ( place );
        xptr new_block = addBlockToChain ( place );
        memcpy ( XADDR ( place ), ( char * ) buf, fspace );
        WRITEP ( new_block );
        memcpy ( XADDR ( new_block ), ( ( char * ) buf + fspace ), size - fspace );
        return new_block + size - fspace;
    }
    else
    {
        //Data fits to block
        WRITEP ( place );
        memcpy ( XADDR ( place ), ( char * ) buf, size );
        return place + size;
    }
}

xptr SortedSequence::nextBlock ( xptr p )
{
    CHECKP ( p );
    return BLK_HEADER ( p ) -> nblk;
}
