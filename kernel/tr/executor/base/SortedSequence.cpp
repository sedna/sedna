#include "tr/executor/base/SortedSequence.h"
#include <ctime>

//Initialization and deinitialization

void SortedSequence::init()
{
    srand(std::time(NULL));

    finalized = false;

    init_accumulator();

     //Initializating buffers;
    buf1 = new char[DATA_BLK_SIZE];
    buf2 = new char[DATA_BLK_SIZE];
}

void SortedSequence::free()
{
    free_accumulator();
    //Clearing heap
    while (!heap.empty())
    {
	heap.pop();
    }
    //Destruction of chains and block memory data
    chains.clear();
    for (unsigned int i = 0; i < empty_blocks.size(); i++)
    {
	vmm_delete_block(empty_blocks[i]);
    }
    empty_blocks.clear();
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

    //1. Finding place in accumulator to store new tuple
    if (el_cnt % PTR_BLK_SIZE == 0 && el_cnt != 0)
    {
	//if pointers block is full
	ptr_place = add_block_to_chain(ptr_place);
	blk_cnt++;
    }

    el_cnt++;

    //2. Serializing data to this place
    void *tuple_buf = buf1; //Buffer for serialization
    size_t size = serializer -> serialize(t, tuple_buf);

    //2.a Writing pointer
    WRITEP(ptr_place);
    data_ptr *ptr = (data_ptr *)XADDR(ptr_place);
    ptr -> value = val_place;
    ptr -> size = size;
    ptr_place += sizeof(data_ptr);

    //2.b Writing value
    xptr tmp_ptr = BLOCKXPTR(val_place);
    val_place = writeData(tuple_buf, size, val_place);
    if (tmp_ptr != BLOCKXPTR(val_place))
    {
	blk_cnt++;
    }

    //3. Checking if accumulator is full
    if (blk_cnt > MAX_BLOCKS_IN_CHAIN) {
	finalize_accumulator();
	reinit_accumulator();
    }
}

void SortedSequence::sort()
{
    if (finalized)
    {
	return;
    }
    finalized = true;
    finalize_accumulator();
    for (unsigned int i = 0; i < chains.size(); i++)
    {
	heap.push(SSChainPtr(&(chains[i])));
    }
}

void SortedSequence::next(tuple &t)
{
    if (heap.empty())
    {
	t.set_eos();
	return;
    }
    else
    {
	//Extracting maximum from heap
	SSChain *best = heap.top().ptr;
	heap.pop();
	serializer -> deserialize(t, best -> value, best -> size);
	if (!best -> empty())
	{
	    best -> next();
	    //Putting chain back to heap
	    heap.push(SSChainPtr(best));
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

void SortedSequence::init_accumulator()
{
    first_ptr_block = get_free_block();
    first_val_block =  get_free_block();
    ptr_place = first_ptr_block + sizeof(seq_blk_hdr);
    val_place = first_val_block + sizeof(seq_blk_hdr);
    blk_cnt = 2, el_cnt = 0;
}

void SortedSequence::reinit_accumulator()
{
    free_accumulator();
    init_accumulator();
}

void SortedSequence::free_accumulator()
{
    xptr it = first_ptr_block, tmp;
    do {
	tmp = it;
	it = next_block(it);
	empty_blocks.push_back(tmp);
    } while (it != XNULL);
    first_ptr_block = XNULL;

    it = first_val_block;
    do {
	tmp = it;
	it = next_block(it);
	empty_blocks.push_back(tmp);
    } while (it != XNULL);
    first_val_block = XNULL;
}

void SortedSequence::finalize_accumulator()
{
    //In-block sorting of accumulator (qsort)
    xptr tmp_ptr, tmp_ptr_next;
    tmp_ptr = first_ptr_block;
    while (tmp_ptr != XNULL)
    {
	tmp_ptr_next = next_block(tmp_ptr);
	if (tmp_ptr_next != XNULL)
	{
	    in_block_sort(tmp_ptr, 0, PTR_BLK_SIZE - 1);
	}
	else
	{
	    in_block_sort(tmp_ptr, 0, el_cnt % PTR_BLK_SIZE - 1);
	}
	tmp_ptr = tmp_ptr_next;
    }
    //Merging of sorted blocks
    blocks_sort(el_cnt % PTR_BLK_SIZE);
    make_new_chain(el_cnt % PTR_BLK_SIZE);
}

xptr SortedSequence::get_ptr(xptr &block, int ind)
{
    return block + sizeof(seq_blk_hdr) + ind * sizeof(data_ptr);
}

size_t SortedSequence::get_val(void *buf, xptr &block, int ind)
{
    xptr tmp_ptr = block + sizeof(seq_blk_hdr) + ind * sizeof(data_ptr);
    size_t size = ((data_ptr *)XADDR(tmp_ptr)) -> size;
    readData(buf, size, ((data_ptr *)XADDR(tmp_ptr)) -> value);
    return size;
}

void SortedSequence::swap_pointers(xptr p1, xptr p2)
{
    data_ptr *buf1 = new data_ptr;
    data_ptr *buf2 = new data_ptr;

    readData(buf1, sizeof(data_ptr), p1);
    readData(buf2, sizeof(data_ptr), p2);

    writeData(buf1, sizeof(data_ptr), p2);
    writeData(buf2, sizeof(data_ptr), p1);

    delete buf1;
    delete buf2;
}

void SortedSequence::in_block_sort(xptr& p, int left, int right)
{
    if (right - left + 1 <= 7)
    {
	//Insertion sort in small cases
	void *tuple_buf1 = buf1;
	void *tuple_buf2 = buf2;
	size_t tuple_size1, tuple_size2;
	for (int i = left + 1; i <= right; i++)
	{
	    tuple_size1 = get_val(tuple_buf1, p, i);
	    int j = i - 1;
	    do {
		tuple_size2 = get_val(tuple_buf2, p, j);
		if (serializer -> compare(tuple_buf1, tuple_size1, tuple_buf2, tuple_size2) < 0)
		{
		    swap_pointers(get_ptr(p,j), get_ptr(p, j + 1));
		}
		else
		{
		    break;
		}
		j--;
	    } while (j >= 0);
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
	pivot_size = get_val(pivot, p, left + rand() % (right - left + 1));

	while (i < j)
	{
	    tuple_size = get_val(tuple_buf, p, i);
	    while (serializer -> compare(tuple_buf, tuple_size, pivot, pivot_size) < 0)
	    {
		i++;
		tuple_size = get_val(tuple_buf, p, i);
	    }

	    tuple_size = get_val(tuple_buf, p, j);
	    while (serializer -> compare(tuple_buf, tuple_size, pivot, pivot_size) > 0)
	    {
		j--;
		tuple_size = get_val(tuple_buf, p, j);
	    }

	    if (i <= j)
	    {
		swap_pointers(get_ptr(p, i), get_ptr(p, j));
		i++, j--;
	    }
	}

	if (i < right) in_block_sort(p, i, right);
	if (left < j) in_block_sort(p, left, j);
    }
}

xptr SortedSequence::merge_blocks(xptr &p1, int size1, xptr &p2, int size2)
{
    //Pointers to new sequence:
    xptr first_block = get_free_block();
    xptr ptr_place = first_block + sizeof(seq_blk_hdr);

    //Buffers for merging:
    void *tuple_buf1 = buf1;
    void *tuple_buf2 = buf2;
    size_t tuple_size1, tuple_size2;

    tuple_size1 = get_val(tuple_buf1, p1, 0);
    tuple_size2 = get_val(tuple_buf2, p2, 0);

    //Counters of already written elements:
    int written1 = 0, written2 = 0;

    while (written1 + written2 < size1 + size2)
    {
	if (written2 == size2 || (written1 < size1 && serializer -> compare(tuple_buf1, tuple_size1, tuple_buf2, tuple_size2) < 0))
	{
	    //Element in first sequence less than element in second
	    ptr_place = writeData(tuple_buf1, tuple_size1, ptr_place);
	    written1++;

	    if (written1 % PTR_BLK_SIZE == 0 || size1 == written1)
	    {
		//If we wrote a whole block or all elements we should free it
		xptr tmp_block = p1;
		p1 = next_block(tmp_block);
		if (p1 != XNULL)
		{
		    //If we have more blocks in sequence
		    p1 += sizeof(seq_blk_hdr);
		}
		empty_blocks.push_back(BLOCKXPTR(tmp_block));
	    }
	    if (written1 < size1)
	    {
		tuple_size1 = get_val(tuple_buf1, p1, written1 % PTR_BLK_SIZE);
	    }
	}
	else
	{
	    //Otherwise
	    ptr_place = writeData(tuple_buf2, tuple_size2, ptr_place);
	    written2++;

	    if (written2 % PTR_BLK_SIZE == 0 || size2 == written2)
	    {
		//If we wrote a whole block or all elements we should free it
		xptr tmp_block = p2;
		p2 = next_block(tmp_block);
		if (p2 != XNULL)
		{
		    //If we have more blocks in sequence
		    p2 += sizeof(seq_blk_hdr);
		}
		empty_blocks.push_back(BLOCKXPTR(tmp_block));
	    }
	    if (written2 < size2)
	    {
		tuple_size2 = get_val(tuple_buf2, p2, written2 % PTR_BLK_SIZE);
	    }
	}
    }
    return first_block;
}

void SortedSequence::blocks_sort(int ptrs_in_last)
{
    //Vector of blocks and number of containing elements
    typedef std::vector < xptr > t_blk_ptr_array;
    typedef std::vector < int > t_blk_size_array;
    t_blk_ptr_array blocks, old_blocks;
    t_blk_size_array sizes, old_sizes;
    for (xptr it = first_ptr_block; it != XNULL; it = next_block(it))
    {
	blocks.push_back(it);
	sizes.push_back(PTR_BLK_SIZE);
    }

    sizes[sizes.size() - 1] = ptrs_in_last;

    while (blocks.size() > 1)
    {
	old_blocks = blocks, old_sizes = sizes;
	for (unsigned i = 1; i < old_blocks.size(); i += 2)
	{
	    blocks.push_back(merge_blocks(old_blocks[i - 1], old_sizes[i - 1], old_blocks[i], old_sizes[i]));
	    sizes.push_back(old_sizes[i - 1] + old_sizes[i]);
	}
	if (old_blocks.size() % 2)
	{
	    blocks.push_back(old_blocks.back());
	    sizes.push_back(old_sizes.back());
	}
    }
    first_ptr_block = blocks.front();
}

void SortedSequence::make_new_chain(int ptrs_in_last)
{
    chains.push_back(SSChain());
    SSChain &new_chain = chains.back();
    new_chain.init(this);

    void *tuple_buf = buf1;
    size_t tuple_size;

    xptr next_it;
    for (xptr it = first_ptr_block; it != XNULL; it = next_it)
    {
	next_it = next_block(it);
	for (unsigned i = 0; i < (next_it == XNULL ? ptrs_in_last : PTR_BLK_SIZE); i++)
	{
	    tuple_size = get_val(tuple_buf, it, i);
	    new_chain.add(tuple_buf, tuple_size);
	}
    }
    new_chain.set_end();
}

//*******************************************************************
//SSChain structure methods
//*******************************************************************

void SortedSequence::SSChain::init(SortedSequence *_parent_)
{
    initialized =  true;

    parent = _parent_;

    value = new char[DATA_BLK_SIZE];
    first_block = parent -> get_free_block();
    place = first_block + sizeof(seq_blk_hdr);
    to_read = 0;
}

SortedSequence::SSChain::~SSChain()
{
    if (!initialized) return;

    delete[] value;
    xptr it = first_block, tmp;
    do {
	tmp = it;
	it = parent -> next_block(it);
	parent -> empty_blocks.push_back(tmp);
    } while (it != XNULL);
}

void SortedSequence::SSChain::add(void *buf, size_t tuple_size)
{
    place = parent -> writeData((void *)(&tuple_size), sizeof(size_t), place);
    place = parent -> writeData(buf, tuple_size, place);
    to_read++;
}

void SortedSequence::SSChain::set_end()
{
    place = first_block + sizeof(seq_blk_hdr);
    next();
}

void SortedSequence::SSChain::next()
{
    place = parent -> readData((void *)(&size), sizeof(size_t), place);
    place = parent -> readData(value, size, place);
    to_read--;
}

//*******************************************************************
//Working with block memory
//*******************************************************************

xptr SortedSequence::get_free_block()
{
    xptr block = XNULL;
    if (!empty_blocks.empty())
    {
	block = empty_blocks.back();
	empty_blocks.pop_back();
    }
    else
    {
	vmm_alloc_tmp_block(&block);
    }
    WRITEP(block);
    ((seq_blk_hdr *)XADDR(BLOCKXPTR(block))) -> nblk = XNULL;
    return block;
}

xptr SortedSequence::add_block_to_chain(const xptr& p)
{
    xptr new_block = get_free_block();
    WRITEP(p);
    ((seq_blk_hdr *)XADDR(BLOCKXPTR(p))) -> nblk = new_block;
    new_block += sizeof(seq_blk_hdr);
    return new_block;
}

xptr SortedSequence::readData(void *buf, size_t size, const xptr &place)
{
    CHECKP(place);
    size_t fspace = GET_FREE_SPACE(place);
    if (fspace < size)
    {
	//We should read from two blocks
	xptr next_block = ((seq_blk_hdr *)XADDR(BLOCKXPTR(place))) -> nblk + sizeof(seq_blk_hdr);
	CHECKP(place);
	memcpy((char *)buf, XADDR(place), fspace);
	CHECKP(next_block);
	memcpy(((char *)buf + fspace), XADDR(next_block), size - fspace);
	return next_block + size - fspace;
    }
    else
    {
	//From one block
	CHECKP(place);
	memcpy((char *)buf, XADDR(place), size);
	return place + size;
    }
}

xptr SortedSequence::writeData(void* buf, size_t size, const xptr &place)
{
    CHECKP(place);
    size_t fspace = GET_FREE_SPACE(place);
    if (fspace < size)
    {
	//Data doesn't fit to given block
	xptr new_block = add_block_to_chain(place);
	WRITEP(place);
	memcpy(XADDR(place), (char *)buf, fspace);
	WRITEP(new_block);
	memcpy(XADDR(new_block), ((char *)buf + fspace), size - fspace);
	return new_block + size - fspace;
    }
    else
    {
	//Data fits to block
	WRITEP(place);
	memcpy(XADDR(place), (char *)buf, size);
	return place + size;
    }
}

xptr SortedSequence::next_block(const xptr &p)
{
    CHECKP(p);
    return ((seq_blk_hdr *)XADDR(BLOCKXPTR(p))) -> nblk;
}
