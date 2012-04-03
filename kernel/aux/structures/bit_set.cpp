/*
 * File:  bit_set.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */
#include "common/sedna.h"
#include "common/bit_set.h"

#define OFFSET_PTR(P,OFS)		(void *)((char*)(P)+(OFS))
#define ROUND_SIZE_UP(SZ,K)		(((SZ)+(K)-1)&~((K)-1))
#define PTR_DISTANCE(B,A)		((char*)(A)-(char*)(B))

bit_set::bit_set (int _size_): size(_size_),
                               external_memory(false)
{
   	if (size <= 0) 
        throw USER_EXCEPTION2(SE1003, "Size must be positive in bit_set::bit_set.");
	
    capacity = ROUND_SIZE_UP(_size_,8)/8;
    bits = se_new unsigned char[capacity];
    initialize();
}

bit_set::bit_set (void* _bits_, int _size_): size(_size_),
                                             bits((unsigned char*)_bits_),
                                             external_memory(true)
{
    capacity = ROUND_SIZE_UP(_size_,8)/8;
}

int bit_set::getNextSetBitIdx(unsigned int startPos) const
{
/*	
//  The following code is not valid for some reasons:
//  1. on some platforms alignmet is required - unsigned *istart = (unsigned*) start will throw exception;
//  2. it seems that in first while we can break the boundary of 'bits';
//  3. BIG_ENDIAN_ORDER is not finished which is used on PPC platform ...

static const int bitsPerInt = sizeof(int)*8;
#ifdef BIG_ENDIAN_ORDER
	int base = (startPos & 0x07);              /// number of bit inside byte
#else
    int base = startPos % (bitsPerInt);        /// number of bit inside unsigned int
#endif
	void *start = OFFSET_PTR(bits, startPos/bitsPerInt*4);
	void *end = OFFSET_PTR(bits, capacity);
	unsigned *istart = (unsigned *)start;     
	unsigned *iend = (unsigned *)end;
	unsigned sample = 0;
	int res = -1, i = 0;

	if (istart<iend)
	{
#ifdef BIG_ENDIAN_ORDER
		sample = istart[0] & (((unsigned)-1)<<(bitsPerInt-(8-base))) | 0xFFFFFF;
#else
		sample = istart[0] & (((unsigned)-1)<<base);
#endif
		while (sample==0 && istart<iend)
		{
			sample = (++istart)[0];
		}

#ifdef BIG_ENDIAN_ORDER
		while (i++<bitsPerInt && 0==(sample & (1 << (base + (bitsPerInt-((i>>3)+1)*8)))));
		res = (int)PTR_DISTANCE(bits,istart)*bitsPerInt/4 + i;
#else
		while (i<bitsPerInt && 0==(sample & ~((~(unsigned)1)<<i))) ++i;
		res = (int)PTR_DISTANCE(bits,istart)*bitsPerInt/4 + i;
#endif
		
		if (res>=size) res=-1;
	}
	return res;*/
    
    int i = startPos;
    while(i < size)
    {
        int index = (i >> 3);
        unsigned char mask = (1 << (i & 0x07));
        if (0 != (bits[index] & mask)) return i;
        i++;
    }
    return -1;
}

bit_set::~bit_set ()
{
    if(!external_memory)
    {
        delete[] bits;
        bits = NULL;
    }
}

void bit_set::initialize () 
{
    for (int i = 0; i < capacity; i++)	bits[i] = 0;
}

void bit_set::setAt (int i) 
{
    if (i < 0 || i >= size) 
	    throw USER_EXCEPTION2(SE1003, "Incorrect index in bit_set::setAt.");
    int index = (i >> 3);
    unsigned char mask = (1 << (i & 0x07));
    bits[index] |= mask;
}

void bit_set::clearAt (int i) 
{
    if (i < 0 || i >= size) 
        throw USER_EXCEPTION2(SE1003, "Incorrect index in bit_set::clearAt.");
    int index = (i >> 3);
    unsigned char mask = (1 << (i & 0x07));
    bits[index] &= ~mask;
}

bool bit_set::testAt (int i) const 
{
    if (i < 0 || i >= size) 
        throw USER_EXCEPTION2(SE1003, "Incorrect index in bit_set::testAt.");
    int index = (i >> 3);
    unsigned char mask = (1 << (i & 0x07));
    return ((bits[index] & mask) != 0);
}

void bit_set::clear ()
{
    initialize();
}

int bit_set::get_size () const 
{ 
    return size; 
}

int bit_set::get_size_in_bytes () const
{
    return capacity;	
}

void* bit_set::get_ptr_to_bytes () const
{
    return bits;
}

void bit_set::set_ptr_to_bytes (void* _bits_, int _size_)
{
    if(!external_memory) delete[] bits;
    bits = (unsigned char*)_bits_;
    size = _size_;
    external_memory = true;
    capacity = (size >> 0x03);
    if ((size & 0x07) != 0) capacity++;
}


