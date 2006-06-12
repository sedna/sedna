/*
 * File:  bit_set.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



#include "sedna.h"
#include "bit_set.h"



bit_set::bit_set (int _size_): size(_size_),
							   external_memory(false)
{
   	if (size <= 0) 
		throw USER_EXCEPTION2(SE1003, "Size must be positive in bit_set::bit_set.");
	
	capacity = size / (sizeof(char) * 8);
	if (size % (sizeof(char) * 8) != 0) capacity++;
	bits = new char[capacity];
	initialize();
}

bit_set::bit_set (void* _bits_, int _size_): size(_size_),
											 external_memory(true),
											 bits((char*)_bits_)
{
	capacity = size / (sizeof(char) * 8);
	if (size % (sizeof(char) * 8) != 0) capacity++;	
}


bit_set::~bit_set ()
{
   	if(!external_memory)
   	{
		delete bits;
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
	int index = i / (sizeof(char) * 8);
	char mask = ((char) 1) << (i % (sizeof(char) * 8));
	if ((bits[index] & mask) == 0) 
		bits[index] |= mask;
}

void bit_set::clearAt (int i) 
{
	if (i < 0 || i >= size) 
		throw USER_EXCEPTION2(SE1003, "Incorrect index in bit_set::clearAt.");
	int index = i / (sizeof(char) * 8);
	char mask = ((char) 1) << (i % (sizeof(char) * 8));
	if ((bits[index] & mask) != 0) 
		bits[index] &= ~mask;
}

bool bit_set::testAt (int i) const 
{
	if (i < 0 || i >= size) 
		throw USER_EXCEPTION2(SE1003, "Incorrect index in bit_set::testAt.");
	int index = i / (sizeof(char) * 8);
	char mask = ((char) 1) << (i % (sizeof(char) * 8));
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
	return capacity * sizeof(char);	
}

void* bit_set::get_ptr_to_bytes () const
{
	return bits;
}

void bit_set::set_ptr_to_bytes (void* _bits_, int _size_)
{
	delete bits;
	bits = (char*)_bits_;
	size = _size_;
	external_memory = true;
	capacity = size / (sizeof(char) * 8);
	if (size % (sizeof(char) * 8) != 0) capacity++;
}


