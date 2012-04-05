/*
 * File:  bit_set.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BIT_SET_H
#define _BIT_SET_H

class bit_set {

private:

    int   size;             //size in bits
    int   capacity;         //size in bytes 
    unsigned char* bits;    //pointer to internal memory buffer
    bool  external_memory;  //'true' if external memory buffer is used

	void initialize();

	bit_set(const bit_set&);
	void operator = (const bit_set&);

public:

    bit_set (int _size_);   //_size_ is needed size in bits

    ~bit_set();
    
    void setAt   (int i);
    void clearAt (int i);
    bool testAt  (int i) const;
    int getNextSetBitIdx(unsigned int startPos = 0) const;

    void clear();           //set all bits to zero
    int  get_size() const;  //return size in bits

    /////////////////////////////////////////////////////////////
    /// Accessors for serialization/deserialization processes.///
    ///            Some of them are DANGEROUS!                ///
    /////////////////////////////////////////////////////////////

    int   get_size_in_bytes() const;							
    void* get_ptr_to_bytes () const;

    /////////////////////////////////////////////////////////////
    /// You can create bit_set over external memory. Note, however,
    /// that size must conform to the capacity of the given memory! 
    /// Note, also, that external memory is NOT released!
    void  set_ptr_to_bytes (void* _bits_, int _size_);
    bit_set (void* _bits_, int _size_);
    /////////////////////////////////////////////////////////////

};

#endif
