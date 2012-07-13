/*
 * File:  bit_set.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BIT_SET_H
#define _BIT_SET_H

/*
 NOTE: Bitset has been proven to work _significally_ slower when not inlined.
 */

namespace sedna {

class Bitset {

#define OFFSET_PTR(P,OFS)               (void *)((char*)(P)+(OFS))
#define ROUND_SIZE_UP(SZ,K)             (((SZ)+(K)-1)&~((K)-1))
#define PTR_DISTANCE(B,A)               ((char*)(A)-(char*)(B))

private:
    int   size;             //size in bits
    int   capacity;         //size in bytes 
    unsigned char* bits;    //pointer to internal memory buffer
    bool  external_memory;  //'true' if external memory buffer is used

    void initialize() {
        for (int i = 0; i < capacity; i++)  bits[i] = 0;
    };

    Bitset(const Bitset&);
    void operator = (const Bitset&);
public:
    //_size_ is needed size in bits
    bit_set (int _size_) : size(_size_), external_memory(false)
    {
        U_ASSERT(size > 0);

        capacity = ROUND_SIZE_UP(_size_,8)/8;
        bits = se_new unsigned char[capacity];
        initialize();
    }

    ~Bitset() {
        if (!external_memory) {
            delete[] bits;
            bits = NULL;
        }
    };
    
    void setAt   (int i) {
        U_ASSERT(i >= 0 && i < size);

        int index = (i >> 3);
        unsigned char mask = (1 << (i & 0x07));
        bits[index] |= mask;
    };
    
    void clearAt (int i) {
        U_ASSERT(i >= 0 && i < size);

        int index = (i >> 3);
        unsigned char mask = (1 << (i & 0x07));
        bits[index] &= ~mask;
    };
    
    bool testAt  (int i) const {
        U_ASSERT(i >= 0 && i < size);
      
        int index = (i >> 3);
        unsigned char mask = (1 << (i & 0x07));
        return ((bits[index] & mask) != 0);
    };

    int getNextSetBitIdx(unsigned int startPos = 0) const {
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

    //set all bits to zero
    void clear() { initialize(); }

    //return size in bits
    int  get_size() const { return size; };

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
    void  set_ptr_to_bytes (void* _bits_, int _size_) {
        if(!external_memory) delete[] bits;
        bits = (unsigned char*)_bits_;
        size = _size_;
        external_memory = true;
        capacity = (size >> 0x03);
        if ((size & 0x07) != 0) capacity++;
    }


    bit_set (void* _bits_, int _size_)
      : size(_size_), bits((unsigned char*)_bits_), external_memory(true)
    {
        capacity = ROUND_SIZE_UP(_size_,8)/8;
    }

#undefine OFFSET_PTR
#undefine ROUND_SIZE_UP
#undefine PTR_DISTANCE

};

};

#endif /* _BIT_SET_H */
