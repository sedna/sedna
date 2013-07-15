/*
 * File: xs_binary.h
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _XS_BINARY_H
#define _XS_BINARY_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


/////////////////////////////////////////////////////////////////////
/// This file defines interface fot casting to or from binary types.
/// For binary types we can perform only the following operations:
/////////////////////////////////////////////////////////////////////


tuple_cell cast_string_type_to_xs_base64Binary(const tuple_cell &c);
tuple_cell cast_string_type_to_xs_hexBinary(const tuple_cell &c);

tuple_cell cast_base64Binary_to_hexBinary(const tuple_cell &c);
tuple_cell cast_hexBinary_to_base64Binary(const tuple_cell &c);

// Returns length of the source binary data.
// May return actual length (length of the base64 encoded representation)
// through the optinal 'length' output parameter.
uint64_t xs_base64Binary_length(const tuple_cell *tc, uint64_t *length = NULL);


/////////////////////////////////////////////////////////////////////
/// Binary iterators. For example to execute something like this:
/// md5(doc('images')//image[@id eq 1] cast as xs:base64Binary)
/// This query will return md5 of the image itself, not md5 of
/// the base64 encoded string.
/////////////////////////////////////////////////////////////////////

template <class ByteIterator>
class Base64Iterator;

template <class ByteIterator>
inline bool operator < (const Base64Iterator<ByteIterator>& it1, const Base64Iterator<ByteIterator>& it2);

const unsigned char b64_char_to_value[123] = {'_','_','_','_','_','_','_','_','_','_',      //0x
                                              '_','_','_','_','_','_','_','_','_','_',      //1x
                                              '_','_','_','_','_','_','_','_','_','_',      //2x
                                              '_','_','_','_','_','_','_','_','_','_',      //3x
                                              '_','_','_', 62,'_','_','_', 63, 52, 53,      //4x
                                               54, 55, 56, 57, 58, 59, 60, 61,'_','_',      //5x
                                              '_', 0 ,'_','_','_', 0 , 1 , 2 , 3 , 4 ,      //6x
                                               5 , 6 , 7 , 8 , 9 , 10, 11, 12, 13, 14,      //7x
                                               15, 16, 17, 18, 19, 20, 21, 22, 23, 24,      //8x
                                               25,'_','_','_','_','_','_', 26, 27, 28,      //9x
                                               29, 30, 31, 32, 33, 34, 35, 36, 37, 38,      //10x
                                               39, 40, 41, 42, 43, 44, 45, 46, 47, 48,      //11x
                                               49, 50, 51};                                 //12x


template <class ByteIterator>
class Base64Iterator
{
protected:
    uint64_t pos;

public:
    virtual unsigned char operator *() const = 0;
    virtual Base64Iterator &operator ++() = 0;

    friend inline bool operator ==(const Base64Iterator<ByteIterator>& it1, const Base64Iterator<ByteIterator>& it2)
    {
        return it1.get_pos() == it2.get_pos();
    }
    friend inline bool operator !=(const Base64Iterator<ByteIterator>& it1, const Base64Iterator<ByteIterator>& it2)
    {
        return it1.get_pos() != it2.get_pos();
    }
    friend inline bool operator >(const Base64Iterator<ByteIterator>& it1, const Base64Iterator<ByteIterator>& it2)
    {
        return it1.get_pos() > it2.get_pos();
    }
    friend inline bool operator <(const Base64Iterator<ByteIterator>& it1, const Base64Iterator<ByteIterator>& it2)
    {
        return it1.get_pos() < it2.get_pos();
    }
    friend inline bool operator >=(const Base64Iterator<ByteIterator>& it1, const Base64Iterator<ByteIterator>& it2)
    {
        return it1.get_pos() >= it2.get_pos();
    }
    friend inline bool operator <=(const Base64Iterator<ByteIterator>& it1, const Base64Iterator<ByteIterator>& it2)
    {
        return it1.get_pos() <= it2.get_pos();
    }

protected:
    uint64_t get_pos() const
    {
        return pos;
    }
};



template <class ByteIterator>
class Base64ConstIterator: public Base64Iterator<ByteIterator>
{
private:
    const ByteIterator& end;

public:
    Base64ConstIterator(const ByteIterator& _end_, const uint64_t length): end(_end_)
    {
        this->pos = length;
    }

    virtual unsigned char operator *() const
    {
        throw USER_EXCEPTION2(SE1003, "Base64 const iterator: dereference is not implemented");
    }

    virtual Base64Iterator<ByteIterator> &operator ++()
    {
        throw USER_EXCEPTION2(SE1003, "Base64 const iterator: increment is not implemented");
    }
};


template <class ByteIterator>
class Base64ForwardIterator: public Base64Iterator<ByteIterator>
{
private:
    ByteIterator& start;
    const ByteIterator& end;

    unsigned char buf[4];

    inline void fill_buffer()
    {
        buf[0] = *(start);
        buf[1] = *(++start);
        buf[2] = *(++start);
        buf[3] = *(++start);
        ++start;
    }

public:
    Base64ForwardIterator(ByteIterator& _start_, const ByteIterator& _end_): start(_start_),
                                                                             end(_end_)
    {
        this->pos = 0;
        if (start > end) throw USER_EXCEPTION2(SE1003, "Base64 iterator: invalid underlying iterators (start > end)");

        if (start < end) {
            fill_buffer();
        }
    }

    virtual unsigned char operator *() const
    {
        switch (this->pos % 3) {
            case 0: return b64_char_to_value[buf[0]] << 2 | b64_char_to_value[buf[1]] >> 4;
            case 1: return (b64_char_to_value[buf[1]] & 0x0F) << 4 | b64_char_to_value[buf[2]] >> 2;
            case 2: return (b64_char_to_value[buf[2]] & 3) << 6 | b64_char_to_value[buf[3]];
            default: throw USER_EXCEPTION2(SE1003, "Base64 iterator: invalid iterator state");
        };
    }


    virtual Base64Iterator<ByteIterator> &operator ++()
    {
        this->pos += 1;

        if (this->pos % 3 == 0 && start < end) {
            fill_buffer();
        }

        return *this;
    }
};

#endif /* _XS_BINARY_H */
