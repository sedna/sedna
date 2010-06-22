/*
 * File:  char_iterator.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CHAR_ITERATOR_H
#define _CHAR_ITERATOR_H

#include <iterator>
#include <list>

#include "common/sedna.h"

//#define ITERATOR_CHECK_BOUNDS

class char_iterator : public std::iterator<std::bidirectional_iterator_tag,unsigned char> 
    {
    private:
    	
        //friend class tuple_cell;
        inline friend bool operator ==(const char_iterator& it1, const char_iterator& it2)
		{
			return (it1.begin==it2.begin)&&(it1.size==it2.size)&&(it1.shift==it2.shift);
		}
        inline friend bool operator !=(const char_iterator& it1, const char_iterator& it2)
		{
			return !(it1==it2);
		}
        inline friend bool operator > (const char_iterator& it1, const char_iterator& it2)
		{
			return ((uintptr_t)(it1.begin+it1.shift)>(uintptr_t)(it2.begin+it2.shift));
		}
        inline friend bool operator >=(const char_iterator& it1, const char_iterator& it2)
		{
			return ((uintptr_t)(it1.begin+it1.shift)>=(uintptr_t)(it2.begin+it2.shift));
		}
        inline friend bool operator < (const char_iterator& it1, const char_iterator& it2)
		{
			return ((uintptr_t)(it1.begin+it1.shift)<(uintptr_t)(it2.begin+it2.shift));
		}
        inline friend bool operator <=(const char_iterator& it1, const char_iterator& it2)
		{
			return ((uintptr_t)(it1.begin+it1.shift)<=(uintptr_t)(it2.begin+it2.shift));
		}
		//number of chars left to the end of the string
		unsigned char* begin;
		int size;
		ptrdiff_t shift;
    public:
        typedef int off_t;
		char_iterator(unsigned char* _begin_, int _size_,ptrdiff_t _shift_) :
			begin(_begin_) ,size(_size_) ,shift(_shift_)
       	{

		}
        char_iterator() : begin(NULL) ,size(0) ,shift(0) {}

        unsigned char operator*() { return *(begin+shift); }

        //char operator[](int i) { return s->get(i); }
		inline char_iterator& operator ++() {
			shift++;
#ifdef ITERATOR_CHECK_BOUNDS
			if (shift > size)
				throw USER_EXCEPTION2(SE1003, "char_iterator run out of the string end");
#endif
			return *this;
		}
        inline char_iterator  operator ++(int) { 
			shift++; 
#ifdef ITERATOR_CHECK_BOUNDS
			if (shift > size)
				throw USER_EXCEPTION2(SE1003, "char_iterator run out of the string end");
#endif
			return char_iterator(begin,size,shift-1); 
		}
		inline char_iterator& operator --(){
#ifdef ITERATOR_CHECK_BOUNDS
			if (shift <= 0)
				throw USER_EXCEPTION2(SE1003, "char_iterator run out of the string begining");
#endif
			shift--;return *this;
		}
        inline char_iterator  operator --(int) { 
#ifdef ITERATOR_CHECK_BOUNDS
			if (shift <= 0)
				throw USER_EXCEPTION2(SE1003, "char_iterator run out of the string begining");
#endif
			shift--; return char_iterator(begin,size,shift+1);
		}
		inline ptrdiff_t operator -(const char_iterator it) const { return begin+shift-(it.begin+it.shift);}
		inline char_iterator& operator +=(ptrdiff_t x) { 
			shift += x; 
#ifdef ITERATOR_CHECK_BOUNDS
			if (shift > size)
				throw USER_EXCEPTION2(SE1003, "char_iterator run out of the string end");
#endif
			return *this; 
		} 
		inline char_iterator& operator -=(ptrdiff_t x) {
			shift -= x;
#ifdef ITERATOR_CHECK_BOUNDS
			if (shift < 0)
				throw USER_EXCEPTION2(SE1003, "char_iterator run out of the string begining");
#endif
			return *this;
		} 
    };
#endif
