/*
 * File:  e_string_iterator.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _E_STRING_ITERATOR_H
#define _E_STRING_ITERATOR_H

#include <iterator>
#include <list>

#include "sedna.h"

#include "vmm.h"
#include "xptr.h"		
#include "base.h"
#include "e_string.h"

class e_string_iterator : public std::iterator<std::bidirectional_iterator_tag,char> 
{
private:
    friend bool operator ==(const e_string_iterator& it1, const e_string_iterator& it2);
    friend bool operator !=(const e_string_iterator& it1, const e_string_iterator& it2);
    friend bool operator > (const e_string_iterator& it1, const e_string_iterator& it2);
    friend bool operator >=(const e_string_iterator& it1, const e_string_iterator& it2);
    friend bool operator < (const e_string_iterator& it1, const e_string_iterator& it2);
    friend bool operator <=(const e_string_iterator& it1, const e_string_iterator& it2);
    	
	//xptr to the current block of the string being iterated over
	xptr cur_block_xptr;
	
	//xptr to the place where string begins in the current block
	char* cur_block_str_start_p;
	
	//ptr to the current position in the current block
	char* cur_p;

	//number of chars left to the end of the string
	int chars_left;

	//the string being iterated over
    xptr s;

public:	
	e_string_iterator(int _chars_left_, xptr _s_);
	e_string_iterator(): chars_left(-1) { }


/*	e_string_iterator(const e_string_iterator& _esi_) { *this=_esi_; }
	e_string_iterator& operator =(const e_string_iterator&);

    ~e_string_iterator();*/

	unsigned char operator*() const { CHECKP(cur_block_xptr); return *cur_p; }

    e_string_iterator& operator ++();
    e_string_iterator& operator --();

	//FIXME! (make void or throw exception)
	e_string_iterator operator ++(int) {e_string_iterator old(*this); ++(*this); return old; }
	e_string_iterator operator --(int) {e_string_iterator old(*this); --(*this); return old; }

	//FIXME!
	e_string_iterator& operator +=(int x) { while (x-- > 0) ++(*this); return *this; }
	e_string_iterator  operator +(int x) { 
		e_string_iterator tmp(*this);
		tmp += x;
		return tmp;
	}
	e_string_iterator& operator -=(int x) { while (x-- > 0) --(*this); return *this; }

	int operator -(const e_string_iterator &it) const { return it.chars_left - chars_left; }
    
};

inline bool operator ==(const e_string_iterator& it1, const e_string_iterator& it2)
{
	return it1.chars_left == it2.chars_left;
}
inline bool operator !=(const e_string_iterator& it1, const e_string_iterator& it2)
{
	return it1.chars_left != it2.chars_left;
}
inline bool operator > (const e_string_iterator& it1, const e_string_iterator& it2)
{
	return it1.chars_left < it2.chars_left;
}
inline bool operator >=(const e_string_iterator& it1, const e_string_iterator& it2)
{
	return it1.chars_left <= it2.chars_left;
}
inline bool operator < (const e_string_iterator& it1, const e_string_iterator& it2)
{
	return it1.chars_left > it2.chars_left;
}
inline bool operator <=(const e_string_iterator& it1, const e_string_iterator& it2)
{
	return it1.chars_left >= it2.chars_left;
}

#endif

