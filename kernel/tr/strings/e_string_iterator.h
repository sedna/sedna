/*
 * File:  e_string_iterator.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _E_STRING_ITERATOR_H
#define _E_STRING_ITERATOR_H

#include <iterator>
#include <list>

#include "common/sedna.h"

#include "tr/vmm/vmm.h"
#include "common/xptr/xptr.h"		
#include "tr/strings/e_string.h"

class estr_iterator : public std::iterator<std::bidirectional_iterator_tag,char> 
{
private:
    friend bool operator ==(const estr_iterator& it1, const estr_iterator& it2);
    friend bool operator !=(const estr_iterator& it1, const estr_iterator& it2);
    friend bool operator > (const estr_iterator& it1, const estr_iterator& it2);
    friend bool operator >=(const estr_iterator& it1, const estr_iterator& it2);
    friend bool operator < (const estr_iterator& it1, const estr_iterator& it2);
    friend bool operator <=(const estr_iterator& it1, const estr_iterator& it2);
    	
	//xptr to the current block of the string being iterated over
	xptr cur_block_xptr;
	
	//xptr to the place where string begins in the current block
	char* cur_block_str_start_p;
	
	//ptr to the current position in the current block
	char* cur_p;

	//number of chars left to the end of the string
	strsize_t chars_left;

	//the string being iterated over
    xptr s;

public:	
	typedef str_off_t off_t;
    estr_iterator(str_off_t _chars_left_, xptr _s_);
	estr_iterator(): chars_left(-1) { }

	unsigned char operator*() const { CHECKP(cur_block_xptr); return *cur_p; }

    estr_iterator& operator ++();
    estr_iterator& operator --();

	//FIXME! (make void or throw exception)
	estr_iterator operator ++(int) {estr_iterator old(*this); ++(*this); return old; }
	estr_iterator operator --(int) {estr_iterator old(*this); --(*this); return old; }

	//FIXME! Remove while (x-- > 0)
	estr_iterator& operator +=(off_t x) { while (x-- > 0) ++(*this); return *this; }
	estr_iterator  operator +(off_t x) { 
		estr_iterator tmp(*this);
		tmp += x;
		return tmp;
	}
	estr_iterator& operator -=(off_t x) { while (x-- > 0) --(*this); return *this; }

	str_off_t operator -(const estr_iterator &it) const { return it.chars_left - chars_left; }
    
};

inline bool operator ==(const estr_iterator& it1, const estr_iterator& it2)
{
	return it1.chars_left == it2.chars_left;
}
inline bool operator !=(const estr_iterator& it1, const estr_iterator& it2)
{
	return it1.chars_left != it2.chars_left;
}
inline bool operator > (const estr_iterator& it1, const estr_iterator& it2)
{
	return it1.chars_left < it2.chars_left;
}
inline bool operator >=(const estr_iterator& it1, const estr_iterator& it2)
{
	return it1.chars_left <= it2.chars_left;
}
inline bool operator < (const estr_iterator& it1, const estr_iterator& it2)
{
	return it1.chars_left > it2.chars_left;
}
inline bool operator <=(const estr_iterator& it1, const estr_iterator& it2)
{
	return it1.chars_left >= it2.chars_left;
}

#endif

