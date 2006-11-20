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

template <class BaseIterator, typename UChar32 = int>
class utf8_iterator : public std::iterator<std::bidirectional_iterator_tag, UChar32> 
{
private:
//    friend bool operator ==(const utf8_iterator& it1, const utf8_iterator& it2);
//    friend bool operator !=(const utf8_iterator& it1, const utf8_iterator& it2);
    friend bool operator > (const utf8_iterator& it1, const utf8_iterator& it2);
    friend bool operator >=(const utf8_iterator& it1, const utf8_iterator& it2);
    friend bool operator < (const utf8_iterator& it1, const utf8_iterator& it2);
    friend bool operator <=(const utf8_iterator& it1, const utf8_iterator& it2);
	
	typename std::iterator<std::bidirectional_iterator_tag, UChar32>::value_type cur_c;
	bool has_c;
	BaseIterator it;
public:	

    utf8_iterator() : it() {}
	utf8_iterator(BaseIterator _it_) : it(_it_) {}
	utf8_iterator(const utf8_iterator &_i_) { *this = _i_; }
    ~utf8_iterator() {}

	inline BaseIterator base_iterator()
	{
		return it;
	}

    inline typename std::iterator<std::bidirectional_iterator_tag, UChar32>::value_type operator*()
	{
		//TODO!
		unsigned char ch=*(it);
		typename std::iterator<std::bidirectional_iterator_tag, UChar32>::value_type r;
	
		if (ch < 128)
		{
				r = ch;
		}
		else if (ch < 224) //FIXME: ch mustbe >= 192
		{
			r = ch - 192; r <<= 6;
			BaseIterator tmp(it);
			++(tmp); ch = *(tmp); r += ch - 128;
		}
		else if (ch < 240)
		{
			r = ch - 224; r <<= 6;
			BaseIterator tmp(it);
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128;
		}
		else if (ch < 248)
		{
			r = ch - 240; r <<= 6;
			BaseIterator tmp(it);
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128;
		}
		else if (ch < 252)
		{
			r = ch - 248; r <<= 6;
			BaseIterator tmp(it);
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128;
		}
		else // ch mustbe < 254
		{
			r = ch - 252; r <<= 6;
			BaseIterator tmp(it);
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128;
		}
		return r;
	}

    utf8_iterator& operator ++()
	{
		unsigned char ch=*(it);

		++(it);
		if (ch >= 192)
		{
			++(it);
			if (ch >= 224)
			{
				++(it);
				if (ch >= 240)
				{
					++(it);
					if (ch >= 248)
					{
						++(it);
						if (ch >= 252)
							++(it);
					}
				}
			}
		}
		return *this;
	}
    
    utf8_iterator& operator --()
	{
		--(it);
		unsigned char ch=*(it);
		while (ch >= 128 && ch < 192)
		{
			--(it);
			ch=*(it);
		}
		return *this;
	}

	utf8_iterator& operator =(const utf8_iterator& _i_)
	{
		this->it = _i_.it;
		return (*this);
	}


	inline bool operator ==(const utf8_iterator& it2) const
	{
		return (*this).it == it2.it;
	}
	inline bool operator !=(const utf8_iterator& it2) const
	{
		return (*this).it != it2.it;
	}
};

//TODO - move into class def.
template <class BaseIterator>
inline bool operator > (const utf8_iterator<BaseIterator>& it1, const utf8_iterator<BaseIterator>& it2)
{
	return it1.it > it2.it;
}
template <class BaseIterator>
inline bool operator >=(const utf8_iterator<BaseIterator>& it1, const utf8_iterator<BaseIterator>& it2)
{
	return it1.it >= it2.it;
}
template <class BaseIterator>
inline bool operator < (const utf8_iterator<BaseIterator>& it1, const utf8_iterator<BaseIterator>& it2)
{
	return it1.it < it2.it;
}
template <class BaseIterator>
inline bool operator <=(const utf8_iterator<BaseIterator>& it1, const utf8_iterator<BaseIterator>& it2)
{
	return it1.it <= it2.it;
}

#endif

