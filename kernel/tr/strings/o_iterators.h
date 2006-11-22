/*
 * File:  o_iterators.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _O_ITERATORS_H
#define _O_ITERATORS_H

#include "sedna.h"

template <class uchar_o_iterator>
class utf8_o_iterator 
{
	uchar_o_iterator *it;
public:
	inline utf8_o_iterator(const utf8_o_iterator& x) : it(x.it)
	{
	}
	inline utf8_o_iterator& operator=(const utf8_o_iterator& x)
	{
		it=x.it;
		return *this;
	}
	inline utf8_o_iterator& operator=(const int t)
	{
		if (t < (1 << 7)) {
        	**it = t; ++*it;
        } else if (t < (1 << 11)) {
        	**it = ((t >> 6) | 0xc0); ++*it;
        	**it = ((t & 0x3f) | 0x80); ++*it;
        } else if (t < (1 << 16)) {
        	**it = ((t >> 12) | 0xe0); ++*it;
        	**it = (((t >> 6) & 0x3f) | 0x80); ++*it;
        	**it = ((t & 0x3f) | 0x80); ++*it;
        } else if (t < (1 << 21)) {
        	**it = ((t >> 18) | 0xf0);++*it;
        	**it = (((t >> 12) & 0x3f) | 0x80);++*it;
        	**it = (((t >> 6) & 0x3f) | 0x80);++*it;
        	**it = ((t & 0x3f) | 0x80); ++*it;
        }
		return *this;
	}
	inline utf8_o_iterator& operator*()
	{
		return *this;
	}
	inline utf8_o_iterator& operator++()
	{
		return *this;
	}
	inline utf8_o_iterator operator++(int)
	{
		return *this;
	}
	inline utf8_o_iterator(uchar_o_iterator &_it_) : it(&_it_) {}
};

#endif
