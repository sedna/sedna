/*
 * File:  e_string_o_iterator.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _E_STRING_O_ITERATOR_H
#define _E_STRING_O_ITERATOR_H

#include "sedna.h"

template<class T> class e_string_o_iterator
{
public:
	xptr pos;
	inline e_string_o_iterator(const e_string_o_iterator& x) 
	{
		pos=x.pos;	 
	}
	inline e_string_o_iterator& operator=(const e_string_o_iterator& x)
	{
		pos=x.pos;
		return *this;
	}
	e_string_o_iterator& operator=(const T& t);
	inline e_string_o_iterator& operator*()
	{
		return *this;
	}
	e_string_o_iterator& operator++();
	e_string_o_iterator operator++(int);
	e_string_o_iterator();

	

};
template<class T> e_string_o_iterator<T>& e_string_o_iterator<T>::operator=(const T& t)
{
	T tmp=t;
	CHECKP(pos);
	if (sizeof(T)>1)
	{
		xptr blk=BLOCKXPTR(pos);
		int lgth=(int)PAGE_SIZE- (pos-blk);
		if (lgth>=sizeof(T))
			*((T*)XADDR(pos))=tmp;
		else
		{
			memcpy(XADDR(pos),&tmp,lgth);
			xptr nblk=((e_str_blk_hdr*)XADDR(blk))->nblk;
			CHECKP(nblk);
			memcpy((char*)XADDR(nblk)+sizeof(e_str_blk_hdr),(char*)(&tmp)+lgth,sizeof(T)-lgth);
		}
	}
	else
		*((T*)XADDR(pos))=tmp;
	return *this;
}
template<class T> e_string_o_iterator<T>& e_string_o_iterator<T>::operator++()
{
	CHECKP(pos);
	xptr blk=BLOCKXPTR(pos);
	e_str_blk_hdr* blh=(e_str_blk_hdr*)(XADDR(blk));
	//1. incrementing pos
	pos+=sizeof(T);
	if (pos-blk>=PAGE_SIZE)
	{
		xptr  nblk=blh->nblk;
		if (nblk!=XNULL)
		{
			CHECKP(nblk);
			e_str_blk_hdr* bln=(e_str_blk_hdr*)(XADDR(nblk));
			pos=nblk+sizeof(e_str_blk_hdr)+((pos-blk)-PAGE_SIZE);
			blh=bln;
			blk=nblk;
		}
		else
		{
			xptr new_blk;
			vmm_alloc_tmp_block(&new_blk);
			e_str_blk_hdr::init(XADDR(new_blk));
			e_string_last_blk=new_blk;
			e_str_blk_hdr* bln=(e_str_blk_hdr*)(XADDR(new_blk));
			bln->cursor=sizeof(e_str_blk_hdr)+((pos-blk)-PAGE_SIZE);
			pos=new_blk+bln->cursor;
			CHECKP(blk);
			VMM_SIGNAL_MODIFICATION(blk);
			blh->nblk=new_blk;
			return *this;
		}
	}
	//2. shifting cursor
	if (blh->nblk==XNULL && (pos-blk)==blh->cursor)
	{
		VMM_SIGNAL_MODIFICATION(blk);
		blh->cursor+=sizeof(T);
		if (blh->cursor>=PAGE_SIZE)
		{
			xptr new_blk;
			vmm_alloc_tmp_block(&new_blk);
			e_str_blk_hdr::init(XADDR(new_blk));
			e_string_last_blk=new_blk;
			e_str_blk_hdr* bln=(e_str_blk_hdr*)(XADDR(new_blk));
			bln->cursor=sizeof(e_str_blk_hdr)+(blh->cursor-PAGE_SIZE);
			CHECKP(blk);
			VMM_SIGNAL_MODIFICATION(blk);
			blh->nblk=new_blk;
		}
	}
	return *this;
}
template<class T> e_string_o_iterator<T> e_string_o_iterator<T>::operator++(int)
{
	//XXX
	e_string_o_iterator y=*this;
	CHECKP(y.pos);
	return y;
}
template<class T> e_string_o_iterator<T>::e_string_o_iterator()
{
	//1.precondition:: e string blocks must be initialised
	/*
	if (e_string_last_blk == XNULL)
		init_e_string_blks()
		*/
	//1. initializing new pos
	CHECKP(e_string_last_blk);
	e_str_blk_hdr* bln=(e_str_blk_hdr*)(XADDR(e_string_last_blk));
	if (bln->cursor==PAGE_SIZE)
	{
		xptr new_blk;
		vmm_alloc_tmp_block(&new_blk);
		e_str_blk_hdr::init(XADDR(new_blk));
		xptr blk=e_string_last_blk;
		e_string_last_blk=new_blk;
		pos=new_blk+sizeof(e_str_blk_hdr);
		e_str_blk_hdr* blh=(e_str_blk_hdr*)(XADDR(new_blk));
		blh->cursor=sizeof(e_str_blk_hdr)+sizeof(T);
		CHECKP(blk);
		VMM_SIGNAL_MODIFICATION(blk);
		bln->nblk=new_blk;
		return;
	}
    pos=e_string_last_blk+bln->cursor;
	//2.shifting cursor
	bln->cursor+=sizeof(T);
    if (bln->cursor>=PAGE_SIZE)
	{
		xptr new_blk;
		vmm_alloc_tmp_block(&new_blk);
		e_str_blk_hdr::init(XADDR(new_blk));
		xptr blk=e_string_last_blk;
		e_string_last_blk=new_blk;
		e_str_blk_hdr* blh=(e_str_blk_hdr*)(XADDR(new_blk));
		blh->cursor=sizeof(e_str_blk_hdr)+bln->cursor-PAGE_SIZE;
		CHECKP(blk);
		VMM_SIGNAL_MODIFICATION(blk);
		bln->nblk=new_blk;
		return;
	}

}

template <class uchar_o_iterator>
class utf8_o_iterator 
{
	uchar_o_iterator it;
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
        	*it = t; ++it;
        } else if (t < (1 << 11)) {
        	*it = ((t >> 6) | 0xc0); ++it;
        	*it = ((t & 0x3f) | 0x80); ++it;
        } else if (t < (1 << 16)) {
        	*it = ((t >> 12) | 0xe0); ++it;
        	*it = (((t >> 6) & 0x3f) | 0x80); ++it;
        	*it = ((t & 0x3f) | 0x80); ++it;
        } else if (t < (1 << 21)) {
        	*it = ((t >> 18) | 0xf0);++it;
        	*it = (((t >> 12) & 0x3f) | 0x80);++it;
        	*it = (((t >> 6) & 0x3f) | 0x80);++it;
        	*it = ((t & 0x3f) | 0x80); ++it;
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
	inline utf8_o_iterator(uchar_o_iterator _it_) : it(_it_) {}
};

#endif
