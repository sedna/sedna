/*
 * File:  pstr_long.h
 * Copyright (C) 2005 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PSTR_LONG_H
#define _PSTR_LONG_H

#include "common/sedna.h"
#include <sstream>
#include "common/sm_vmm_data.h"
#include "tr/strings/strings_base.h"
#include "tr/strings/text_data.h"

/* functions with '2' in the end take pointers to strings instead of pointers to string descriptors
 */

//typedef int pstr_long_off_t;
typedef int64_t pstr_long_off_t;
typedef int pstr_long_map_size_t; //type for storing char count in blb map

struct pstr_long_blk_hdr
{
	vmm_sm_blk_hdr sm_vmm;	// sm/vmm parameters
	xptr	next_blk;
	xptr	prev_blk;
};
#define PSTR_LONG_BLK_HDR_SIZE sizeof(struct pstr_long_blk_hdr)
#define PSTR_LONG_BLK_HDR(blk) ( (struct pstr_long_blk_hdr *)(XADDR(blk)))

struct pstr_long_last_blk_ftr
{
	xptr			start;					//xptr of string start (in the 1st blk)
	int				first_blk_char_count;	//
	int				char_count;				//valid only if cursor > 0
	short			block_list_map_size;	//
	short			block_list_size;		//size of block list in this block
	int				cursor;					//byte offset of the end of the string in this block,
											//if negative, there is no string data is this block,
											//  and absolute value is byte offset of the end of the string in prev block
											//  cursor < 0  ==>  block_list_size > 0, TODO: remove this line (or change it by smth like adding block_list_map_size > 0 cond here)
	short			first_blb_gap_size;		//size of the gap in the first 'block list' block (0, if block_list_map_size == 0)
	short			pred_blb_size;			//size of block list in the last 'block list' block (PSTR_LONG_FULL_BLOCK_LIST_SIZE, if block_list_map_size == 0)
};
#define PSTR_LONG_LAST_BLK_FTR_SIZE sizeof(struct pstr_long_last_blk_ftr)
#define PSTR_LONG_LAST_BLK_FTR(blk) ( (struct pstr_long_last_blk_ftr *)((char*)XADDR(blk) + PAGE_SIZE - PSTR_LONG_LAST_BLK_FTR_SIZE) )

struct pstr_long_block_list_entry
{
	xptr			str_blk;			//long string block
	int				char_count;			//number of chars that start in this block
};

#define PSTR_LONG_BLOCK_LIST_ENTRY_SIZE sizeof(struct pstr_long_block_list_entry)

#define PSTR_LONG_BLOCK_LIST_HDR_SIZE sizeof(struct vmm_sm_blk_hdr)
const int PSTR_LONG_FULL_BLOCK_LIST_SIZE = ((PAGE_SIZE - PSTR_LONG_BLOCK_LIST_HDR_SIZE)/PSTR_LONG_BLOCK_LIST_ENTRY_SIZE);

struct pstr_long_block_list_map_entry
{
	xptr					list_blk;
	pstr_long_map_size_t	char_count;
};
#define PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE sizeof(struct pstr_long_block_list_map_entry)

xptr pstr_long_create_str(xptr desc, const text_source_t src);
xptr pstr_long_create_str2(bool persistent, const text_source_t src);
void pstr_long_delete_str(xptr desc);
void pstr_long_delete_str2(const xptr str_ptr);
void pstr_long_append_tail(const xptr desc, const text_source_t src);
void pstr_long_append_tail(const xptr dst_desc, const xptr src_desc);
void pstr_long_truncate(xptr desc, pstr_long_off_t size);

void pstr_long_append_head(xptr desc, const text_source_t src);
void pstr_long_delete_head(xptr desc, pstr_long_off_t size);

void pstr_long_write(xptr desc,se_ostream& crmout);
void pstr_long_feed(xptr desc,	string_consumer_fn fn, void *p);
void pstr_long_feed2(xptr str,	string_consumer_fn fn, void *p);
inline void pstr_long_writextext(xptr desc, se_ostream& crmout)
{
    pstr_long_feed(desc, writextext_cb, &crmout);
}


void pstr_long_copy_to_buffer2(char *buf, const xptr &str_ptr, pstr_long_off_t size); //FIXME: must str_ptr be passed by ref here?
void pstr_long_copy_to_buffer(char *buf, xptr desc);


pstr_long_off_t pstr_long_length(const xptr data);
pstr_long_off_t pstr_long_bytelength2(const xptr data);

#ifdef PSTR_LONG_TEST
void pstr_long_str_info(xptr desc);
void pstr_long_test_bytelength2();
#endif

//need to declare it here, because it's a friend of pstr_long_cursor
class pstr_long_cursor : public str_cursor
{
	//TODO!! blk should point to last_blk or last_blk->pred when pointer is at eof
	// if cursor < 0, then blk should NEVER point to last_blk in get_blk/copy_blk
	// i.e. blk always points to block that contains some string data & if position
	// is not eos, current char is in blk
protected:
	xptr m_start;
	pstr_long_cursor() {} //dummy default constructor for pstr_long_iterator
public:
	pstr_long_cursor(const xptr &_ptr_) {
		last_blk = _ptr_;
		CHECKP(last_blk);
		const xptr _start_ = (PSTR_LONG_LAST_BLK_FTR(last_blk))->start;
		blk = BLOCKXPTR(_start_);
		cursor = (PSTR_LONG_LAST_BLK_FTR(last_blk))->cursor;
		ofs = (unsigned int)((char*)XADDR(_start_) - (char*)XADDR(blk));
		m_start = _start_;//FIXME
	}

	/// sets current position to the end of the string
	/// end_indicator value is ignored, it is used to indicate another constructor only
	pstr_long_cursor(const xptr &_ptr_, int end_indicator);

	//FIXME
	xptr blk;
	xptr last_blk;
	unsigned int ofs;
	int cursor;

    // block oriented copy. buf must have size not less than a page size
	// returns number of bytes copied, 0 if end of string reached.
    virtual size_t copy_blk(char *buf);
	// get's a pointer to string part in the current block and moves cursor to the next block
	// (same as copy_blk, but without copy)
	// returns the length of the string part
	//     or 0 if end of string reached (*ptr is not modified in this case)
	virtual size_t get_blk(char **ptr);


	// like get_blk, but gets data from the first byte in the current block till
	// the cursor position (excluding byte, pointed by cursor)
	// and moves cursor after the end of the previous block (thus making it incompatible with iterator functions)
	// or to the string beginning
	int get_blk_rev(char **ptr);
};

class pstr_long_iterator : public pstr_long_cursor
{
protected:
	pstr_long_off_t m_pos;
public:
    typedef pstr_long_off_t off_t;
    /// creates NULL iterator (i.e. not equal to any valid iterator)
	pstr_long_iterator() : pstr_long_cursor()
	{
		m_pos = -1;
	}
	/// creates iterator pointing to string beginning
	pstr_long_iterator(const xptr &_ptr_) : pstr_long_cursor(_ptr_)
	{
		m_pos = 0;
	}

	/// creates iterator pointing to the end of string
	/// size MUST be exactly the string size in bytes
	pstr_long_iterator(const xptr &_ptr_, pstr_long_off_t _size_) : pstr_long_cursor(_ptr_, true)
	{
		m_pos = _size_;
	}

	unsigned char operator *() const
	{
		U_ASSERT(blk != XNULL);
		CHECKP(blk);
		return *((char*)XADDR(blk) + ofs);
	}

	pstr_long_iterator &operator ++()
	{
		//no error checks here, TODO - add some assertions
		//we don't care if we go after the end of string too far
		++m_pos;
		if (++ofs >= PAGE_SIZE)
		{
			CHECKP(blk);
			if (PSTR_LONG_BLK_HDR(blk)->next_blk != XNULL &&
				((PSTR_LONG_BLK_HDR(blk)->next_blk != last_blk) || (cursor >= 0)))
			{
				blk = PSTR_LONG_BLK_HDR(blk)->next_blk;
				ofs = PSTR_LONG_BLK_HDR_SIZE;
			}
		}
		return *this;
	}

	pstr_long_iterator &operator --()
	{
		//no error checks here, TODO - add some assertions
		//we don't care if we go before the start of string too far
		--m_pos;
		if (((int)(--ofs)) < (int) PSTR_LONG_BLK_HDR_SIZE)
		{
			CHECKP(blk);
			if (PSTR_LONG_BLK_HDR(blk)->prev_blk != XNULL)
			{
				blk = PSTR_LONG_BLK_HDR(blk)->prev_blk;
				ofs = PAGE_SIZE - 1;
			}
		}
		return *this;
	}
	inline pstr_long_off_t get_pos() const { return m_pos; }

	//FIXME!
	pstr_long_iterator operator ++(int) {pstr_long_iterator old(*this); ++(*this); return old; }
	pstr_long_iterator operator --(int) {pstr_long_iterator old(*this); --(*this); return old; }

	//FIXME: += -= and mb other operators don't work with negative values of arguments!!!

	pstr_long_iterator &operator +=(pstr_long_off_t x)
	{
		U_ASSERT(x >= 0);
		//no error checks here, TODO - add some assertions
		//we don't care if we go after the end of string too far
		m_pos += x;
		while (((pstr_long_off_t)ofs+x) >= PAGE_SIZE)
		{
			CHECKP(blk);
			if (PSTR_LONG_BLK_HDR(blk)->next_blk != XNULL &&
				((PSTR_LONG_BLK_HDR(blk)->next_blk != last_blk) || (cursor >= 0)))
			{
				blk = PSTR_LONG_BLK_HDR(blk)->next_blk;
				x -= PAGE_SIZE - ofs;
				ofs = PSTR_LONG_BLK_HDR_SIZE;
			}
			else
				break;
		}
		U_ASSERT(x >= 0 && x < PAGE_SIZE);
		ofs += (unsigned int)x;
		return *this;
	}
	pstr_long_iterator &operator -=(pstr_long_off_t x)
	{
		U_ASSERT(x >= 0);
		//no error checks here, TODO - add some assertions
		//we don't care if we go before the start of string too far
		m_pos -= x;
		while (((pstr_long_off_t)ofs) < x + (pstr_long_off_t)PSTR_LONG_BLK_HDR_SIZE) //ofs - x < PSTR_LONG_BLK_HDR_SIZE
		{
			CHECKP(blk);
			if (PSTR_LONG_BLK_HDR(blk)->prev_blk != XNULL)
			{
				blk = PSTR_LONG_BLK_HDR(blk)->prev_blk;
				x -= 1 + ofs - PSTR_LONG_BLK_HDR_SIZE;
				ofs = PAGE_SIZE - 1;
			}
			else
				break;
		}
		U_ASSERT(x >= 0 && x <= (pstr_long_off_t)ofs);
		ofs -= (unsigned int)x;
		return *this;
	}

	//default `=` operator is ok
};

inline bool operator ==(const pstr_long_iterator& it1, const pstr_long_iterator& it2)
{
	return it1.get_pos() == it2.get_pos();
}
inline bool operator !=(const pstr_long_iterator& it1, const pstr_long_iterator& it2)
{
	return it1.get_pos() != it2.get_pos();
}
inline bool operator <=(const pstr_long_iterator& it1, const pstr_long_iterator& it2)
{
	return it1.get_pos() <= it2.get_pos();
}
inline bool operator < (const pstr_long_iterator& it1, const pstr_long_iterator& it2)
{
	return it1.get_pos() < it2.get_pos();
}
inline bool operator >=(const pstr_long_iterator& it1, const pstr_long_iterator& it2)
{
	return it1.get_pos() >= it2.get_pos();
}
inline bool operator > (const pstr_long_iterator& it1, const pstr_long_iterator& it2)
{
	return it1.get_pos() > it2.get_pos();
}
inline pstr_long_off_t operator - (const pstr_long_iterator& it1, const pstr_long_iterator& it2)
{
	return it1.get_pos() - it2.get_pos();
}

#endif //_PSTR_LONG_H

