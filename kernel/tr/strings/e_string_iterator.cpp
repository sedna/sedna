/*
 * File:  e_string_iterator.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/strings/e_string_iterator.h"
	
using namespace std;

//Take xptr to the string stored in blocks and the size of the string in bytes
estr_iterator::estr_iterator(int _chars_left_, xptr _s_)
{
  chars_left = _chars_left_; 
  s = _s_;
  cur_block_xptr = BLOCKXPTR(s);
  cur_block_str_start_p =(char*) XADDR(s);
  cur_p =(char*) XADDR(s);
}

estr_iterator& estr_iterator::operator ++() 
{ 
	if (chars_left<=0)
		throw USER_EXCEPTION2(SE1003, "e_string_iterator run out of the string end");
	else if (cur_p+1 >= (char*) XADDR(cur_block_xptr) + PAGE_SIZE)
	{//jump to next block
		xptr nblk = E_STR_BLK_HDR(cur_block_xptr)->nblk;
		cur_block_xptr = nblk;
		CHECKP(cur_block_xptr);
		cur_block_str_start_p = (char*)XADDR(nblk)+ sizeof(e_str_blk_hdr);

		cur_p = cur_block_str_start_p;
		chars_left--;
	}
	else {cur_p++; chars_left--;}
	return *this; 
}

estr_iterator& estr_iterator::operator --()
{ 
	if (cur_p-1 < cur_block_str_start_p)
	{//jump to the prev block
		xptr pblk = E_STR_BLK_HDR(cur_block_xptr)->pblk;
		if (pblk == XNULL)
			throw USER_EXCEPTION2(SE1003, "e_string_iterator run out of the string beginning");
		cur_block_xptr = pblk;
		CHECKP(cur_block_xptr);
		cur_block_str_start_p = (char*)XADDR(pblk)+ sizeof(e_str_blk_hdr); //FIXME - wrong!, may go boyond string start

		cur_p = (char*)XADDR(cur_block_xptr)+PAGE_SIZE-1;
		chars_left++;
	}
	else {cur_p--; chars_left++;}
	return *this;
}


