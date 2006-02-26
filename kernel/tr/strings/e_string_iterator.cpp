/*
 * File:  e_string_iterator.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "e_string_iterator.h"
	
using namespace std;

//Take xptr to the string stored in blocks and the size of the string in bytes
e_string_iterator::e_string_iterator(int _chars_left_, xptr _s_, prev_blocks_list_holder *hld) : prev_blocks(hld)
{
  chars_left = _chars_left_; 
  s = _s_;
  cur_block_xptr = BLOCKXPTR(s);
  cur_block_str_start_p =(char*) XADDR(s);
  cur_p =(char*) XADDR(s);
  prev_blocks->pb_list.push_back(new prev_blocks_list_holder::block_info_t(cur_block_xptr, cur_block_str_start_p));     	  	  
  cur_block_in_list_pos = --prev_blocks->pb_list.end();
}


/*
e_string_iterator& e_string_iterator::operator =(const e_string_iterator& _esi_)
{
  chars_left = _esi_.chars_left; 
  s = _esi_.s;
  cur_block_xptr = _esi_.cur_block_xptr;
  cur_block_str_start_p = _esi_.cur_block_str_start_p;
  cur_p = _esi_.cur_p;


  prev_blocks = _esi_.prev_blocks;
  cur_block_in_list_pos = _esi_.cur_block_in_list_pos;

  return *this;
}

e_string_iterator::~e_string_iterator()
{
}
*/

e_string_iterator& e_string_iterator::operator ++() 
{ 
	if (chars_left<=0)
		throw USER_EXCEPTION2(SE1003, "e_string_iterator run out of the string end");
	else if (cur_p+1 >= (char*) XADDR(cur_block_xptr) + PAGE_SIZE)
	{//jump to next block
		if (cur_block_in_list_pos == --prev_blocks->pb_list.end())
		{//next block is not in the list of prev blocks	
    		xptr nblk = E_STR_BLK_HDR(cur_block_xptr)->nblk;
			cur_block_xptr = nblk;
			CHECKP(cur_block_xptr);
			cur_block_str_start_p = (char*)XADDR(nblk)+ sizeof(e_str_blk_hdr);

			cur_p = cur_block_str_start_p;
			prev_blocks->pb_list.push_back(new prev_blocks_list_holder::block_info_t(cur_block_xptr, cur_block_str_start_p));     	
			//add node to the list of previous blocks
    		cur_block_in_list_pos = --prev_blocks->pb_list.end();
			chars_left--;
		}
		else {//next block is in the list
			cur_block_in_list_pos++;
			cur_block_xptr = (*cur_block_in_list_pos)->block_xptr;
			cur_block_str_start_p = (*cur_block_in_list_pos)->block_str_start_p; 
			cur_p = cur_block_str_start_p;
			CHECKP(cur_block_xptr);
			chars_left--;
		}
	}
	else {cur_p++; chars_left--;}
	return *this; 
}

e_string_iterator& e_string_iterator::operator --() 
{ 
	if (cur_p-1 < cur_block_str_start_p)
	{//jump to the prev block
		if (cur_block_in_list_pos == prev_blocks->pb_list.begin())
            throw USER_EXCEPTION2(SE1003, "e_string_iterator run out of the string begining");
		else 
		{
			//FIXME never tested/checked this!
			cur_block_in_list_pos--;
			prev_blocks_list_holder::block_info_t* bi =*cur_block_in_list_pos; 
			cur_block_xptr = bi->block_xptr;
			cur_block_str_start_p = bi->block_str_start_p;
			cur_p = (char*)XADDR(cur_block_xptr)+PAGE_SIZE-1;
			CHECKP(cur_block_xptr);
			chars_left++;
		}
	}
	else {cur_p--; chars_left++;}
	return *this;
}

e_string_iterator_first::~e_string_iterator_first()
{
	delete prev_blocks;
}

/*
#include <boost/regex.hpp>
	
bool matches(e_string_iterator& s, char* pattern)
{	
	boost::regex reg_expr(pattern);
	return boost::regex_match(s.begin(), s.end(), reg_expr, boost::match_default);
}
*/


