/*
 * File:  buff.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BUFF_H
#define _BUFF_H

#include "common/sedna.h"
#include "common/base.h"

#include "tr/btree/btpage.h"
#include "tr/btree/btstruct.h"

/* functions used for temporary partial buffering of btree page contents in internal buffers
   in format equivalent to btree page format, with subsequent copying buffers into acutal
   btree pages. The buffers used are static regions sequentially reused. Is used for btree pages
   splitting
 */

char*	bt_tune_buffering(bool first_buffer, shft key_size);
void	bt_buffer_header(char* pg, char* & dst);
void	bt_buffer_key(char* pg, char* & src, char* & dst);
void	bt_buffer_bigptr(char* pg, char* & src, char* & dst);
template<typename object>
void	bt_buffer_chnk_tmpl(char* pg, char* & src, char* & dst);
shft	bt_buffer_heap_shft();
void	bt_buffer_heap_shft_dec(shft size);
void	bt_buffer_heap_shft_inc(shft size);

#endif
