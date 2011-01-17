/*
 * File:  buff.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/btree/buff.h"

char	buf1[BT_PAGE_SIZE];
char	buf2[BT_PAGE_SIZE];
shft	ksize;			/* key size (for fixed-sized keys); if zero the keys are of variable size */
char*	heap;			/* pointer to the top of heap */
char*	buf;			/* pointer to the head of buffer */

/* makes tuning of buffering functions for operation with
   various kinds of keys (both fixed and variable sized);
   if first_buffer is true - tune to the first buffer, else - to the second;
   sets internal pointer buf used by buffering functions to beginign of the claimed buffer;
   resets the heap pointer to the end of corresponding buffer;
   returns pointer to the buffer exploited
 */
char* bt_tune_buffering(bool first_buffer, shft key_size) {
	if (first_buffer)	{ heap=buf1+BT_PAGE_SIZE; buf=buf1; }
	else				{ heap=buf2+BT_PAGE_SIZE; buf=buf2; }
	ksize=key_size;
	return buf;
}

/* copy page header into current buffer pointed by dst.
   dst is automatically shifted to the end of copy region
 */
void bt_buffer_header(char* pg, char* & dst) {
	memcpy(buf, pg, BT_HSIZE);
	dst+=BT_HSIZE;
}

/* fully transfer the key to the buffer.
   src must point to the slot in key table inside btree page
   dst must point to the slot in key table inside buffer where to transfer
   src and dst pointers are automatically shifted to the next slots in their tables
 */
void bt_buffer_key(char* pg, char* & src, char* & dst) {
	if (ksize) {
		memcpy(dst, src, ksize);
		src+=ksize;
		dst+=ksize;
	} else {
		char*	key_content = pg + *(shft*)src;
		shft	key_size = *((shft*)src + 1);
		heap-=key_size;
		memcpy(heap, key_content, key_size);
		*(shft*)dst = (shft)(heap-buf);
		*((shft*)dst + 1) = key_size;
		src+=2*sizeof(shft);
		dst+=2*sizeof(shft);
	}
}

/* fully transfer the bitptr to the buffer.
   src must point to the slot in bigptr table inside btree page
   dst must point to the slot in bigptr table inside buffer where to transfer
   src and dst pointers are automatically shifted to the next slots in their tables
 */
void bt_buffer_bigptr(char* pg, char* & src, char* & dst) {
	/* bigptr slots are of fixed-size keeping xptrs */
	memcpy(dst, src, sizeof(xptr));
	src+=sizeof(xptr);
	dst+=sizeof(xptr);
}

/* fully transfer the chunk of objects to the buffer.
   src must point to the slot in chunk table inside btree page
   dst must point to the slot in chunk table inside buffer where to transfer
   src and dst pointers are automatically shifted to the next slots in their tables
 */
template<typename object>
void bt_buffer_chnk_tmpl(char* pg, char* & src, char* & dst) {
	char* chnk_ptr = pg + *(shft*)src;
	shft  chnk_size = *((shft*)src + 1);
	heap-=chnk_size*sizeof(object);
	memcpy(heap, chnk_ptr, chnk_size*sizeof(object));
	*(shft*)dst=(shft)(heap-buf);
	*((shft*)dst+1)=chnk_size;
	src+=2*sizeof(shft);
	dst+=2*sizeof(shft);
}


/* returns current shift of heap top in current buffer */
shft bt_buffer_heap_shft() {
	return (shft)(heap-buf);
}

/* decrements heap pointer in current buffer on given value */
void bt_buffer_heap_shft_dec(shft size) {
	heap-=size;
}

/* increments heap pointer in current buffer on given value */
void bt_buffer_heap_shft_inc(shft size) {
	heap+=size;
}
