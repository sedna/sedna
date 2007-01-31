/*
 * File:  pers_free.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


/* pers_free.cpp - implementation of _pers_free
   Copyright (c) 2003, 2004 ISP RAS. All rights reserved. */

/* free.c - C standard library routine.
   Copyright (c) 1989, 1993  Michael J. Haertel

   You may redistribute this library under the terms of the
   GNU Library General Public License (version 2 or any later
   version) as published by the Free Software Foundation.
   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY EXPRESS OR IMPLIED
   WARRANTY.  IN PARTICULAR, THE AUTHOR MAKES NO REPRESENTATION OR
   WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS
   SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE. */

#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include "common/ph/_pers_malloc.h"

/* Return memory to the heap. */
void
_pers_free(void *ptr)
{
    int block, blocks, i, type;
    struct list *prev, *next;

    if (!ptr)
	return;

    block = BLOCK(ptr);

    switch (type = _vars->_heapinfo[block].busy.type) {
    case 0:
	/* Find the free cluster previous to this one in the free list.
	   Start searching at the last block referenced; this may benefit
	   programs with locality of allocation. */
	i = _vars->_heapindex;
	if (i > block)
	    while (i > block)
		i = _vars->_heapinfo[i].free.prev;
	else {
	    do
		i = _vars->_heapinfo[i].free.next;
	    while (i > 0 && i < block);
	    i = _vars->_heapinfo[i].free.prev;
	}

	/* Determine how to link this block into the free list. */
	if (block == i + _vars->_heapinfo[i].free.size) {
	    /* Coalesce this block with its predecessor. */
	    _vars->_heapinfo[i].free.size += _vars->_heapinfo[block].busy.info.size;
	    block = i;
	} else {
	    /* Really link this block back into the free list. */
	    _vars->_heapinfo[block].free.size = _vars->_heapinfo[block].busy.info.size;
	    _vars->_heapinfo[block].free.next = _vars->_heapinfo[i].free.next;
	    _vars->_heapinfo[block].free.prev = i;
	    _vars->_heapinfo[i].free.next = block;
	    _vars->_heapinfo[_vars->_heapinfo[block].free.next].free.prev = block;
	}

	/* Now that the block is linked in, see if we can coalesce it
	   with its successor (by deleting its successor from the list
	   and adding in its size). */
	if (block + _vars->_heapinfo[block].free.size == _vars->_heapinfo[block].free.next) {
	    _vars->_heapinfo[block].free.size
		+= _vars->_heapinfo[_vars->_heapinfo[block].free.next].free.size;
	    _vars->_heapinfo[block].free.next
		= _vars->_heapinfo[_vars->_heapinfo[block].free.next].free.next;
	    _vars->_heapinfo[_vars->_heapinfo[block].free.next].free.prev = block;
	}

	/* Now see if we can return stuff to the system. */
	blocks = _vars->_heapinfo[block].free.size;
	if (blocks >= FINAL_FREE_BLOCKS && block + blocks == _vars->_heaplimit
	    && (*_morecore)(0) == ADDRESS(block + blocks)) {
	    _vars->_heaplimit -= blocks;
	    (*_morecore)(-blocks * BLOCKSIZE);
	    _vars->_heapinfo[_vars->_heapinfo[block].free.prev].free.next
		= _vars->_heapinfo[block].free.next;
	    _vars->_heapinfo[_vars->_heapinfo[block].free.next].free.prev
		= _vars->_heapinfo[block].free.prev;
	    block = _vars->_heapinfo[block].free.prev;
	}

	/* Set the next search to begin at this block. */
	_vars->_heapindex = block;
	break;

    default:
	/* Get the address of the first free fragment in this block. */
	prev = (struct list *) ((char *) ADDRESS(block)
				+ (_vars->_heapinfo[block].busy.info.frag.first
				   << type));

	if (_vars->_heapinfo[block].busy.info.frag.nfree == (BLOCKSIZE >> type) - 1
	&& _vars->_fragblocks[type] > 1) {
	    /* If all fragments of this block are free, remove them
	       from the fragment list and free the whole block. */
	    --_vars->_fragblocks[type];
	    for (next = prev, i = 1; i < BLOCKSIZE >> type; ++i)
		next = next->next;
	    prev->prev->next = next;
	    if (next)
		next->prev = prev->prev;
	    _vars->_heapinfo[block].busy.type = 0;
	    _vars->_heapinfo[block].busy.info.size = 1;
	    _pers_free(ADDRESS(block));
	} else if (_vars->_heapinfo[block].busy.info.frag.nfree) {
	    /* If some fragments of this block are free, link this fragment
	       into the fragment list after the first free fragment of
	       this block. */
	    next = (struct list *)ptr;
	    next->next = prev->next;
	    next->prev = prev;
	    prev->next = next;
	    if (next->next)
		next->next->prev = next;
	    ++_vars->_heapinfo[block].busy.info.frag.nfree;
	} else {
	    /* No fragments of this block are free, so link this fragment
	       into the fragment list and announce that it is the first
	       free fragment of this block. */
	    prev = (struct list *) ptr;
	    _vars->_heapinfo[block].busy.info.frag.nfree = 1;
	    _vars->_heapinfo[block].busy.info.frag.first
		= (unsigned int) ((char *) ptr - (char *) NULL) % BLOCKSIZE
		  >> type;
	    prev->next = _vars->_fraghead[type].next;
	    prev->prev = &_vars->_fraghead[type];
	    prev->prev->next = prev;
	    if (prev->next)
		prev->next->prev = prev;
	}
	break;
    }
}

