/*
 * File:  _pers_malloc.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


/* _pers_malloc.h - internal declarations for persistent allocator.
   Copyright (c) 2003, 2004 ISP RAS. All rights reserved. */

/* malloc.h - declarations for the allocator.
   Copyright (c) 1989, 1993  Michael J. Haertel

   You may redistribute this library under the terms of the
   GNU Library General Public License (version 2 or any later
   version) as published by the Free Software Foundation.
   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY EXPRESS OR IMPLIED
   WARRANTY.  IN PARTICULAR, THE AUTHOR MAKES NO REPRESENTATION OR
   WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS
   SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE. */

#ifndef __PERS_MALLOC_H
#define __PERS_MALLOC_H

/* Underlying allocation function; successive calls should return
   contiguous pieces of memory. */
extern void *(*_morecore)(long);

/* Default value of previous. */
/*extern void *_default_morecore(long);*/

/* Allocation function for allocating persistent storage and mapping it
   to main memory */
extern void *_persistent_morecore(long);

#define _morecore		_persistent_morecore


/* The allocator divides the heap into blocks of fixed size; large
   requests receive one or more whole blocks, and small requests
   receive a fragment of a block.  Fragment sizes are powers of two,
   and all fragments of a block are the same size.  When all the
   fragments in a block have been freed, the block itself is freed.
   WARNING: BLOCKSIZE must be set greater than or equal to the
   machine's page size for valloc() to work correctly.  The default
   definition here is 4096 bytes. */
#define INT_BIT (CHAR_BIT * sizeof (int))
#define BLOCKLOG (INT_BIT > 16 ? 12 : 9)
#define BLOCKSIZE (1 << BLOCKLOG)
#define BLOCKIFY(SIZE) (((SIZE) + BLOCKSIZE - 1) / BLOCKSIZE)

/* Determine the amount of memory spanned by the initial heap table
   (not an absolute limit). */
#define HEAP (INT_BIT > 16 ? 4194304 : 65536)

/* Number of contiguous free blocks allowed to build up at the end of
   memory before they will be returned to the system. */
#define FINAL_FREE_BLOCKS 8

/* Data structure giving per-block information. */
union info {
    struct {
	int type;		/* Zero for a large block, or positive
				   giving the logarithm to the base two
				   of the fragment size. */
	union {
	    struct {
		int nfree;	/* Free fragments in a fragmented block. */
		int first;	/* First free fragment of the block. */
	    } frag;
	    int size;		/* Size (in blocks) of a large cluster. */
	} info;
    } busy;
    struct {
	int size;		/* Size (in blocks) of a free cluster. */
	int next;		/* Index of next free cluster. */
	int prev;		/* Index of previous free cluster. */
    } free;
};

/* Address to block number and vice versa. */
#define BLOCK(A) (((char *) (A) - _vars->_heapbase) / BLOCKSIZE + 1)
#define ADDRESS(B) ((void *) (((B) - 1) * BLOCKSIZE + _vars->_heapbase))

/* Doubly linked lists of free fragments. */
struct list {
    struct list *next;
    struct list *prev;
};


struct pers_malloc_vars
{
    /* Pointer to first block of the heap. */
    char *_heapbase;

    /* Table indexed by block number giving per-block information. */
    union info *_heapinfo;

    /* Current search index for the heap table. */
    int _heapindex;

    /* Limit of valid info table indices. */
    int _heaplimit;

    /* Count of blocks for each fragment size. */
    int _fragblocks[BLOCKLOG];

    /* Free list headers for each fragment size. */
    struct list _fraghead[BLOCKLOG];

    /* Number of info entries. */
    int heapsize;

    /* Are we experienced? */
    int initialized;
};

extern struct pers_malloc_vars *_vars;

void *_pers_malloc(size_t size);
void *_pers_realloc(void *ptr, size_t size);
void _pers_free(void *ptr);


#endif
