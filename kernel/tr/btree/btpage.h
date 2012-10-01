/*
 * File:  btpage.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BTPAGE_H
#define _BTPAGE_H

#include "common/sedna.h"
#include "common/xptr/xptr.h"
#include "common/xptr/sm_vmm_data.h"

#include "tr/structures/nodetypes.h"

struct btree_blk_hdr
{
    vmm_sm_blk_hdr sm_vmm;

    xptr next;
    xptr prev;
    xptr _deprecated_parent;
    xptr lmp;

    bool is_leaf;
    bool is_clus;
    bool is_clus_head;
    bool is_clus_tail;

    xmlscm_type key_type;

    shft key_size;
    shft key_num;
    shft heap;
};

struct btree_chnk_hdr
{
	shft	c_shft;
	shft	c_size;
};

struct btree_key_hdr
{
	shft	k_shft;
	shft	k_size;
};

/*
	Makeup of a page and funcs used in manipulating btree pages
	===========================================================
	Btree page is an unstructured mess stuffed into a character buffer with all locations being determined
	via pointer arithmetic. The leaf and non-leaf pages are different, though the header is the same, while
	some header fields are intended for leaf pages, other - for non-leaf pages.

	Page header:
	==============
	xptr				next page - pointer to the next page. Both for leaf and non-leaf pages links pages
						at the same tree level in key-ascending order by this pointer

	xptr				prev page - pointer to the prev page. Both for leaf and non-leaf pages links pages
						at the same tree level in key-descending order by this pointer

	xptr				parent page - pointer to the parent page

	xptr				leftmost pointer - pointer to the page of keys less than all keys in
						current page (used in non-leaf pages)

	bool				leaf/non-leaf page

	bool				cluster page (indicates that the page is inside cluster)

	bool				cluster head page

	bool				cluster tail page

	sizeof(xmlscm_type)	type of keys in the page

	shft				size of keys (used for fixed-size keys)
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	Note:				This is used extra as a flag, i.e. if the page stores fixed-size keys, this fiels > 0,
						otherwise this field = 0, which indicates that page stores variable-size keys
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	shft				number of keys in the page

	shft				heap boundary (the boundary of heap area keeping key contents and object chunks
						growing in right-to-left direction from page tail)

	For non-leaf nodes:
	====================
	The page consists of key table ordered by key value, followed directly by big_ptr table of size equal to key
	table size, entries of which correspond to keys in the key table.The key table and
	big_ptr table dynamically grow in left-to-right direction from the page header.The rest of the page
	(heap) is dynamically filled with key contents (in right-to-left-direction from page tail) - for
	variable-size keys case.
	Key table:
	~~~~~~~~~
	For fixed-length keys keeps key contents in each element of fixed size, defined by 'size of keys'
	field in page header.
	For variable-length keys keeps pairs {key_shft;key_size}, where:
	shft	'key_shft' - shft to the key contents begining inside page
	shft	'key_size' - size of the key in bytes
	Big_ptr table:
	~~~~~~~~~~~~~
	Consists of {big_ptr} elements, where:
	xptr	'big_ptr' - pointer to the page with keys above or equal corresponding key in key table.


	For leaf nodes:
	===================
	Similar to non-leaf page, the page consists of key table ordered by key value, (note that big_ptr table is
	absent) followed
	by object chunk table. Key table is organized similar to non-leaf pages (see above).
	The key table and object chunk table grow dynamically in left-to-right direction from
	the page header. The rest of the page (heap) is dynamically filled with key contents and chunks of
	objects (in right-to-left-direction from page tail)
	Chunk table:
	~~~~~~~~~~~
	Chunk table keeps pairs {chnk_shft; chnk_size} each describing chunk of objects sticking to the
	corresponding key in key table, where:
	shft	'chnk_shft' - shft of the chunk inside the page
	shft	'chnk_size' - number of objects in the chunk

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	Note:
	* The objects inside the chunk are ordered by value (in left-to-right direction)
	* We assume
	the sequence of objects with common key value may propogate to a number of consequtive leaf pages.
	Still, as the page is rather big (typically 64K) and the size of object is small, we consider this
	situation to be non-frequent. This case is identified as cluster of pages, such pages are marked
	with flags inside the page. Thus, when performing
	search for all objects with given key value or searching some concrete object, in general we must
	traverse a sequence of consequtive pages.

 */

#define BT_PAGE_SIZE PAGE_SIZE //4*1024

/* this switch define if the btree implementation will support clusters of leaf pages. If defined,
   the groups of objects sticking to common key which don't fit in single block will be permitted
   to propogate to neighbouring blocks thus constituting clusters of pages. Otherwise, the strict
   restriction is imposed to the number of objects which can stick to common key. Btree software
   will generate exception in case of overhead.
*/
#define	PERMIT_CLUSTERS

/* obsolete
enum key_type {
	KEY_INT,
	KEY_STRING,
	KEY_DATE,
	KEY_UNDEF
};
*/

/* checks key types as variable or fixed-length */
inline	bool	BT_VARIABLE_KEY_TYPE(xmlscm_type t)
											{
											switch (t) {
												case xs_string:
													return true;
													break;
												case xs_date:
												case xs_time:
												case xs_dateTime:
												case xs_duration:
												case xs_yearMonthDuration:
												case xs_dayTimeDuration:
												case xs_integer:
												case xs_float:
												case xs_double:
													return false;
													break;
												default:
                                                    throw USER_EXCEPTION2(SE1008, "Unexpected key type in BT_VARIABLE_KEY_TYPE()");
											}
											}
/* check the same but using KEY_SIZE field of the given page */
inline	bool	BT_VARIABLE_KEY_TYPE(char* p)
											{
											if (((btree_blk_hdr*)p)->key_size)
												return false;
											else
												return true;
											}

/* b-tree page header size */
#define	BT_HSIZE		(sizeof(btree_blk_hdr))
#define BT_PAGE_PAYLOAD	(BT_PAGE_SIZE - BT_HSIZE)

/* Macros to access page header fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/
#define BT_NEXT_PTR(p)						(&BT_NEXT(p))
#define BT_NEXT(p)							(((btree_blk_hdr*)(p))->next)

#define BT_PREV_PTR(p)						(&BT_PREV(p))
#define BT_PREV(p)							(((btree_blk_hdr*)(p))->prev)

#define BT_PARENT_PTR(p)					(&BT_PARENT(p))
#define BT_PARENT(p)						(((btree_blk_hdr*)(p))->parent)

#define BT_LMP_PTR(p)						(&BT_LMP(p))
#define BT_LMP(p)							(((btree_blk_hdr*)(p))->lmp)

#define BT_IS_LEAF_PTR(p)					(&BT_IS_LEAF(p))
#define BT_IS_LEAF(p)						(((btree_blk_hdr*)(p))->is_leaf)

#define BT_IS_CLUS_PTR(p)					(&BT_IS_CLUS(p))
#define BT_IS_CLUS(p)						(((btree_blk_hdr*)(p))->is_clus)

#define BT_IS_CLUS_HEAD_PTR(p)				(&BT_IS_CLUS_HEAD(p))
#define BT_IS_CLUS_HEAD(p)					(((btree_blk_hdr*)(p))->is_clus_head)

#define BT_IS_CLUS_TAIL_PTR(p)				(&BT_IS_CLUS_TAIL(p))
#define BT_IS_CLUS_TAIL(p)					(((btree_blk_hdr*)(p))->is_clus_tail)

#define BT_KEY_TYPE_PTR(p)					(&BT_KEY_TYPE(p))
#define BT_KEY_TYPE(p)						(((btree_blk_hdr*)(p))->key_type)

/* for fixed-size keys defines the size of keys stored in the page
   Serves as the flag of page containing variable-size keys if 0 */
#define BT_KEY_SIZE_PTR(p)					(&BT_KEY_SIZE(p))
#define BT_KEY_SIZE(p)						(((btree_blk_hdr*)(p))->key_size)

#define BT_KEY_NUM_PTR(p)					(&BT_KEY_NUM(p))
#define BT_KEY_NUM(p)						(((btree_blk_hdr*)(p))->key_num)

/* heap boundary (grows in right-to-left direction from page tail) */
#define BT_HEAP_PTR(p)						(&BT_HEAP(p))
#define BT_HEAP(p)							(((btree_blk_hdr*)(p))->heap)
#define BT_HEAP_SET_NULL(p)                 (((btree_blk_hdr*)(p))->heap = (shft) BT_PAGE_SIZE)

/* Macros to access tables and their elements and misc
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

/* head of key table (page body begining) */
inline	char*	BT_KEY_TAB(char* p)			{return (p + BT_HSIZE);}

/* returns the actual length of the record in key table */
inline	shft	BT_KEY_TAB_SIZE(char* pg)
											{
											 return (BT_KEY_SIZE(pg)) ? (BT_KEY_SIZE(pg)) : 2*sizeof(shft);
											}


/* return pointer to the i-th element of key table (counted from 0) */
inline	char*	BT_KEY_TAB_AT(char* pg, shft i)
											{
											/* assume the page is correct */
											shft el_size = (BT_KEY_SIZE(pg))?
															(BT_KEY_SIZE(pg)):
															 2*sizeof(shft);
											return (BT_KEY_TAB(pg) + i*el_size);
											}

inline	btree_key_hdr *	BT_KEY_ITEM_AT(char* pg, shft i) {
											/* assume the caller knows what he need it for */
											return ((btree_key_hdr *) (BT_KEY_TAB(pg) + i*2*sizeof(shft)));
											}

/* head of big_ptr table */
inline	char*	BT_BIGPTR_TAB(char* p)		{ return BT_KEY_TAB_AT(p, BT_KEY_NUM(p)); }

/* return pointer to the i-th element of big_ptr table (counted from 0) */
inline	char*	BT_BIGPTR_TAB_AT(char* p, shft i)
											{return (BT_BIGPTR_TAB(p) + i*sizeof(xptr));}

/* head of chunk table. Same as BIG_PTR_TAB functions due to the fact that
   big_ptr table and chunk table are mutualy exclusive for non-leaf and leaf page
   layouts correspondingly */
inline	char*	BT_CHNK_TAB(char* p)		{ return BT_BIGPTR_TAB(p);}

/* return pointer to the i-th element of chunk table (counted from 0) */
inline	char*	BT_CHNK_TAB_AT(char* p, shft i)
											{return (BT_CHNK_TAB(p) + i*2*sizeof(shft));}

/* the same as previous, but return value is casted to (btree_chnk_hdr *) */
inline	btree_chnk_hdr *	BT_CHNK_ITEM_AT(char* p, shft i)
											{return ((btree_chnk_hdr *)(BT_CHNK_TAB(p) + i*2*sizeof(shft)));}


/* return shift field of the i-th element of chunk table */
inline	shft	BT_CHNK_ITEM_SHIFT(char* p, shft i)
											{return (BT_CHNK_ITEM_AT(p, i)->c_shft);}

/* return size field of the i-th element of chunk table */
inline	shft	BT_CHNK_ITEM_SIZE(char* p, shft i)
											{return (BT_CHNK_ITEM_AT(p, i)->c_size);}

/* current amount of page free space */
inline	shft	BT_PFS(char* p)				{
											if (BT_IS_LEAF(p))
												return (shft)(p + BT_HEAP(p) -
														BT_CHNK_TAB_AT(p, BT_KEY_NUM(p)));
											else
												return (shft)(p + BT_HEAP(p) -
														BT_BIGPTR_TAB_AT(p, BT_KEY_NUM(p)));
											}
#endif
