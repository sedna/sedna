/*
 * File:  pstr_long.cpp
 * Copyright (C) 2005 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "common/errdbg/d_printf.h"

#include "tr/vmm/vmm.h"
#include "tr/strings/strings.h"
#include "tr/strings/e_string.h"
#include "tr/pstr/pstr_long.h"
#include "tr/pstr/pstr.h"
#include "tr/structures/nodeutils.h"
#include "tr/tr_base.h"
#include "tr/tr_globals.h"

#if 1
#define DBGBLOCKS_PRITNF(x)
#else
#define DBGBLOCKS_PRITNF(x) printf x
#endif

//TODO!!!! write plog when overwrinting unused space in existing blocks !!!!

//static variables used when operating with strings
static CharCounter		*intl_char_counter;
static pstr_long_block_list_entry		*intl_last_block_list_entry;
static char				intl_map_buf[PAGE_SIZE];
static pstr_long_block_list_map_entry	*intl_last_map_entry;
static xptr				intl_last_blk;
static xptr				intl_last_block_list_block;
static xptr				intl_first_blb;

static pstr_long_last_blk_ftr	intl_ftr;

inline static
void setNodePstrLongData(xptr node, xptr textptr)
{
    WRITEP(node);
    internal::node_text_t * nodetext = getTextFromAnyNode(node);
    memcpy(nodetext->data, &textptr, sizeof(xptr));
    nodetext->size = internal::textInPstrLong;
}

inline static
xptr getNodePstrLongData(xptr node)
{
    xptr result;
    CHECKP(node);
    internal::node_text_t * nodetext = getTextFromAnyNode(node);
    U_ASSERT(nodetext->size == internal::textInPstrLong);
    memcpy(&result, nodetext->data, sizeof(xptr));
    return result;
}

inline static
void clearPstrLong(xptr node)
{
    WRITEP(node);
    getTextFromAnyNode(node)->size = 0;
}


#ifndef MIN
#define MIN(a,b)		((a) < (b) ? (a) : (b))
#endif

static inline void intl_alloc_blk(bool persistent, xptr &new_blk)
{
	if (persistent)
	{
		vmm_alloc_data_block(&new_blk);
	}
	else
		vmm_alloc_tmp_block(&new_blk);

	RECOVERY_CRASH;
	DBGBLOCKS_PRITNF(("alloc_blk: 0x%08lx %08lx\n", new_blk.layer, new_blk.addr));
}
static inline void intl_alloc_blk(const xptr desc, xptr &new_blk)
{
	intl_alloc_blk(IS_DATA_BLOCK(desc), new_blk);
}

static inline void intl_alloc_string_block(bool persistent, const bool plog)
{
	xptr new_blk;
	intl_alloc_blk(persistent, new_blk);
	VMM_SIGNAL_MODIFICATION(new_blk);
	((pstr_long_blk_hdr*)XADDR(new_blk))->prev_blk = intl_last_blk;
	CHECKP(intl_last_blk);
	VMM_SIGNAL_MODIFICATION(intl_last_blk);
	((pstr_long_blk_hdr*)XADDR(intl_last_blk))->next_blk = new_blk;

	if (intl_last_blk != BLOCKXPTR(intl_ftr.start))
	{
		intl_ftr.block_list_size++;
		intl_last_block_list_entry--; //FIXME!!
		intl_last_block_list_entry->char_count = intl_ftr.char_count;
		intl_last_block_list_entry->str_blk	= intl_last_blk;
	}
	else
		intl_ftr.first_blk_char_count = intl_ftr.char_count;
	intl_ftr.char_count = 0;
	intl_last_blk = new_blk;
}

static inline char *intl_block_list_end_addr()
{
	return ((char *)tr_globals::e_string_buf + sizeof(tr_globals::e_string_buf));
}

static inline struct pstr_long_block_list_entry  *intl_block_list_end()
{
	return (struct pstr_long_block_list_entry*)intl_block_list_end_addr();
}

static inline void intl_set_empty_block_list()
{
	intl_ftr.block_list_size = 0;
	intl_last_block_list_entry = intl_block_list_end();
}

static inline struct pstr_long_block_list_map_entry *intl_map_end()
{
	return (struct pstr_long_block_list_map_entry *) ((char*)intl_map_buf + PAGE_SIZE);
}

static inline void intl_set_empty_map()
{
	intl_ftr.block_list_map_size = 0;
	intl_last_map_entry = intl_map_end();
}

static inline void intl_add_map_entry(const xptr list_blk, const int char_count)
{
	intl_last_map_entry--; //FIXME!!
	intl_last_map_entry->list_blk = list_blk;
	intl_last_map_entry->char_count = char_count;
	intl_ftr.block_list_map_size++;
}

/// move block list part to the last block-list block
///   returns size in bytes of moved part of the list
/// pre: intl_ftr.block_list_size >= 0
///     intl_ftr.pred_block_list_size, intl_last_block_list_block and intl_first_blb is correct
///     map is in the intl buffer
static inline int intl_move_block_list_part()
{
	int move_cnt = MIN(PSTR_LONG_FULL_BLOCK_LIST_SIZE - intl_ftr.pred_blb_size, intl_ftr.block_list_size);
	int move_size = 0;
	if (move_cnt > 0)
	{
		move_size = move_cnt * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
		CHECKP(intl_last_block_list_block);
		VMM_SIGNAL_MODIFICATION(intl_last_block_list_block);

		memcpy((char*)XADDR(intl_last_block_list_block) + PAGE_SIZE - (((int)intl_ftr.pred_blb_size)*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE) - move_size,
			intl_block_list_end_addr()-move_size, move_size);

		U_ASSERT(intl_ftr.block_list_map_size > 0);
		char * ptr = intl_block_list_end_addr()-move_size;
		for (int i = 0; i < move_cnt; i++)
		{
			intl_last_map_entry->char_count += ((pstr_long_block_list_entry *)ptr)->char_count;
			ptr += PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
		}

		memmove((char*)intl_last_block_list_entry + move_size,
			(char*)intl_last_block_list_entry,
			intl_ftr.block_list_size * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE - move_size);
		intl_ftr.block_list_size -= move_cnt;
		intl_ftr.pred_blb_size += move_cnt;

		intl_last_block_list_entry = (struct pstr_long_block_list_entry*)((char*)intl_last_block_list_entry + move_size);
	}
	return move_size;
}

static pstr_long_map_size_t intl_block_list_char_count()
{
	pstr_long_map_size_t cnt = 0;
	char *ptr = (char*)intl_last_block_list_entry;
	for (int i = 0; i < intl_ftr.block_list_size; i++)
	{
		cnt += ((pstr_long_block_list_entry*)ptr)->char_count;
		ptr += PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
	}

	return cnt;
}

//moves all of the block list in the buffer to block list blocks (updates map too)
//pre:		intl_ftr.pred_block_list_size, intl_last_block_list_block and intl_first_blb is correct
///			map is in the intl buffer
//updates:	map buffer
static inline void intl_move_list_to_blb(bool persistent)
{
	if (intl_ftr.pred_blb_size < PSTR_LONG_FULL_BLOCK_LIST_SIZE)
		intl_move_block_list_part();
	//FIXME - if block list is in 1 block & has a gap at the start - move it?
	if (intl_ftr.block_list_size == 0)
		return;

	xptr list_blk;
	intl_alloc_blk(persistent, list_blk);
	VMM_SIGNAL_MODIFICATION(list_blk);

	const int bl_size = intl_ftr.block_list_size * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
	memcpy((char*)XADDR(list_blk) + PAGE_SIZE - bl_size, intl_last_block_list_entry, bl_size);

	intl_add_map_entry(list_blk, intl_block_list_char_count());

	if (intl_first_blb == XNULL)
	{
		intl_first_blb = list_blk;
		intl_ftr.first_blb_gap_size = 0;
	}
	intl_last_block_list_block = list_blk;
	intl_ftr.pred_blb_size = intl_ftr.block_list_size;
	intl_set_empty_block_list();
}


//doesn't flush block list & map
//pre:  intl_ftr.cursor > 0, ...
//	    intl_last_blk is in memory & may be modified
//post: intl_last_blk is in memory & may be modified
static inline bool intl_append_str_pc(bool persistent, const char *data, pstr_long_off_t size, bool plog)
{
	U_ASSERT(intl_ftr.cursor > 0);
	int avail = PAGE_SIZE - intl_ftr.cursor;

	while (size > avail)
	{
		memcpy((char *)XADDR(intl_last_blk) + intl_ftr.cursor, data, avail);
		intl_ftr.char_count += intl_char_counter->count_chars(data, avail);
		data += avail;
		size -= avail;
		intl_alloc_string_block(persistent, plog);
		plog = false;
		intl_ftr.cursor = PSTR_LONG_BLK_HDR_SIZE;
		avail = PAGE_SIZE - intl_ftr.cursor;

		if (intl_ftr.block_list_size >= PSTR_LONG_FULL_BLOCK_LIST_SIZE)
			intl_move_list_to_blb(persistent);

		CHECKP(intl_last_blk);
		VMM_SIGNAL_MODIFICATION(intl_last_blk);
	}
	//size <= avail <= PAGE_SIZE here
	memcpy((char *)XADDR(intl_last_blk) + intl_ftr.cursor, data, (size_t)size);
	intl_ftr.cursor += (int)size;
	intl_ftr.char_count += intl_char_counter->count_chars(data, size);

	RECOVERY_CRASH;
	return plog;
}

//flushes block list and map buffers, may change intl_last_blk
//pre: intl_last_blk is in memory & may be modified
//	   there intl variables are set: intl_last_blk, intl_ftr, intl_last_ble, intl_last_me
static inline void intl_finalize_str(bool persistent, bool plog)
{
	//avail may be negative
	int avail = PAGE_SIZE - intl_ftr.cursor - PSTR_LONG_LAST_BLK_FTR_SIZE - (intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE);
	struct pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
	int x = MIN(PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE, intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE);
	if (x > avail)
	{
		intl_alloc_string_block(persistent, plog);
		plog = false;

		CHECKP(intl_last_blk);
		VMM_SIGNAL_MODIFICATION(intl_last_blk);
		ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
		intl_ftr.cursor = -intl_ftr.cursor;
		avail = PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE - PSTR_LONG_LAST_BLK_FTR_SIZE - (intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE);
	}
	((pstr_long_blk_hdr*)XADDR(intl_last_blk))->next_blk = XNULL;

	if (((int)intl_ftr.block_list_size)*(int)PSTR_LONG_BLOCK_LIST_ENTRY_SIZE > avail)
	{
		//TODO! - make sure that if cursor < 0, last str-block ref stays in last block
		intl_move_list_to_blb(persistent);

		avail -= PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
		//FIXME - check that last_block_max_textsize/avail/ravail isn't too small

		CHECKP(intl_last_blk);
		VMM_SIGNAL_MODIFICATION(intl_last_blk);
	}

	char *ptr = (char*)ftr;
	const int map_bytes = intl_ftr.block_list_map_size * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
	const int blocklist_bytes = intl_ftr.block_list_size * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;

	//TODO: use memcpy here

	ftr->start	= intl_ftr.start;
	ftr->char_count	= intl_ftr.char_count;
	ftr->cursor = intl_ftr.cursor;

	ftr->block_list_map_size = intl_ftr.block_list_map_size;
	ftr->block_list_size = intl_ftr.block_list_size;
	ftr->pred_blb_size = intl_ftr.pred_blb_size;
	ftr->first_blb_gap_size = intl_ftr.first_blb_gap_size;
	ftr->first_blk_char_count = intl_ftr.first_blk_char_count;

	ptr -= map_bytes;
	memcpy(ptr, (char *)intl_last_map_entry, map_bytes);
	ptr -= blocklist_bytes;
	memcpy(ptr, intl_last_block_list_entry, blocklist_bytes);
}

static xptr pstr_long_create_str2(bool persistent, const char *data, pstr_long_off_t size0)
{
	pstr_long_off_t size = size0;
	intl_alloc_blk(persistent, intl_last_blk);
	VMM_SIGNAL_MODIFICATION(intl_last_blk);
	intl_ftr.start = intl_last_blk + PSTR_LONG_BLK_HDR_SIZE;

	((pstr_long_blk_hdr*)XADDR(intl_last_blk))->prev_blk = XNULL;

	intl_ftr.block_list_map_size = 0;
	intl_last_map_entry = (struct pstr_long_block_list_map_entry *) ((char*)intl_map_buf + PAGE_SIZE);

	intl_set_empty_block_list();
	intl_char_counter = charset_handler->new_char_counter();
	intl_ftr.char_count = 0;
	intl_last_block_list_block = XNULL;
	intl_ftr.first_blb_gap_size = 0;
	intl_ftr.pred_blb_size = PSTR_LONG_FULL_BLOCK_LIST_SIZE;
	intl_first_blb = XNULL;

	intl_ftr.cursor = PSTR_LONG_BLK_HDR_SIZE;
	intl_append_str_pc(persistent, data, size, false);
	intl_finalize_str(persistent, false);

	charset_handler->free_char_counter(intl_char_counter);

	return intl_last_blk;
}

static xptr pstr_long_append_tail_estr2(const xptr str_ptr,const xptr data, pstr_long_off_t size);
static xptr pstr_long_append_tail2(const xptr str_ptr, const text_source_t src);
xptr pstr_long_create_str2(bool persistent, const text_source_t src)
{
	xptr str_ptr;
	switch (src.type)
	{
	case text_source_t::text_mem:
		str_ptr = pstr_long_create_str2(persistent, src.u.cstr, get_text_size(src));
		return str_ptr;
	case text_source_t::text_pstr:
		{
			char *tmp = (char*) malloc((size_t) get_text_size(src)); //FIXME? sb
			const xptr ptr= src.u.data;
			CHECKP(ptr);
			memcpy(tmp, (char*) XADDR(ptr), get_text_size(src));
			const xptr res = pstr_long_create_str2(persistent, tmp, get_text_size(src));
			free(tmp);
			return res;
		}
	case text_source_t::text_pstrlong:
		str_ptr = pstr_long_create_str2(persistent, "", 0);
		return pstr_long_append_tail2(str_ptr, src);
	case text_source_t::text_estr:
		str_ptr = pstr_long_create_str2(persistent, "", 0);
		str_ptr = pstr_long_append_tail_estr2(str_ptr, src.u.data, get_text_size(src));

		return str_ptr;
	}

	U_ASSERT(false);
	return XNULL;
}


xptr pstr_long_create_str(xptr desc, const text_source_t src)
{
	xptr str_ptr = pstr_long_create_str2(IS_DATA_BLOCK(desc), src);
	setNodePstrLongData(desc, str_ptr);
	return str_ptr;
}

//pre: CHECKP(intl_last_blk), footer is in intl_ftr
inline void intl_copy_map_to_buf()
{
	const int mapsize = intl_ftr.block_list_map_size * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
	intl_last_map_entry = (struct pstr_long_block_list_map_entry *) ((char*)intl_map_buf + PAGE_SIZE - mapsize);
	if (mapsize > 0)
		memcpy(intl_last_map_entry,
		(char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - mapsize,
		mapsize);
}

//delete block with log
static inline void intl_delete_blk(const xptr &blk)
{
	vmm_delete_block(blk);
	RECOVERY_CRASH;
	DBGBLOCKS_PRITNF(("delete_blk: 0x%08lx %08lx\n", blk.layer, blk.addr));
}

//pre:   intl_ftr, intl_blb, intl_map, intl_ftr.block_list_map_size > 0
static inline void intl_move_last_blb_to_buf()
{
	const xptr blb = intl_last_map_entry->list_blk;
	intl_last_map_entry++; //FIXME!!!!
	intl_ftr.block_list_map_size--;

	int gap, bll;
	bll = intl_ftr.pred_blb_size;
	intl_ftr.pred_blb_size = PSTR_LONG_FULL_BLOCK_LIST_SIZE;

	if (intl_ftr.block_list_map_size == 0)
		gap = intl_ftr.first_blb_gap_size;
	else
		gap = 0;


	intl_ftr.block_list_size = bll - gap;
	const int bls = intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
	intl_last_block_list_entry = (struct pstr_long_block_list_entry*)(intl_block_list_end_addr() - bls);

	CHECKP(blb);
	memcpy(intl_last_block_list_entry,
		(char*)XADDR(blb) + PAGE_SIZE - gap*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE - bls,
		bls);

	intl_delete_blk(blb);
}
void pstr_long_delete_str2(const xptr str_ptr)
{
	intl_last_blk = str_ptr;
	CHECKP(intl_last_blk);
	struct pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
	memcpy(&intl_ftr, ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);

	intl_copy_map_to_buf();
	int bls = intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
	intl_last_block_list_entry = (struct pstr_long_block_list_entry*)(intl_block_list_end_addr() - bls);
	memcpy(intl_last_block_list_entry,
		(char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE - bls,
		bls);

	intl_delete_blk(intl_last_blk);

	while (intl_ftr.block_list_size > 0)
	{
		const xptr blk = (intl_last_block_list_entry->str_blk);
		intl_last_block_list_entry++; //FIXME!!!!
		intl_ftr.block_list_size--;

		intl_delete_blk(blk);
	}

	while (intl_ftr.block_list_map_size > 0)
	{
		intl_move_last_blb_to_buf();

		while (intl_ftr.block_list_size > 0)
		{
			const xptr blk = (intl_last_block_list_entry->str_blk);
			intl_last_block_list_entry++; //FIXME!!!!
			intl_ftr.block_list_size--;

			intl_delete_blk(blk);
		}
	}

	return;
}
void pstr_long_delete_str(xptr desc)
{
    xptr intl_last_blk = getNodePstrLongData(desc);
    clearPstrLong(desc);

    pstr_long_delete_str2(intl_last_blk);
}

inline char * intl_last_blk_last_ble_addr(const int mapsize, const int blsize)
{
	return (char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - mapsize - blsize;
}

inline pstr_long_block_list_entry * intl_last_blk_last_ble(const int mapsize, const int blsize)
{
	return (pstr_long_block_list_entry *)intl_last_blk_last_ble_addr(mapsize, blsize);
}


static xptr pstr_long_append_tail_mem2(const xptr str_ptr,const char *data, str_off_t size0)
{
	U_ASSERT(size0 < SIZE_MAX);
	//TODO: move string first
	pstr_long_off_t size = size0;
	intl_last_blk = str_ptr;

	CHECKP(intl_last_blk);
	struct pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
	memcpy(&intl_ftr, ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);
	intl_char_counter = charset_handler->new_char_counter();

	if (intl_ftr.cursor < 0)
	{
		int avail = intl_ftr.cursor + PAGE_SIZE;
		int cursor = abs(intl_ftr.cursor);
		xptr prev = ((struct pstr_long_blk_hdr *)XADDR(intl_last_blk))->prev_blk;
		CHECKP(prev);
		VMM_SIGNAL_MODIFICATION(prev);
		if (avail >= size)
		{
			memcpy((char*)XADDR(prev) + cursor, data, (size_t)size);
			CHECKP(intl_last_blk);
			VMM_SIGNAL_MODIFICATION(intl_last_blk);
			if (avail == size)
				ftr->cursor = PSTR_LONG_BLK_HDR_SIZE;
			else
				ftr->cursor -= (int)size;
			//FIXME!!!
			ftr->char_count += intl_char_counter->count_chars(data, size);

			charset_handler->free_char_counter(intl_char_counter);
			return str_ptr;
		}
		else
		{
			memcpy((char*)XADDR(prev) + cursor, data, avail);
			intl_ftr.cursor = PSTR_LONG_BLK_HDR_SIZE;
			intl_ftr.char_count += intl_char_counter->count_chars(data, avail);
			data += avail;
			size -= avail;
		}
	}
	U_ASSERT(intl_ftr.cursor > 0);
	int avail = PAGE_SIZE - intl_ftr.cursor - (intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE) - PSTR_LONG_LAST_BLK_FTR_SIZE;
	int bls = intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
	if (avail >= size + bls)
	{
		CHECKP(intl_last_blk);
		VMM_SIGNAL_MODIFICATION(intl_last_blk);
		memcpy((char*)XADDR(intl_last_blk + intl_ftr.cursor), data, (size_t)size);
		intl_ftr.cursor += (int)size;
		intl_ftr.char_count += intl_char_counter->count_chars(data, size);

		ftr->cursor		= intl_ftr.cursor;
		ftr->char_count	= intl_ftr.char_count;

		charset_handler->free_char_counter(intl_char_counter);
		return str_ptr;
	}
	CHECKP(intl_last_blk);
	intl_last_block_list_entry = (struct pstr_long_block_list_entry*)(intl_block_list_end_addr() - bls);
	memcpy(intl_last_block_list_entry,
		intl_last_blk_last_ble_addr(intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE, bls),
		bls);

	intl_copy_map_to_buf();

	if (intl_ftr.block_list_map_size == 0)
	{
		intl_last_block_list_block = XNULL;
		intl_first_blb = XNULL;
	}
	else
	{
		intl_last_block_list_block = ((struct pstr_long_block_list_map_entry *)((char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - intl_ftr.block_list_map_size * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE))->list_blk;
		intl_first_blb = ((struct pstr_long_block_list_map_entry *)((char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE))->list_blk;
	}
	int lblb_avail = PSTR_LONG_FULL_BLOCK_LIST_SIZE - intl_ftr.pred_blb_size;

	//FIXME!!
	const int need_extra = MIN(PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE, intl_ftr.block_list_size > lblb_avail ? (intl_ftr.block_list_size - lblb_avail)*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE : 0);
	if (avail >= size + need_extra)
	{
		//it is possible to move block list to new block, so that string text will fit in the last block

		int move_size = intl_move_block_list_part();
		bls -= move_size;
		if (move_size > 0)
		{
			U_ASSERT(intl_ftr.block_list_map_size > 0);
			pstr_long_block_list_map_entry * const blk_me = (pstr_long_block_list_map_entry *)((char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - intl_ftr.block_list_map_size * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE);
			CHECKP(intl_last_blk);
			VMM_SIGNAL_MODIFICATION(intl_last_blk);
			blk_me->char_count = intl_last_map_entry->char_count;
		}
		if (avail < size + bls)
		{  //need to create a new block-list block

			const int mapsize = intl_ftr.block_list_map_size;
			intl_set_empty_map(); //we only need add one entry to our map
			intl_move_list_to_blb(IS_DATA_BLOCK(str_ptr));

			CHECKP(intl_last_blk);
			VMM_SIGNAL_MODIFICATION(intl_last_blk);

			intl_ftr.block_list_map_size = mapsize + 1;
			char * const dst = ((char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - intl_ftr.block_list_map_size * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE);
			memcpy(dst,
				intl_last_map_entry,
				PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE);
		}
		else
		{
			CHECKP(intl_last_blk);
			VMM_SIGNAL_MODIFICATION(intl_last_blk);
			U_ASSERT(move_size > 0);
			if (intl_ftr.block_list_size > 0)
			{
				char *ptr = (char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - (intl_ftr.block_list_map_size * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE);
				const int blocklist_bytes = intl_ftr.block_list_size * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
				ptr -= blocklist_bytes;
				memcpy(ptr, intl_last_block_list_entry, blocklist_bytes);
			}
		}
		U_ASSERT(size >= 0);
		memcpy((char*)XADDR(intl_last_blk + intl_ftr.cursor), data, (size_t)size);
		intl_ftr.cursor += (int)size;
		intl_ftr.char_count += intl_char_counter->count_chars(data, size);

		memcpy(ftr, &intl_ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);

		charset_handler->free_char_counter(intl_char_counter);
		return str_ptr;
	}

	//at this place block list & map are loaded to intl bufs
	U_ASSERT(intl_ftr.cursor > 0);
	CHECKP(intl_last_blk);
	VMM_SIGNAL_MODIFICATION(intl_last_blk);

	bool plog = intl_append_str_pc(IS_DATA_BLOCK(str_ptr), data, size, true); //FIXME: is plog really always true?
	intl_finalize_str(IS_DATA_BLOCK(str_ptr), plog);
	charset_handler->free_char_counter(intl_char_counter);

	return intl_last_blk;
}
//TODO: remove and use pstr_long_append_tail_mem2
static void pstr_long_append_tail_mem(const xptr desc,const char *data, pstr_long_off_t size0)
{
	xptr str_ptr = getNodePstrLongData(desc);
	str_ptr = pstr_long_append_tail_mem2(str_ptr, data, size0);
	setNodePstrLongData(desc, str_ptr);
}


static xptr pstr_long_append_tail_estr2(const xptr str_ptr,const xptr data, pstr_long_off_t size)
{
	//FIXME: this function is not tested
	char *tmp = (char*)malloc(PAGE_SIZE);
	xptr res = str_ptr;
	estr_cursor cur(data, size);
	int len;
	while ( (len = cur.copy_blk(tmp)) > 0)
		res = pstr_long_append_tail_mem2(res, tmp, len);
	return res;
}
static void pstr_long_append_tail_estr(const xptr desc,const xptr data, pstr_long_off_t size)
{
    xptr str_ptr = getNodePstrLongData(desc);
	str_ptr = pstr_long_append_tail_estr2(str_ptr, data, size);
    setNodePstrLongData(desc, str_ptr);
}

///////////////////////
/// pstr_long_cursor
///////////////////////

pstr_long_cursor::pstr_long_cursor(const xptr &_ptr_, int end_indicator)
{
	last_blk = _ptr_;
	CHECKP(last_blk);
	cursor = (PSTR_LONG_LAST_BLK_FTR(last_blk))->cursor;
	m_start = (PSTR_LONG_LAST_BLK_FTR(last_blk))->start;
	if (cursor > 0)
	{
		blk = last_blk;
		ofs = cursor;
	}
	else
	{
		blk = PSTR_LONG_BLK_HDR(last_blk)->prev_blk;
		ofs = -cursor;
	}
}


///pre: data_buf size >= PAGE_SIZE
size_t pstr_long_cursor::copy_blk(char *data_buf)
{
	if (blk == XNULL)
		return 0;
	if (blk != last_blk)
	{
		int data_buf_cnt;
		U_ASSERT(blk != XNULL);
		CHECKP(blk);
		if (((struct pstr_long_blk_hdr *)XADDR(blk))->next_blk == last_blk && cursor < 0)
		{
			memcpy(data_buf, (char*)XADDR(blk) + ofs, -cursor-ofs);
			data_buf_cnt = -cursor-ofs;
		}
		else
		{
			memcpy(data_buf, (char*)XADDR(blk) + ofs, PAGE_SIZE-ofs);
			data_buf_cnt = PAGE_SIZE-ofs;
		}
		blk = ((struct pstr_long_blk_hdr *)XADDR(blk))->next_blk;
		ofs = PSTR_LONG_BLK_HDR_SIZE;
		return data_buf_cnt;
	}
	else
	{
		if (cursor > 0)
		{
			CHECKP(blk);
			memcpy(data_buf, (char*)XADDR(blk) + ofs, cursor-ofs);
			blk = XNULL;
			return (cursor-ofs);
		}
		else
			return 0;
	}
}

size_t pstr_long_cursor::get_blk(char **ptr)
{
	//FIXME!!! blk shouldn't be XNULL
	if (blk == XNULL)
		return 0;
	if (blk != last_blk)
	{
		int data_buf_cnt;
		U_ASSERT(blk != XNULL);
		CHECKP(blk);
		if (((struct pstr_long_blk_hdr *)XADDR(blk))->next_blk == last_blk && cursor < 0)
		{
			*ptr = (char*)XADDR(blk) + ofs;
			data_buf_cnt = -cursor-ofs;
		}
		else
		{
			*ptr = (char*)XADDR(blk) + ofs;
			data_buf_cnt = PAGE_SIZE-ofs;
		}
		blk = ((struct pstr_long_blk_hdr *)XADDR(blk))->next_blk;
		ofs = PSTR_LONG_BLK_HDR_SIZE;
		return data_buf_cnt;
	}
	else
	{
		if (cursor > 0)
		{
			CHECKP(blk);
			*ptr = (char*)XADDR(blk) + ofs;
			blk = XNULL;
			return (cursor-ofs);
		}
		else
			return 0;
	}
}

int  pstr_long_cursor::get_blk_rev(char **ptr)
{
	U_ASSERT(blk != XNULL);

	if (blk != BLOCKXPTR(m_start))
	{
		int data_buf_cnt;
		CHECKP(blk);
		data_buf_cnt = ofs-PSTR_LONG_BLK_HDR_SIZE;
		*ptr = (char*)XADDR(blk) + PSTR_LONG_BLK_HDR_SIZE;
		blk = ((struct pstr_long_blk_hdr *)XADDR(blk))->prev_blk;
		U_ASSERT(blk != XNULL);
		ofs = PAGE_SIZE;

		if (data_buf_cnt == 0)
			return this->get_blk_rev(ptr); // (+ operator may move to next block)
		else
			return data_buf_cnt;
	}
	else
	{
		int data_buf_cnt = ofs;
		ofs = (char*)XADDR(m_start) - (char*)XADDR(blk);
		data_buf_cnt -= ofs;
		CHECKP(blk);
		*ptr = (char*)XADDR(m_start);
		return data_buf_cnt;
	}
}


//pre:	size0 is actual string size, FIXME
static xptr pstr_long_append_tail2(const xptr dst_str_ptr, const xptr src_str_ptr, pstr_long_off_t size0)
{
	U_ASSERT(size0 >= 0);
	U_TRACE(("dst_str_ptr.addr=%p, src_str_ptr.addr=%p, size0=%ld\n", dst_str_ptr.addr, src_str_ptr.addr, size0));
	if (size0 < (PAGE_SIZE * 2))
	{//FIXME?
		char *tmp_buf = (char*)malloc((size_t)size0); //FIXME: use static buf?
		pstr_long_copy_to_buffer2(tmp_buf, src_str_ptr, size0);
		xptr res = pstr_long_append_tail_mem2(dst_str_ptr, tmp_buf, size0);
		free(tmp_buf);
		return res;
	}
	else
	{
		//make sure cursor > 0, etc and use intl_append_str_pc

		//TODO: move string first
		pstr_long_off_t size = size0;
		intl_last_blk = dst_str_ptr;

		char *data_buf = (char *)malloc(PAGE_SIZE);
		int data_buf_cnt=0, data_buf_ofs=0;

		CHECKP(intl_last_blk);
		struct pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
		memcpy(&intl_ftr, ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);
		intl_char_counter = charset_handler->new_char_counter();

		pstr_long_cursor src_cur(src_str_ptr);

		if (intl_ftr.cursor < 0)
		{
			int avail = intl_ftr.cursor + PAGE_SIZE;
			int cursor = abs(intl_ftr.cursor);
			xptr prev = ((struct pstr_long_blk_hdr *)XADDR(intl_last_blk))->prev_blk;

			U_ASSERT(avail < size);

			while (avail > 0)
			{
				if (data_buf_ofs >= data_buf_cnt)
				{
					data_buf_cnt = src_cur.copy_blk(data_buf);
					data_buf_ofs = 0;
					U_ASSERT(data_buf_cnt > 0);
				}
				const int sz = MIN(avail, data_buf_cnt - data_buf_ofs);
				CHECKP(prev);
				VMM_SIGNAL_MODIFICATION(prev);
				memcpy((char*)XADDR(prev) + cursor, data_buf + data_buf_ofs, sz);
				intl_ftr.char_count += intl_char_counter->count_chars(data_buf+data_buf_ofs, sz);
				cursor += sz;
				data_buf_ofs += sz;
				avail -= sz;
				size -= sz;
			}

			intl_ftr.cursor = PSTR_LONG_BLK_HDR_SIZE;
		}
		U_ASSERT(intl_ftr.cursor > 0);
		int avail = PAGE_SIZE - intl_ftr.cursor - (intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE) - PSTR_LONG_LAST_BLK_FTR_SIZE;
		int bls = intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
		U_ASSERT(avail < size + bls);
		CHECKP(intl_last_blk);
		intl_last_block_list_entry = (struct pstr_long_block_list_entry*)(intl_block_list_end_addr() - bls);
		memcpy(intl_last_block_list_entry,
			intl_last_blk_last_ble_addr(intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE, bls),
			bls);

		if (intl_ftr.block_list_map_size == 0)
			intl_first_blb = XNULL;
		else
		{
			intl_last_block_list_block = ((struct pstr_long_block_list_map_entry *)((char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - intl_ftr.block_list_map_size * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE))->list_blk;
			intl_first_blb = ((struct pstr_long_block_list_map_entry *)((char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE))->list_blk;
		}
		int lblb_avail = PSTR_LONG_FULL_BLOCK_LIST_SIZE - intl_ftr.pred_blb_size;

		//FIXME!!
		const int need_extra = MIN(PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE, intl_ftr.block_list_size > lblb_avail ? (intl_ftr.block_list_size - lblb_avail)*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE : 0);
		U_ASSERT(avail < size + need_extra);

		//at this place: (ftr_buf.cursor > 0) is true and block list is loaded to intl buf
		CHECKP(intl_last_blk);

		intl_copy_map_to_buf();

		bool plog = true;

		if (data_buf_cnt > data_buf_ofs)
			plog = intl_append_str_pc(IS_DATA_BLOCK(dst_str_ptr), data_buf + data_buf_ofs, data_buf_cnt - data_buf_ofs, plog);
		//FIXME slow?
		while (size > 0)
		{
			U_ASSERT(src_cur.blk != XNULL);
			data_buf_cnt = src_cur.copy_blk(data_buf);
			if (data_buf_cnt > size)
			{
				data_buf_cnt = (int)size;
				U_ASSERT(dst_str_ptr == src_str_ptr);
			}
			data_buf_ofs = 0;
			CHECKP(intl_last_blk);
			VMM_SIGNAL_MODIFICATION(intl_last_blk);

			if (data_buf_cnt > data_buf_ofs)
				plog = intl_append_str_pc(IS_DATA_BLOCK(dst_str_ptr), data_buf + data_buf_ofs, data_buf_cnt - data_buf_ofs, plog);

			size -= data_buf_cnt;
		}
		U_ASSERT(size == 0);

		free(data_buf);
		//intl_append_str_pc will be executed at least once, because size0 >= PAGE_SIZE*2
		//  thus intl_last_blk is in mem & may be modified
		intl_finalize_str(IS_DATA_BLOCK(dst_str_ptr), plog);
		charset_handler->free_char_counter(intl_char_counter);

		return intl_last_blk;
	}
}

//pre: dst_desc != src_desc
void pstr_long_append_tail(const xptr dst_desc, const xptr src_desc)
{
	U_ASSERT(dst_desc != src_desc);
	CHECKP(src_desc);
	U_ASSERT(CommonTextNode(src_desc).isPstrLong());
	xptr src_ptr = getNodePstrLongData(src_desc);
	pstr_long_off_t src_size = pstr_long_bytelength2(src_ptr);
	xptr dst_ptr = getNodePstrLongData(dst_desc);

	dst_ptr = pstr_long_append_tail2(dst_ptr, src_ptr, src_size);

	setNodePstrLongData(dst_desc, dst_ptr);
//	T_DSC(dst_desc)->data.lsp.size += src_size;
}

//pre: size is actual string size for pstr_long strings
//FIXME: make pre cond - size <= actual stirng size for pstr_long strings
static xptr pstr_long_append_tail2(xptr str_ptr, const text_source_t src)
{
    switch (src.type)
    {
    case text_source_t::text_mem:
        return pstr_long_append_tail_mem2(str_ptr, src.u.cstr, get_text_size(src));
    case text_source_t::text_pstr:
        {
        const xptr ptr= src.u.data;
                char *tmp = (char*)malloc((size_t) get_text_size(src)); //FIXME? sb
                CHECKP(ptr);
                memcpy(tmp, (char*)XADDR(ptr), (size_t) get_text_size(src));
                str_ptr = pstr_long_append_tail_mem2(str_ptr, tmp, get_text_size(src));
                free(tmp);
                return str_ptr;
        }
    case text_source_t::text_pstrlong:
        {
                return pstr_long_append_tail2(str_ptr, src.u.data, get_text_size(src));
        }
    case text_source_t::text_estr:
            return pstr_long_append_tail_estr2(str_ptr, src.u.data, get_text_size(src));
    }
    U_ASSERT(false);
    return XNULL;
}

void pstr_long_append_tail(const xptr desc, const text_source_t src)
{
	xptr str_ptr = getNodePstrLongData(desc);
	str_ptr = pstr_long_append_tail2(str_ptr, src);
	setNodePstrLongData(desc, str_ptr);
}

///
/// pre: footer in intl_ftr, map and block_list in corr. buffers, 0 <= ofs <= str_size
/// returns:   (mapent == intl_map_end() && blb_ind == -1) iff ofs'th byte is in the first string-block
///            mapent == NULL iff ofs'th byte is in blb from the last_block/last_block and blb_ind != -1
///			   blb_ind == intl_ftr.block_list_size iff ofs'th byte is in the last_block and mapent != intl_map_end()
///            first block ind is 0 (or gap if it's first blb)
void intl_seek_byteofs(xptr &res, pstr_long_block_list_map_entry *&mapent, int &blb_ind, pstr_long_off_t ofs)
{
	pstr_long_off_t cur = 0;
	bool first_me = true;
	pstr_long_off_t sz;

	mapent = intl_map_end();
	sz = PAGE_SIZE - ((char*)XADDR(intl_ftr.start)-(char*)XADDR(BLOCKXPTR(intl_ftr.start)));
	if (cur + sz > ofs)
	{
		res = intl_ftr.start + (int)ofs;
		blb_ind = -1;
		return;
	}
	else
		cur += sz;

	//FIXME: this loop may be 'unrolled' to run in constant time
	while (mapent > intl_last_map_entry)
	{
		mapent--; //FIXME!!!!
		sz = 0;
		int gap = 0;
		if (first_me)
		{
			gap = intl_ftr.first_blb_gap_size;
			first_me = false;
		}
		int bll = PSTR_LONG_FULL_BLOCK_LIST_SIZE;
		if (mapent == intl_last_map_entry)
			bll = intl_ftr.pred_blb_size;

		sz = bll - gap;
		sz *= (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);

		if (cur + sz > ofs)
		{
			CHECKP(mapent->list_blk);
			//ind is actually < PSTR_LONG_FULL_BLOCK_LIST_SIZE, it's pstr_long_off_t only to avoid conversion in the next line
			const pstr_long_off_t ind = (ofs - cur)/(pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
			U_ASSERT(ind < PSTR_LONG_FULL_BLOCK_LIST_SIZE);
			cur += ind * (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
			int ble_ofs = PAGE_SIZE - ((int)ind + 1 + gap)*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
			pstr_long_block_list_entry *ble = (pstr_long_block_list_entry *)((char*)XADDR(mapent->list_blk) + ble_ofs);

			U_ASSERT(ofs - cur >= 0);
			U_ASSERT(ofs - cur < PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
			const int ofs_in_blk = (int)(ofs-cur) + PSTR_LONG_BLK_HDR_SIZE;

			res = ble->str_blk + ofs_in_blk;
			blb_ind = (int)ind + gap;

			return;
		}
		else
			cur += sz;
	}

	mapent = NULL;

	const pstr_long_off_t skip_fb = (ofs - cur)/(pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
	U_ASSERT(skip_fb < PSTR_LONG_FULL_BLOCK_LIST_SIZE);
	if (skip_fb < (pstr_long_off_t)intl_ftr.block_list_size)
	{
		cur += skip_fb * (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);

		pstr_long_block_list_entry *ble = (pstr_long_block_list_entry *)(intl_block_list_end_addr() - (((int)skip_fb + 1)*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE));
		U_ASSERT(ofs - cur >= 0);
		U_ASSERT(ofs - cur < PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
		const int ofs_in_blk = (int)(ofs-cur) + PSTR_LONG_BLK_HDR_SIZE;

		res = ble->str_blk + ofs_in_blk;
		blb_ind = (int)skip_fb;

		return;
	}
	sz = skip_fb * (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
	cur += sz;
	U_ASSERT((int)skip_fb == intl_ftr.block_list_size);
	U_ASSERT(intl_ftr.cursor > 0);
	U_ASSERT(ofs - cur >= 0);
	U_ASSERT(ofs - cur < PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
	const int ofs_in_blk = (int)(ofs-cur) + PSTR_LONG_BLK_HDR_SIZE;

	res = intl_last_blk + ofs_in_blk;
	blb_ind = (int)skip_fb;
}

static inline void intl_remove_last_ble()
{
	intl_last_block_list_entry++; //FIXME!!!!
	intl_ftr.block_list_size--;
}

//truncate size bytes from string end
void pstr_long_truncate(xptr desc, pstr_long_off_t size)
{
	U_TRACE(("dest.addr=%p, size=%ld\n", desc.addr, size));
	U_ASSERT(size >= 0);
	CHECKP(desc);
	intl_last_blk = getNodePstrLongData(desc);
	pstr_long_off_t trunc_ofs = pstr_long_bytelength2(intl_last_blk) - size;
	U_ASSERT(trunc_ofs > 0);

	CHECKP(intl_last_blk);
	struct pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
	memcpy(&intl_ftr, ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);

	intl_char_counter = charset_handler->new_char_counter();

	if (intl_ftr.cursor < 0)
	{
		int cursor = -intl_ftr.cursor;
		//TODO: check & remove this line - U_ASSERT(intl_ftr.block_list_size > 0);
		U_ASSERT((PSTR_LONG_BLK_HDR(intl_last_blk))->prev_blk != XNULL);
		if (cursor - PSTR_LONG_BLK_HDR_SIZE >= size)
		{
			intl_ftr.cursor += (int)size;
			xptr pred_blk = (PSTR_LONG_BLK_HDR(intl_last_blk))->prev_blk;

			int bs = intl_ftr.block_list_size; //FIXME: these 2 vars suxx
			int bs0 = intl_ftr.block_list_size;
			if (bs > 0)
				bs--;
			if (PAGE_SIZE + size >= cursor + (intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE) + ((bs)*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE) + PSTR_LONG_LAST_BLK_FTR_SIZE)
			{
				intl_copy_map_to_buf();

				if (intl_ftr.block_list_size > 0)
					intl_ftr.block_list_size--; //delete last block list element
				const int bls = intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
				const int mapsize = intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
				intl_last_block_list_entry = (struct pstr_long_block_list_entry*)(intl_block_list_end_addr() - bls);
				memcpy(intl_last_block_list_entry,
					intl_last_blk_last_ble_addr(mapsize, bls),
					bls);

				if (bs0 > 0)
				{
					intl_ftr.char_count = intl_last_blk_last_ble(mapsize, bls+PSTR_LONG_BLOCK_LIST_ENTRY_SIZE)->char_count;
					U_ASSERT(intl_last_blk_last_ble(mapsize, bls+PSTR_LONG_BLOCK_LIST_ENTRY_SIZE)->str_blk == pred_blk);
				}
				else
					intl_ftr.char_count = intl_ftr.first_blk_char_count;

				if (intl_ftr.block_list_map_size == 0)
				{
					intl_first_blb = XNULL;
					intl_last_block_list_block = XNULL;
				}
				else
				{
					intl_last_block_list_block = ((struct pstr_long_block_list_map_entry *)((char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - intl_ftr.block_list_map_size * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE))->list_blk;
					intl_first_blb = ((struct pstr_long_block_list_map_entry *)((char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE))->list_blk;
				}

				intl_delete_blk(intl_last_blk);

				const int map_start = PAGE_SIZE - PSTR_LONG_LAST_BLK_FTR_SIZE - mapsize;
				const int bl_start = map_start - bls;

				intl_ftr.cursor = -intl_ftr.cursor;
				U_ASSERT(intl_ftr.cursor == cursor - size);
				U_ASSERT(map_start >= cursor - size);

				intl_last_blk = pred_blk;

				CHECKP(intl_last_blk);
				VMM_SIGNAL_MODIFICATION(intl_last_blk);

				//update char counts
				const int cnt = intl_char_counter->count_chars((char*)XADDR(intl_last_blk) + intl_ftr.cursor, size);
				intl_ftr.char_count -= cnt;
				if (intl_last_blk == BLOCKXPTR(intl_ftr.start))
					intl_ftr.first_blk_char_count -= cnt;


				ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
				((pstr_long_blk_hdr*)XADDR(intl_last_blk))->next_blk = XNULL;

				memcpy((char*)XADDR(intl_last_blk) + bl_start, intl_last_block_list_entry, bls);
				memcpy((char*)XADDR(intl_last_blk) + map_start, intl_last_map_entry, mapsize);
				memcpy(ftr, &intl_ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);

	            setNodePstrLongData(desc, intl_last_blk);
//	            T_DSC(desc)->data.lsp.size -= size;

				charset_handler->free_char_counter(intl_char_counter);

				return;
			}
			else
			{
				//else just update charcount in last block since we can't delete it
				CHECKP(pred_blk);
				const int cnt = intl_char_counter->count_chars((char*)XADDR(pred_blk) + cursor - size, size);

				CHECKP(intl_last_blk);
				VMM_SIGNAL_MODIFICATION(intl_last_blk);
				ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
				const int bls = intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;

				pstr_long_block_list_entry *ent = intl_last_blk_last_ble(intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE, bls);
				U_ASSERT(ent->str_blk == pred_blk);

				ent->char_count -= cnt;
				ftr->cursor = intl_ftr.cursor;
				if (pred_blk == BLOCKXPTR(intl_ftr.start))
				{
					ftr->first_blk_char_count -= cnt;
				}

//	            WRITEP(desc);
//	            T_DSC(desc)->data.lsp.size -= size;

				charset_handler->free_char_counter(intl_char_counter);

				return;
			}
		} //if (cursor - PSTR_LONG_BLK_HDR_SIZE >= size)
	}
	else
		if (intl_ftr.cursor - PSTR_LONG_BLK_HDR_SIZE >= size)
		{
			CHECKP(intl_last_blk);
			VMM_SIGNAL_MODIFICATION(intl_last_blk);
			ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
			intl_ftr.cursor -= (int)size;
			const int cnt = intl_char_counter->count_chars((char*)XADDR(intl_last_blk) + intl_ftr.cursor, size);

			ftr->char_count -= cnt;
			ftr->cursor = intl_ftr.cursor;
			if (intl_last_blk == BLOCKXPTR(intl_ftr.start))
			{
				ftr->first_blk_char_count -= cnt;
			}

//            WRITEP(desc);
//            T_DSC(desc)->data.lsp.size -= size;

			charset_handler->free_char_counter(intl_char_counter);

			return;
		}

	intl_copy_map_to_buf();
	const int bls = intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
	const int mapsize = intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
	intl_last_block_list_entry = (struct pstr_long_block_list_entry*)(intl_block_list_end_addr() - bls);
	memcpy(intl_last_block_list_entry,
		intl_last_blk_last_ble_addr(mapsize, bls),
		bls);

	xptr trunc_from;
	pstr_long_block_list_map_entry *me;
	int bl_ind;
	intl_seek_byteofs(trunc_from, me, bl_ind, trunc_ofs);

	intl_delete_blk(intl_last_blk);

	if (me == NULL)
	{
		U_ASSERT(bl_ind >= 0);
		U_ASSERT(bl_ind < intl_ftr.block_list_size);//the case where trunc_ofs is in the last block is handled above

		while (intl_ftr.block_list_size > bl_ind + 1)
		{
			intl_delete_blk(intl_last_block_list_entry->str_blk);
			intl_remove_last_ble();
		}
		intl_last_blk = intl_last_block_list_entry->str_blk;
		intl_ftr.char_count = intl_last_block_list_entry->char_count;
		intl_remove_last_ble();
	}
	else
	{
		while (intl_ftr.block_list_size > 0)
		{
			intl_delete_blk(intl_last_block_list_entry->str_blk);
			intl_remove_last_ble();
		}

		while (intl_last_map_entry < me)
		{
			intl_move_last_blb_to_buf();
			while (intl_ftr.block_list_size > 0)
			{
				intl_delete_blk(intl_last_block_list_entry->str_blk);
				intl_remove_last_ble();
			}
		}
		U_ASSERT(intl_last_map_entry == me);

		if (me < intl_map_end())
		{
			intl_move_last_blb_to_buf();

			while (intl_ftr.block_list_size > bl_ind + 1)
			{
				intl_delete_blk(intl_last_block_list_entry->str_blk);
				intl_remove_last_ble();
			}
			intl_last_blk = intl_last_block_list_entry->str_blk;
			intl_ftr.char_count = intl_last_block_list_entry->char_count;
			intl_remove_last_ble();
		}
		else
		{
			intl_last_blk = BLOCKXPTR(intl_ftr.start);
			intl_ftr.char_count = intl_ftr.first_blk_char_count;
		}
	}
	U_ASSERT(intl_last_blk == BLOCKXPTR(trunc_from));

	intl_ftr.cursor = (char*)XADDR(trunc_from) - (char*)XADDR(BLOCKXPTR(trunc_from));

	CHECKP(intl_last_blk);
	VMM_SIGNAL_MODIFICATION(intl_last_blk);
	U_ASSERT(intl_last_blk + intl_ftr.cursor == trunc_from);
	const int cnt = intl_char_counter->count_chars((char*)XADDR(trunc_from), PAGE_SIZE - intl_ftr.cursor);

	intl_ftr.char_count -= cnt;
	if (intl_last_blk == BLOCKXPTR(intl_ftr.start))
		intl_ftr.first_blk_char_count -= cnt;



	intl_finalize_str(IS_DATA_BLOCK(desc), true);
	charset_handler->free_char_counter(intl_char_counter);

    setNodePstrLongData(desc, intl_last_blk);
//    T_DSC(desc)->data.lsp.size -= size;
}


static void pstr_long_append_head(xptr desc,const char *data, pstr_long_off_t size0)
{
	U_TRACE(("desc.addr=%p,data=%p,size0=%ld\n", desc.addr, data, size0));
	U_ASSERT(size0 >= 0);
	if (size0 == 0)
		return;

	pstr_long_off_t size = size0;
	CHECKP(desc);
	intl_last_blk = getNodePstrLongData(desc);

	CHECKP(intl_last_blk);
	struct pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
	memcpy(&intl_ftr, ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);
	xptr old_first_blb = XNULL;
	if (intl_ftr.block_list_map_size > 0)
		old_first_blb = ((struct pstr_long_block_list_map_entry *)((char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE))->list_blk;;
	intl_char_counter = charset_handler->new_char_counter();

	const xptr first_blk = BLOCKXPTR(intl_ftr.start);
	const int first_blk_avail = (char*)XADDR(intl_ftr.start) - (char*)XADDR(first_blk) - PSTR_LONG_BLK_HDR_SIZE;
	if (first_blk_avail >= size)
	{
		VMM_SIGNAL_MODIFICATION(intl_last_blk);

		//FIXME!: first_blk may also be last_blk
		intl_ftr.first_blk_char_count += intl_char_counter->count_chars(data, size);
		intl_ftr.start -= (int)size;
		ftr->start = intl_ftr.start;
		ftr->first_blk_char_count = intl_ftr.first_blk_char_count;

		CHECKP(first_blk);
		VMM_SIGNAL_MODIFICATION(first_blk);

		U_ASSERT(BLOCKXPTR(intl_ftr.start) == first_blk); // we changed ftr->start
		memcpy(XADDR(intl_ftr.start), data, (size_t)size);

//	    WRITEP(desc);
//	    U_ASSERT(isPstrLong(T_DSC(desc)));
//	    T_DSC(desc)->data.lsp.size += size;

		charset_handler->free_char_counter(intl_char_counter);
		return;
	}

	const pstr_long_off_t part12 = size - first_blk_avail;
	const pstr_long_off_t blk_count = (part12-1) / (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
	const int part1 = (int)(part12 - (blk_count * (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE)));

	//FIXME: make sure blk_count fits into int
	int new_blk_count = (int)(blk_count + 1);
	int next_bls = 0;
	const int fblb_avail = intl_ftr.first_blb_gap_size;
	if (new_blk_count > fblb_avail && intl_ftr.block_list_map_size > 0)
		next_bls = (new_blk_count - fblb_avail) % PSTR_LONG_FULL_BLOCK_LIST_SIZE;
	if (next_bls == 0)
		next_bls = PSTR_LONG_FULL_BLOCK_LIST_SIZE;


	//write part1
	U_ASSERT(part1 > 0);
	xptr new_first_blk;
	int new_first_blk_char_count;
	intl_alloc_blk(desc, new_first_blk);
	VMM_SIGNAL_MODIFICATION(new_first_blk);
	((pstr_long_blk_hdr*)XADDR(new_first_blk))->prev_blk = XNULL;
	intl_ftr.start = new_first_blk + (PAGE_SIZE - part1);
	memcpy(XADDR(intl_ftr.start), data, part1);
	new_first_blk_char_count = intl_char_counter->count_chars(data, part1);
	data += part1;
	size -= part1;

	int real_bls = intl_ftr.block_list_size;
	int real_maps = intl_ftr.block_list_map_size;
	intl_set_empty_block_list();
	intl_set_empty_map();
	xptr last_new_blk = new_first_blk;
	int new_gap = -1;
	int last_bls = -1;
	while (size > first_blk_avail)
	{
		const int chunk = PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE;
		xptr new_blk;
		intl_alloc_blk(desc, new_blk);
		VMM_SIGNAL_MODIFICATION(new_blk);
		((pstr_long_blk_hdr*)XADDR(new_blk))->prev_blk = last_new_blk;
		memcpy((char*)XADDR(new_blk) + PSTR_LONG_BLK_HDR_SIZE, data, chunk);

		CHECKP(last_new_blk);
		VMM_SIGNAL_MODIFICATION(last_new_blk);
		((pstr_long_blk_hdr*)XADDR(last_new_blk))->next_blk = new_blk;

		intl_ftr.block_list_size++;
		intl_last_block_list_entry--; //FIXME!!
		intl_last_block_list_entry->char_count = intl_char_counter->count_chars(data, chunk);
		intl_last_block_list_entry->str_blk	= new_blk;

		//we will surely need space here for the old first block
		if (intl_ftr.block_list_size >= next_bls)
		{
			U_ASSERT(intl_ftr.block_list_size == next_bls);
			if (new_gap == -1)
				new_gap = PSTR_LONG_FULL_BLOCK_LIST_SIZE - intl_ftr.block_list_size;
			last_bls = intl_ftr.block_list_size;

			xptr list_blk;
			intl_alloc_blk(desc, list_blk);
			VMM_SIGNAL_MODIFICATION(list_blk);

			const int bl_size = intl_ftr.block_list_size * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
			const int fbl_size = PSTR_LONG_FULL_BLOCK_LIST_SIZE * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
			memcpy((char*)XADDR(list_blk) + PAGE_SIZE - fbl_size, intl_last_block_list_entry, bl_size);

			intl_add_map_entry(list_blk, intl_block_list_char_count());

			intl_set_empty_block_list();
			next_bls = PSTR_LONG_FULL_BLOCK_LIST_SIZE;
		}

		data += chunk;
		size -= chunk;

		last_new_blk = new_blk;
	}

	CHECKP(last_new_blk);
	VMM_SIGNAL_MODIFICATION(last_new_blk);
	((pstr_long_blk_hdr*)XADDR(last_new_blk))->next_blk = first_blk;

	U_ASSERT(size == first_blk_avail);
	CHECKP(first_blk);
	VMM_SIGNAL_MODIFICATION(first_blk);
	((pstr_long_blk_hdr*)XADDR(first_blk))->prev_blk = last_new_blk;
	if (first_blk_avail > 0)
	{
		//no need to log? FIXME!!!
		memcpy((char*)XADDR(first_blk) + PSTR_LONG_BLK_HDR_SIZE, data, size);
		intl_ftr.first_blk_char_count += intl_char_counter->count_chars(data, size);
	}

	intl_ftr.block_list_size++;
	intl_last_block_list_entry--; //FIXME!!
	intl_last_block_list_entry->char_count = intl_ftr.first_blk_char_count;
	intl_last_block_list_entry->str_blk	= first_blk;

	if (intl_ftr.block_list_size >= next_bls)
	{
		U_ASSERT(intl_ftr.block_list_size == next_bls);
		U_ASSERT(intl_ftr.first_blb_gap_size == 0);

		if (new_gap == -1)
			new_gap = PSTR_LONG_FULL_BLOCK_LIST_SIZE - intl_ftr.block_list_size;
		last_bls = intl_ftr.block_list_size;

		xptr list_blk;
		intl_alloc_blk(desc, list_blk);
		VMM_SIGNAL_MODIFICATION(list_blk);

		const int bl_size = intl_ftr.block_list_size * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
		const int fbl_size = PSTR_LONG_FULL_BLOCK_LIST_SIZE * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
		memcpy((char*)XADDR(list_blk) + PAGE_SIZE - fbl_size, intl_last_block_list_entry, bl_size);

		intl_add_map_entry(list_blk, intl_block_list_char_count());

		intl_set_empty_block_list();
	}
	else if (intl_ftr.first_blb_gap_size > 0)
	{
		U_ASSERT(intl_ftr.first_blb_gap_size >= intl_ftr.block_list_size);
		U_ASSERT((intl_ftr.first_blb_gap_size == intl_ftr.block_list_size) || (new_gap == -1) );

		U_ASSERT(old_first_blb != XNULL);
		CHECKP(old_first_blb);

		VMM_SIGNAL_MODIFICATION(old_first_blb);

		const int bl_size = intl_ftr.block_list_size * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
		const int fbl_size = intl_ftr.first_blb_gap_size * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
		//FIXME - we need to plog it =(
		memcpy((char*)XADDR(old_first_blb) + PAGE_SIZE - fbl_size, intl_last_block_list_entry, bl_size);

		intl_ftr.first_blb_gap_size -= intl_ftr.block_list_size;

		//FIXME: may avoid CHECKP here by doing this later

		CHECKP(intl_last_blk);
		VMM_SIGNAL_MODIFICATION(intl_last_blk);

		U_ASSERT(real_maps > 0);
		pstr_long_block_list_map_entry *first_me = (pstr_long_block_list_map_entry *)((char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE);

		first_me->char_count += intl_block_list_char_count();

		intl_set_empty_block_list();
	}
	else
	{
		U_ASSERT(real_maps == 0);

		//TODO: merge block lists

		//FIXME: that's quick hack, see TODO above
		if (new_gap == -1)
			new_gap = 0;
		last_bls = intl_ftr.block_list_size;
		xptr list_blk;
		intl_alloc_blk(desc, list_blk);
		VMM_SIGNAL_MODIFICATION(list_blk);

		const int bl_size = intl_ftr.block_list_size * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
		memcpy((char*)XADDR(list_blk) + PAGE_SIZE - bl_size, intl_last_block_list_entry, bl_size);

		intl_add_map_entry(list_blk, intl_block_list_char_count());
		intl_set_empty_block_list();
	}

	U_ASSERT(intl_ftr.block_list_size == 0);
	U_ASSERT(ftr == PSTR_LONG_LAST_BLK_FTR(intl_last_blk));
	intl_ftr.first_blk_char_count = new_first_blk_char_count;

	CHECKP(intl_last_blk);
	VMM_SIGNAL_MODIFICATION(intl_last_blk);
	if (new_gap != -1)
	{
		U_ASSERT(last_bls != -1);
		U_ASSERT(intl_ftr.block_list_map_size > 0);
		if (real_maps == 0)
			intl_ftr.pred_blb_size = last_bls;
		intl_ftr.first_blb_gap_size = new_gap;

		const int mapsize_last_blk = real_maps * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
		const int mapsize_new = intl_ftr.block_list_map_size * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
		U_ASSERT((char*)intl_last_map_entry == (char*)intl_map_buf + PAGE_SIZE - mapsize_new);
		intl_last_map_entry = (struct pstr_long_block_list_map_entry *) ((char*)intl_map_buf + PAGE_SIZE - mapsize_new - mapsize_last_blk);
		if (mapsize_last_blk > 0)
			memcpy(intl_last_map_entry,
			(char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - mapsize_last_blk,
			mapsize_last_blk);
		intl_ftr.block_list_map_size += real_maps;

		const int bls = real_bls*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
		intl_last_block_list_entry = (struct pstr_long_block_list_entry*)(intl_block_list_end_addr() - bls);
		memcpy(intl_last_block_list_entry,
			intl_last_blk_last_ble_addr(mapsize_last_blk, bls),
			bls);
		intl_ftr.block_list_size = real_bls;
		intl_finalize_str(IS_DATA_BLOCK(desc), true);

        setNodePstrLongData(desc, intl_last_blk);
//        T_DSC(desc)->data.lsp.size += size0;

		charset_handler->free_char_counter(intl_char_counter);
		return;
	}
	U_ASSERT(intl_ftr.block_list_map_size == 0);
	intl_ftr.block_list_map_size = real_maps;
	intl_ftr.block_list_size = real_bls;

	memcpy(ftr, &intl_ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);

//    WRITEP(desc);
//    T_DSC(desc)->data.lsp.size += size0;

	charset_handler->free_char_counter(intl_char_counter);
}

void pstr_long_append_head(xptr desc, const text_source_t src)
{
	switch (src.type)
	{
	case text_source_t::text_mem:
		pstr_long_append_head(desc, src.u.cstr, get_text_size(src));
		return;
	case text_source_t::text_estr:
		{
			//FIXME!!!!!
			char *tmp = (char*)malloc((size_t) get_text_size(src)); //FIXME? sb
			estr_copy_to_buffer(tmp, src.u.data, get_text_size(src));
			pstr_long_append_head(desc, tmp, get_text_size(src));
			free(tmp);
			return;
		}
	case text_source_t::text_pstr:
		{
			char *tmp = (char*)malloc((size_t) get_text_size(src)); //FIXME? sb
			const xptr ptr = src.u.data;
			CHECKP(ptr);
			memcpy(tmp, (char*)XADDR(ptr), (size_t) get_text_size(src));
			pstr_long_append_head(desc, tmp, get_text_size(src));
			free(tmp);
			return;
		}
	case text_source_t::text_pstrlong:
		{
			//TODO!!! - test it
			char *tmp = (char*)malloc(PAGE_SIZE); //FIXME? sb
			const xptr ptr = src.u.data;
			pstr_long_cursor cur(ptr, true);
			char *strptr;
			int len;
			while ( (len = cur.get_blk_rev(&strptr))) //FIXME - make&use some sort of copy_blk_rev
			{
				memcpy(tmp, strptr, len);
				pstr_long_append_head(desc, tmp, len);
			}
			free(tmp);
			return;
		}
	}
	U_ASSERT(false);
}

void pstr_long_delete_head(xptr desc, pstr_long_off_t size)
{
	U_TRACE(("desc.addr=%p, size=%ld\n", desc.addr, size));
	U_ASSERT(size >= 0);
	CHECKP(desc);
	intl_last_blk = getNodePstrLongData(desc);
	U_ASSERT(size <= pstr_long_bytelength2(desc));

	CHECKP(intl_last_blk);
	struct pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
	memcpy(&intl_ftr, ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);

	intl_char_counter = charset_handler->new_char_counter();

	intl_copy_map_to_buf();
	const int bls = intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
	const int mapsize = intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
	intl_last_block_list_entry = (struct pstr_long_block_list_entry*)(intl_block_list_end_addr() - bls);
	memcpy(intl_last_block_list_entry,
		intl_last_blk_last_ble_addr(mapsize, bls),
		bls);

	xptr trunc_from;
	pstr_long_block_list_map_entry *me;
	int bl_ind;
	intl_seek_byteofs(trunc_from, me, bl_ind, size);

	if (bl_ind == -1)
	{
		U_ASSERT(BLOCKXPTR(trunc_from) == BLOCKXPTR(intl_ftr.start));
		U_ASSERT((char*)XADDR(trunc_from) - (char*)XADDR(intl_ftr.start) == size);

		CHECKP(intl_ftr.start);
		intl_ftr.first_blk_char_count -= intl_char_counter->count_chars((char*)XADDR(intl_ftr.start), size);
		intl_ftr.start = trunc_from;

		WRITEP(intl_last_blk);
		ftr->start = intl_ftr.start;
		ftr->first_blk_char_count = intl_ftr.first_blk_char_count;

//	    WRITEP(desc);
//	    T_DSC(desc)->data.lsp.size -= size;

		charset_handler->free_char_counter(intl_char_counter);
		return;
	}

	intl_delete_blk(BLOCKXPTR(intl_ftr.start));

	if (me == NULL)
	{
		pstr_long_block_list_map_entry *me_tmp;
		me_tmp = intl_map_end();
		me_tmp--; //FIXME
		int gap = intl_ftr.first_blb_gap_size; //we always start with the 1st blb
		while (intl_ftr.block_list_map_size > 0)
		{
			//DELETE map entry and all associated blocks
			const xptr blb = me_tmp->list_blk;
			int bls;
			if (intl_ftr.block_list_map_size == 1)
				bls = intl_ftr.pred_blb_size; //it's last blb
			else
				bls = PSTR_LONG_FULL_BLOCK_LIST_SIZE;

			char *cur_ent_addr = (char*)XADDR(blb) + PAGE_SIZE - (gap+1)*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
			while (bls > gap)
			{
				CHECKP(blb); //FIXME
				const xptr str_blk = ((pstr_long_block_list_entry*)cur_ent_addr)->str_blk;
				intl_delete_blk(str_blk);
				bls--;
				cur_ent_addr -= PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
			}

			intl_ftr.block_list_map_size--;
			me_tmp--; //FIXME
			gap = 0;

			intl_delete_blk(blb);
		}

		U_ASSERT(intl_ftr.block_list_map_size == 0);
		U_ASSERT(bl_ind <= intl_ftr.block_list_size);


		intl_ftr.first_blb_gap_size = 0;
		intl_ftr.pred_blb_size = PSTR_LONG_FULL_BLOCK_LIST_SIZE;
		char *cur_ent_addr = intl_block_list_end_addr();
		while (bl_ind > 0)
		{
			cur_ent_addr -= PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
			intl_delete_blk(((pstr_long_block_list_entry*)cur_ent_addr)->str_blk);
			intl_ftr.block_list_size--;
		}
		if (intl_ftr.block_list_size > 0)
		{
			cur_ent_addr -= PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
			U_ASSERT((((pstr_long_block_list_entry*)cur_ent_addr)->str_blk) == BLOCKXPTR(trunc_from));
			intl_ftr.first_blk_char_count = ((pstr_long_block_list_entry*)cur_ent_addr)->char_count;
			intl_ftr.block_list_size--;
		}
		else
		{
			U_ASSERT(intl_ftr.block_list_size == 0);
			U_ASSERT(BLOCKXPTR(trunc_from) == intl_last_blk);
			intl_ftr.first_blk_char_count = intl_ftr.char_count;
		}
	}
	else
	{
		pstr_long_block_list_map_entry *me_tmp;
		me_tmp = intl_map_end();
		me_tmp--; //FIXME
		int gap = intl_ftr.first_blb_gap_size; //we always start with the 1st blb
		while (me_tmp > me)
		{
			//DELETE map entry and all associated blocks
			const xptr blb = me_tmp->list_blk;
			int bls;
			if (intl_ftr.block_list_map_size == 1)
				bls = intl_ftr.pred_blb_size; //it's last blb
			else
				bls = PSTR_LONG_FULL_BLOCK_LIST_SIZE;

			char *cur_ent_addr = (char*)XADDR(blb) + PAGE_SIZE - (gap+1)*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
			while (bls > gap)
			{
				CHECKP(blb); //FIXME
				const xptr str_blk = ((pstr_long_block_list_entry*)cur_ent_addr)->str_blk;
				intl_delete_blk(str_blk);
				bls--;
				cur_ent_addr -= PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
			}

			intl_ftr.block_list_map_size--;
			me_tmp--; //FIXME
			gap = 0;

			intl_delete_blk(blb);
		}

		U_ASSERT(me < intl_map_end());
		U_ASSERT(intl_ftr.block_list_map_size > 0);
		U_ASSERT(me_tmp == me);
		const xptr blb = me_tmp->list_blk;
		int bls;
		if (intl_ftr.block_list_map_size == 1)
			bls = intl_ftr.pred_blb_size; //it's last blb
		else
			bls = PSTR_LONG_FULL_BLOCK_LIST_SIZE;
		char *cur_ent_addr = (char*)XADDR(blb) + PAGE_SIZE - (gap+1)*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
		while (bl_ind > gap)
		{
			CHECKP(blb);
			const xptr str_blk = ((pstr_long_block_list_entry*)cur_ent_addr)->str_blk;
			me->char_count -= ((pstr_long_block_list_entry*)cur_ent_addr)->char_count;
			intl_delete_blk(str_blk);
			cur_ent_addr -= PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
			gap++;
		}
		CHECKP(blb);
		U_ASSERT(((pstr_long_block_list_entry*)cur_ent_addr)->str_blk == BLOCKXPTR(trunc_from));
		intl_ftr.first_blk_char_count = ((pstr_long_block_list_entry*)cur_ent_addr)->char_count;
		me->char_count -= ((pstr_long_block_list_entry*)cur_ent_addr)->char_count;
		gap++;
		if (gap >= bls)
		{
			U_ASSERT(gap == bls);
			intl_delete_blk(blb);
			intl_ftr.block_list_map_size--;
			intl_ftr.first_blb_gap_size = 0; //we just deleted the first blb, so gap = 0 (even if map_size = 0)
			if (intl_ftr.block_list_map_size == 0)
				intl_ftr.pred_blb_size = PSTR_LONG_FULL_BLOCK_LIST_SIZE;
		}
		else
			intl_ftr.first_blb_gap_size = gap;
	}

	const char * strpart_start = (char *)XADDR(BLOCKXPTR(trunc_from)) + PSTR_LONG_BLK_HDR_SIZE;
	CHECKP(trunc_from);
	intl_ftr.first_blk_char_count -= intl_char_counter->count_chars(strpart_start, (char*)XADDR(trunc_from) - strpart_start);
	if (BLOCKXPTR(trunc_from) == intl_last_blk)
		intl_ftr.char_count = intl_ftr.first_blk_char_count;
	intl_ftr.start = trunc_from;

	CHECKP(intl_last_blk);
	VMM_SIGNAL_MODIFICATION(intl_last_blk);
	intl_finalize_str(IS_DATA_BLOCK(desc), true);

    setNodePstrLongData(desc, intl_last_blk);
//    T_DSC(desc)->data.lsp.size -= size;

	charset_handler->free_char_counter(intl_char_counter);
}

static void pstr_long_write_suffix(xptr start, se_ostream& crmout)
{
	//TODO
}

//TODO: rename these 2 functions
void pstr_long_feed2(xptr str,	string_consumer_fn fn, void *p)
{
	//TODO : use pstr_long_write_suffix
	intl_last_blk = str;

	CHECKP(intl_last_blk);
	xptr start = (PSTR_LONG_LAST_BLK_FTR(intl_last_blk))->start;
	xptr blk = BLOCKXPTR(start);
	intl_ftr.cursor = (PSTR_LONG_LAST_BLK_FTR(intl_last_blk))->cursor;
	int ofs = (char*)XADDR(start) - (char*)XADDR(blk);

	while (blk != intl_last_blk)
	{
		CHECKP(blk);
		xptr next_blk = ((struct pstr_long_blk_hdr *)XADDR(blk))->next_blk;
		if (next_blk == intl_last_blk && intl_ftr.cursor < 0)
			fn((char*)XADDR(blk) + ofs, -ofs-intl_ftr.cursor, p);
		else
			fn((char*)XADDR(blk) + ofs, PAGE_SIZE-ofs, p);

		blk = next_blk;
		ofs = PSTR_LONG_BLK_HDR_SIZE;
	}
	if (intl_ftr.cursor > 0)
	{
		CHECKP(blk);
		fn((char*)XADDR(blk) + ofs, intl_ftr.cursor-ofs, p);
	}
}
void pstr_long_feed(xptr desc,	string_consumer_fn fn, void *p)
{
	pstr_long_feed2(getNodePstrLongData(checkp(desc)), fn, p);
}

void pstr_long_copy_to_buffer2(char *buf, const xptr &str_ptr, pstr_long_off_t size)
{
	pstr_long_cursor cur(str_ptr);
	while (size > 0)
	{
		char *ptr;
		int cnt = cur.get_blk(&ptr);
		if (cnt == 0)
			return; //specified size is less than string size
		if (cnt > size)
			cnt = (int)size;
		memcpy(buf, ptr, cnt);
		buf += cnt;
		size -= cnt;
	}
}

void pstr_long_copy_to_buffer(char *buf, xptr desc)
{
	xptr pstr_blk = getNodePstrLongData(checkp(desc));
	pstr_long_copy_to_buffer2(buf, pstr_blk, pstr_long_bytelength2(pstr_blk));
}

pstr_long_off_t pstr_long_length(const xptr data)
{
	intl_last_blk = data;

	CHECKP(intl_last_blk);
	const pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);

	pstr_long_off_t cnt = ftr->char_count;

	if (BLOCKXPTR(ftr->start) != intl_last_blk)
		cnt += ftr->first_blk_char_count;

	char *ptr = (char *)ftr;

	for (int i = 0; i < ftr->block_list_map_size; i++)
	{
		ptr -= PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
		cnt += ((pstr_long_block_list_map_entry*)ptr)->char_count;
	}
	for (int i = 0; i < ftr->block_list_size; i++)
	{
		ptr -= PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
		cnt += ((pstr_long_block_list_entry*)ptr)->char_count;
	}

	return cnt;
}

pstr_long_off_t pstr_long_bytelength2(const xptr data)
{
	//TODO: there's no need to copy anything since we work with only one block
	intl_last_blk = data;

	CHECKP(intl_last_blk);
	struct pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
	memcpy(&intl_ftr, ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);

	//two cases when the whole string is in the first block (and we can't assuse it's filled upto its end)
	if (same_block(intl_ftr.start, intl_last_blk))
	{
		U_ASSERT(intl_ftr.cursor > 0);
		U_ASSERT(intl_ftr.cursor >= ((char*)XADDR(intl_ftr.start)-(char*)XADDR(BLOCKXPTR(intl_ftr.start))) );
		return intl_ftr.cursor - ((char*)XADDR(intl_ftr.start)-(char*)XADDR(BLOCKXPTR(intl_ftr.start)));
	}
	if (same_block(intl_ftr.start, ((struct pstr_long_blk_hdr *)XADDR(intl_last_blk))->prev_blk) && intl_ftr.cursor < 0)
	{
		return -intl_ftr.cursor - ((char*)XADDR(intl_ftr.start)-(char*)XADDR(BLOCKXPTR(intl_ftr.start)));
	}

	intl_copy_map_to_buf();
	const int bls = intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
	const int mapsize = intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
	intl_last_block_list_entry = (struct pstr_long_block_list_entry*)(intl_block_list_end_addr() - bls);
	memcpy(intl_last_block_list_entry,
		intl_last_blk_last_ble_addr(mapsize, bls),
		bls);

	pstr_long_off_t cur = 0;
	bool first_me = true;
	pstr_long_off_t sz;

	pstr_long_block_list_map_entry *mapent = intl_map_end();
	sz = PAGE_SIZE - ((char*)XADDR(intl_ftr.start)-(char*)XADDR(BLOCKXPTR(intl_ftr.start)));
	cur += sz;

	//FIXME: this loop may be 'unrolled' to run in constant time
	while (mapent > intl_last_map_entry)
	{
		mapent--; //FIXME!!!!
		sz = 0;
		int gap = 0;
		if (first_me)
		{
			gap = intl_ftr.first_blb_gap_size;
			first_me = false;
		}
		int bll = PSTR_LONG_FULL_BLOCK_LIST_SIZE;
		if (mapent == intl_last_map_entry)
			bll = intl_ftr.pred_blb_size;

		sz = bll - gap;
		sz *= (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);

		cur += sz;
	}

	const pstr_long_off_t skip_fb = intl_ftr.block_list_size;
	sz = skip_fb * (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
	cur += sz;

	if (intl_ftr.cursor >= 0)
	{
		U_ASSERT(intl_ftr.cursor >= PSTR_LONG_BLK_HDR_SIZE);
		cur += intl_ftr.cursor-PSTR_LONG_BLK_HDR_SIZE;
	}
	else
	{
		cur += -PSTR_LONG_BLK_HDR_SIZE-intl_ftr.cursor;
		cur -= PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE;
	}
	return cur;
}

#ifdef PSTR_LONG_TEST
void pstr_long_str_info(xptr desc)
{
	/*
	d_printf3("long_str: desc = 0x%08lx %08lx\n", desc.layer, desc.addr);
	CHECKP(desc);
	intl_last_blk = ((struct t_dsc *)XADDR(desc))->data;
	d_printf3("long_str: last_blk = 0x%08lx %08lx\n", intl_last_blk.layer, intl_last_blk.addr);
	d_printf2("long_str: bytesize = %ld\n", ((struct t_dsc *)XADDR(desc))->size);
	CHECKP(intl_last_blk);
	struct pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
	d_printf1("long_str: cursor=%ld\n", ftr->cursor);
	d_printf1("long_str: char_count=%ld\n", ftr->char_count);
	d_printf1("long_str: map_size=%ld\n", ftr->block_list_map_size);
	d_printf1("long_str: block_list_size=%ld\n", ftr->block_list_size);
	d_printf1("long_str: first_blb_gap_s=%ld\n", ftr->first_blb_gap_size);
	d_printf2("long_str: pred_blbs=%ld/%ld\n", ftr->pred_blb_size, PSTR_LONG_FULL_BLOCK_LIST_SIZE);
	d_printf2("long_str: first_blk_char_count=%ld\n", ftr->first_blk_char_count);
	d_printf2("long_str: length=%ld\n", pstr_long_length(desc));
	*/
}
void pstr_long_test_bytelength2()
{
	//TODO: use pstr_long_append_head too
	d_printf1("pstr_long_test_bytelength2: start\n");
	pstr_long_off_t sz = 3;
	static char buf[4444];
	srand(123);
	for (int i = 0; i < sizeof(buf); i++)
		buf[i] = 'a' + rand()%26;
	xptr str = pstr_long_create_str2(true, buf, 3);


	for (int i = 0; i < 10000; i++)
	{
		pstr_long_off_t _sz = pstr_long_bytelength2(str);
		if (sz != _sz)
		{
			d_printf3("pstr_long_test_bytelength2: err! "PRIi64" != "PRIi64"\n", sz, _sz);
			_sz = pstr_long_bytelength2(str); //for debug
		}
		else
		{
			d_printf2("pstr_long_test_bytelength2: ok, sz=%ld\n", sz);
		}
		if (rand()%11 == 0 && sz < 100000000)
		{
			str = pstr_long_append_tail2(str, str, sz);
			sz *= 2;
		}
		else
		{
			int len = (rand() % sizeof(buf)) + 1;
			str = pstr_long_append_tail2(str, buf, len, text_mem);
			sz += len;
		}
	}
}
#endif
