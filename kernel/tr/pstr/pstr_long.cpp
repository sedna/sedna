/*
 * File:  pstr_long.cpp
 * Copyright (C) 2005 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/vmm/vmm.h"
#include "tr/log/log.h"
#include "tr/strings/strings.h"
#include "tr/strings/e_string.h"
#include "tr/pstr/pstr_long.h"
#include "tr/structures/nodes.h"
#include "tr/pstr/pstr.h"
#include "common/errdbg/d_printf.h"


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

#ifndef MIN
#define MIN(a,b)		((a) < (b) ? (a) : (b))
#endif

static inline void intl_alloc_blk(const xptr desc, xptr &new_blk)
{
	if (IS_DATA_BLOCK(desc))
	{
		vmm_alloc_data_block(&new_blk);
		hl_phys_log_create_node_blk(XADDR(new_blk));
	}
	else
		vmm_alloc_tmp_block(&new_blk);

	DBGBLOCKS_PRITNF(("alloc_blk: 0x%08lx %08lx\n", new_blk.layer, new_blk.addr));
}

static inline void intl_alloc_string_block(const xptr desc, const bool plog)
{
	xptr new_blk;
	intl_alloc_blk(desc, new_blk);
	VMM_SIGNAL_MODIFICATION(new_blk);
	((pstr_long_blk_hdr*)XADDR(new_blk))->prev_blk = intl_last_blk;
	CHECKP(intl_last_blk);
	VMM_SIGNAL_MODIFICATION(intl_last_blk);
	if (plog && IS_DATA_BLOCK(intl_last_blk))
		hl_phys_log_change(&((pstr_long_blk_hdr*)XADDR(intl_last_blk))->next_blk, sizeof(xptr));
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
static inline void intl_move_list_to_blb(const xptr desc)
{
	if (intl_ftr.pred_blb_size < PSTR_LONG_FULL_BLOCK_LIST_SIZE)
		intl_move_block_list_part();
	//FIXME - if block list is in 1 block & has a gap at the start - move it?
	if (intl_ftr.block_list_size == 0)
		return;

	xptr list_blk;
	intl_alloc_blk(desc, list_blk);
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
static inline bool intl_append_str_pc(const xptr desc, const char *data, pstr_long_off_t size, bool plog)
{
	U_ASSERT(intl_ftr.cursor > 0);
	int avail = PAGE_SIZE - intl_ftr.cursor;

	while (size > avail)
	{
		if (plog && IS_DATA_BLOCK(intl_last_blk))
			hl_phys_log_change((char *)XADDR(intl_last_blk) + intl_ftr.cursor, avail);
		memcpy((char *)XADDR(intl_last_blk) + intl_ftr.cursor, data, avail);
		intl_ftr.char_count += intl_char_counter->count_chars(data, avail);
		data += avail;
		size -= avail;
		intl_alloc_string_block(desc, plog);
		plog = false;
		intl_ftr.cursor = PSTR_LONG_BLK_HDR_SIZE;
		avail = PAGE_SIZE - intl_ftr.cursor;

		if (intl_ftr.block_list_size >= PSTR_LONG_FULL_BLOCK_LIST_SIZE)
			intl_move_list_to_blb(desc);

		CHECKP(intl_last_blk);
		VMM_SIGNAL_MODIFICATION(intl_last_blk);
	}

	if (plog && IS_DATA_BLOCK(intl_last_blk))
		hl_phys_log_change((char *)XADDR(intl_last_blk) + intl_ftr.cursor, size);
	memcpy((char *)XADDR(intl_last_blk) + intl_ftr.cursor, data, size);
	intl_ftr.cursor += size;
	intl_ftr.char_count += intl_char_counter->count_chars(data, size);

	return plog;
}

//flushes block list and map buffers, may change intl_last_blk
//pre: intl_last_blk is in memory & may be modified
//	   there intl variables are set: intl_last_blk, intl_ftr, intl_last_ble, intl_last_me
static inline void intl_finalize_str(const xptr desc, bool plog)
{
	//avail may be negative
	int avail = PAGE_SIZE - intl_ftr.cursor - PSTR_LONG_LAST_BLK_FTR_SIZE - (intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE);
	struct pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
	int x = MIN(PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE, intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE);
	if (x > avail)
	{
		intl_alloc_string_block(desc, plog);
		plog = false;

		CHECKP(intl_last_blk);
		VMM_SIGNAL_MODIFICATION(intl_last_blk);
		ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
		intl_ftr.cursor = -intl_ftr.cursor;
		avail = PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE - PSTR_LONG_LAST_BLK_FTR_SIZE - (intl_ftr.block_list_map_size*PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE);
	}
	if (plog && IS_DATA_BLOCK(intl_last_blk) && (((pstr_long_blk_hdr*)XADDR(intl_last_blk))->next_blk != XNULL))
		hl_phys_log_change(&((pstr_long_blk_hdr*)XADDR(intl_last_blk))->next_blk, sizeof(xptr));
	((pstr_long_blk_hdr*)XADDR(intl_last_blk))->next_blk = XNULL;

	if (intl_ftr.block_list_size*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE > avail)
	{
		//TODO! - make sure that if cursor < 0, last str-block ref stays in last block
		intl_move_list_to_blb(desc);

		avail -= PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
		//FIXME - check that last_block_max_textsize/avail/ravail isn't too small

		CHECKP(intl_last_blk);
		VMM_SIGNAL_MODIFICATION(intl_last_blk);
	}

	char *ptr = (char*)ftr;
	const int map_bytes = intl_ftr.block_list_map_size * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE;
	const int blocklist_bytes = intl_ftr.block_list_size * PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
	if (plog && IS_DATA_BLOCK(intl_last_blk))
		hl_phys_log_change(ptr - (map_bytes + blocklist_bytes), map_bytes + blocklist_bytes + sizeof(*ftr));

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

static xptr pstr_long_create_str(xptr desc, const char *data, pstr_long_off_t size0)
{
	pstr_long_off_t size = size0;
	intl_alloc_blk(desc, intl_last_blk);
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
	intl_append_str_pc(desc, data, size, false);
	intl_finalize_str(desc, false);

	charset_handler->free_char_counter(intl_char_counter);

	CHECKP(desc);
	VMM_SIGNAL_MODIFICATION(desc);
	if (IS_DATA_BLOCK(desc))
	{
		hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
		hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->data, sizeof(((struct t_dsc *)XADDR(desc))->data));
	}
	((struct t_dsc *)XADDR(desc))->size = size0;
	((struct t_dsc *)XADDR(desc))->data = intl_last_blk;

	return intl_last_blk;
}

static void pstr_long_append_tail_estr(const xptr desc,const xptr data, pstr_long_off_t size0);
xptr pstr_long_create_str(xptr desc, const void *data, pstr_long_off_t size, text_type ttype)
{
	switch (ttype)
	{
	case text_mem:
		return pstr_long_create_str(desc, (char*)data, size);
	case text_doc:
		if (size <= PSTRMAXSIZE)
		{
			char *tmp = (char*)malloc(size); //FIXME? sb
			const xptr ptr=*(xptr*)data;
			CHECKP(ptr);
			memcpy(tmp, (char*)XADDR(ptr), size);
			const xptr res = pstr_long_create_str(desc, tmp, size);
			free(tmp);
			return res;
		}
		else
		{
			pstr_long_create_str(desc, "", 0);
			pstr_long_append_tail(desc, data, size, ttype);
			//FIXME  make append_tail return last_blk or use intl_last_blk here?
			CHECKP(desc);
			return ((struct t_dsc *)XADDR(desc))->data;
		}
	case text_estr:
		pstr_long_create_str(desc, "", 0);
		pstr_long_append_tail_estr(desc, *(xptr*)data, size);

		CHECKP(desc);
		return ((struct t_dsc *)XADDR(desc))->data;
	}

	//TODO! - exception/assert
	return XNULL;
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
	if (IS_DATA_BLOCK(blk))
		hl_phys_log_change_blk(XADDR(blk));
	vmm_delete_block(blk);
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

void pstr_long_delete_str(const xptr desc)
{
	CHECKP(desc);
	intl_last_blk = ((struct t_dsc *)XADDR(desc))->data;
	((struct t_dsc *)XADDR(desc))->data = XNULL;
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

	bool last_blb = true;
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

inline char * intl_last_blk_last_ble_addr(const int mapsize, const int blsize)
{
	return (char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - mapsize - blsize;
}

inline pstr_long_block_list_entry * intl_last_blk_last_ble(const int mapsize, const int blsize)
{
	return (pstr_long_block_list_entry *)intl_last_blk_last_ble_addr(mapsize, blsize);
}



static void pstr_long_append_tail_mem(const xptr desc,const char *data, pstr_long_off_t size0)
{
	//TODO: переносить сначала строку
	pstr_long_off_t size = size0;
	CHECKP(desc);
	intl_last_blk = ((struct t_dsc *)XADDR(desc))->data;

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
			memcpy((char*)XADDR(prev) + cursor, data, size);
			CHECKP(intl_last_blk);
			VMM_SIGNAL_MODIFICATION(intl_last_blk);
			if (IS_DATA_BLOCK(intl_last_blk))
			{
				hl_phys_log_change(&ftr->cursor, sizeof(ftr->cursor));
				hl_phys_log_change(&ftr->char_count, sizeof(ftr->char_count));
			}
			if (avail == size)
				ftr->cursor = PSTR_LONG_BLK_HDR_SIZE;
			else
				ftr->cursor -= size;
			//FIXME!!!
			ftr->char_count += intl_char_counter->count_chars(data, size);
			CHECKP(desc);
			VMM_SIGNAL_MODIFICATION(desc);
			if (IS_DATA_BLOCK(desc))
				hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
			((struct t_dsc *)XADDR(desc))->size += size;

			charset_handler->free_char_counter(intl_char_counter);
			return;
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
		memcpy((char*)XADDR(intl_last_blk + intl_ftr.cursor), data, size);
		intl_ftr.cursor += size;
		intl_ftr.char_count += intl_char_counter->count_chars(data, size);

		if (IS_DATA_BLOCK(desc))
		{
			hl_phys_log_change(&ftr->cursor, sizeof(ftr->cursor));
			hl_phys_log_change(&ftr->char_count, sizeof(ftr->char_count));
		}
		ftr->cursor		= intl_ftr.cursor;
		ftr->char_count	= intl_ftr.char_count;

		CHECKP(desc);
		VMM_SIGNAL_MODIFICATION(desc);
		if (IS_DATA_BLOCK(desc))
			hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
		((struct t_dsc *)XADDR(desc))->size += size0;

		charset_handler->free_char_counter(intl_char_counter);
		return;
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
			if (IS_DATA_BLOCK(desc))
				hl_phys_log_change(&blk_me->char_count, sizeof(blk_me->char_count));
			blk_me->char_count = intl_last_map_entry->char_count;
		}
		if (avail < size + bls)
		{  //need to create a new block-list block
			
			const int mapsize = intl_ftr.block_list_map_size;
			intl_set_empty_map(); //we only need add one entry to our map
			intl_move_list_to_blb(desc);

			CHECKP(intl_last_blk);
			VMM_SIGNAL_MODIFICATION(intl_last_blk);
			
			intl_ftr.block_list_map_size = mapsize + 1;
			char * const dst = ((char*)PSTR_LONG_LAST_BLK_FTR(intl_last_blk) - intl_ftr.block_list_map_size * PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE);
			if (IS_DATA_BLOCK(desc))
				hl_phys_log_change(dst, PSTR_LONG_BLOCK_LIST_MAP_ENTRY_SIZE);
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
				if (IS_DATA_BLOCK(intl_last_block_list_block))
					hl_phys_log_change(ptr, blocklist_bytes);
				memcpy(ptr, intl_last_block_list_entry, blocklist_bytes);
			}
		}
		//FIXME: log only part that intersects block list/map?
		if (IS_DATA_BLOCK(desc))
			hl_phys_log_change((char*)XADDR(intl_last_blk + intl_ftr.cursor), size);
		memcpy((char*)XADDR(intl_last_blk + intl_ftr.cursor), data, size);
		intl_ftr.cursor += size;
		intl_ftr.char_count += intl_char_counter->count_chars(data, size);

		if (IS_DATA_BLOCK(desc))
			hl_phys_log_change(ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);
		memcpy(ftr, &intl_ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);

		CHECKP(desc);
		VMM_SIGNAL_MODIFICATION(desc);
		if (IS_DATA_BLOCK(desc))
			hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
		((struct t_dsc *)XADDR(desc))->size += size0;

		charset_handler->free_char_counter(intl_char_counter);
		return;
	}

	//at this place block list & map are loaded to intl bufs
	U_ASSERT(intl_ftr.cursor > 0);
	CHECKP(intl_last_blk);
	VMM_SIGNAL_MODIFICATION(intl_last_blk);

	bool plog = intl_append_str_pc(desc, data, size, true); //FIXME: is plog really always true?
	intl_finalize_str(desc, plog);
	charset_handler->free_char_counter(intl_char_counter);

	CHECKP(desc);
	VMM_SIGNAL_MODIFICATION(desc);
	if (IS_DATA_BLOCK(desc))
	{
		hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
		hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->data, sizeof(((struct t_dsc *)XADDR(desc))->data));
	}
	((struct t_dsc *)XADDR(desc))->size += size0;
	((struct t_dsc *)XADDR(desc))->data = intl_last_blk;
}

static void pstr_long_append_tail_estr(const xptr desc,const xptr data, pstr_long_off_t size)
{
	//FIXME: this function is not tested
	char *tmp = (char*)malloc(PAGE_SIZE);
	estr_cursor cur(data, size);
	int len;
	while ( (len = cur.copy_blk(tmp)) > 0)
		pstr_long_append_tail_mem(desc, tmp, len);
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
int  pstr_long_cursor::copy_blk(char *data_buf)
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
			memcpy(data_buf, (char*)XADDR(blk) + ofs, -ofs-cursor);
			data_buf_cnt = -ofs-cursor;
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

int  pstr_long_cursor::get_blk(char **ptr)
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
			data_buf_cnt = -ofs-cursor;
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
void pstr_long_append_tail(const xptr dst_desc, const xptr src, pstr_long_off_t size0)
{
	U_TRACE(("dst_desc.addr=%p, src.addr=%p, size0=%ld\n", dst_desc.addr, src.addr, size0));
	if (size0 < (PAGE_SIZE * 2))
	{//FIXME?
		char *tmp_buf = (char*)malloc(size0); //FIXME: use static buf?
		pstr_long_copy_to_buffer(tmp_buf, src, size0);
		pstr_long_append_tail_mem(dst_desc, tmp_buf, size0);
		free(tmp_buf);
	}
	else
	{
		//make sure cursor > 0, etc and use intl_append_str_pc

		//TODO: переносить сначала строку
		pstr_long_off_t size = size0;
		CHECKP(dst_desc);
		intl_last_blk = ((struct t_dsc *)XADDR(dst_desc))->data;

		char *data_buf = (char *)malloc(PAGE_SIZE);
		int data_buf_cnt=0, data_buf_ofs=0;

		CHECKP(intl_last_blk);
		struct pstr_long_last_blk_ftr *ftr = PSTR_LONG_LAST_BLK_FTR(intl_last_blk);
		memcpy(&intl_ftr, ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);
		intl_char_counter = charset_handler->new_char_counter();

		const xptr src_last_blk = src;

		CHECKP(src_last_blk);
		xptr start = (PSTR_LONG_LAST_BLK_FTR(src_last_blk))->start;

		pstr_long_cursor src_cur(start, src_last_blk);

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
			plog = intl_append_str_pc(dst_desc, data_buf + data_buf_ofs, data_buf_cnt - data_buf_ofs, plog);
		//FIXME slow?
		while (size > 0)
		{
			U_ASSERT(src_cur.blk != XNULL);
			data_buf_cnt = src_cur.copy_blk(data_buf);
			if (data_buf_cnt > size)
			{
				data_buf_cnt = size;
				U_ASSERT(((struct t_dsc *)XADDR(dst_desc))->data == src);
			}
			data_buf_ofs = 0;
			CHECKP(intl_last_blk);
			VMM_SIGNAL_MODIFICATION(intl_last_blk);

			if (data_buf_cnt > data_buf_ofs)
				plog = intl_append_str_pc(dst_desc, data_buf + data_buf_ofs, data_buf_cnt - data_buf_ofs, plog);

			size -= data_buf_cnt;
		}
		U_ASSERT(size == 0);

		free(data_buf);
		//intl_append_str_pc will be executed at least once, because size0 >= PAGE_SIZE*2
		//  thus intl_last_blk is in mem & may be modified
		intl_finalize_str(dst_desc, plog);
		charset_handler->free_char_counter(intl_char_counter);

		CHECKP(dst_desc);
		VMM_SIGNAL_MODIFICATION(dst_desc);
		if (IS_DATA_BLOCK(dst_desc))
		{
			hl_phys_log_change(&((struct t_dsc *)XADDR(dst_desc))->size, sizeof(((struct t_dsc *)XADDR(dst_desc))->size));
			hl_phys_log_change(&((struct t_dsc *)XADDR(dst_desc))->data, sizeof(((struct t_dsc *)XADDR(dst_desc))->data));
		}
		((struct t_dsc *)XADDR(dst_desc))->size += size0;
		((struct t_dsc *)XADDR(dst_desc))->data = intl_last_blk;
	}
}

//pre: dst_desc != src_desc
void pstr_long_append_tail(const xptr dst_desc, const xptr src_desc)
{
	U_ASSERT(dst_desc != src_desc);
	pstr_long_append_tail(dst_desc, ((struct t_dsc *)XADDR(src_desc))->data,  ((struct t_dsc *)XADDR(src_desc))->size);
}

//pre: size is actual string size for pstr_long strings
//FIXME: make pre cond - size <= actual stirng size for pstr_long strings
void pstr_long_append_tail(const xptr desc,const void *data, pstr_long_off_t size, text_type ttype)
{
	switch (ttype)
	{
	case text_mem:
		pstr_long_append_tail_mem(desc, (char*)data, size);
		return;
	case text_doc:
		if (size <= PSTRMAXSIZE)
		{
			char *tmp = (char*)malloc(size); //FIXME? sb
			const xptr ptr=*(xptr*)data;
			CHECKP(ptr);
			memcpy(tmp, (char*)XADDR(ptr), size);
			pstr_long_append_tail_mem(desc, tmp, size);
			free(tmp);
			return;
		}
		else
		{
			const xptr ptr=*(xptr*)data;
			pstr_long_append_tail(desc, ptr, size);
			return;
		}
	case text_estr:
		pstr_long_append_tail_estr(desc, *(xptr*)data, size);
		return;
	}
	//TODO - error
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
			const pstr_long_off_t ind = (ofs - cur)/(pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
			cur += ind * (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
			int ble_ofs = PAGE_SIZE - ((int)ind + 1 + gap)*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE;
			pstr_long_block_list_entry *ble = (pstr_long_block_list_entry *)((char*)XADDR(mapent->list_blk) + ble_ofs);

			const int ofs_in_blk = (int)(ofs-cur) + PSTR_LONG_BLK_HDR_SIZE;
			U_ASSERT(ofs_in_blk > 0);
			U_ASSERT(ofs_in_blk < PAGE_SIZE);
			
			res = ble->str_blk + ofs_in_blk;
			blb_ind = ind + gap;

			return;
		}
		else
			cur += sz;
	}

	mapent = NULL;

	const pstr_long_off_t skip_fb = (ofs - cur)/(pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
	if ((int)skip_fb < intl_ftr.block_list_size)
	{
		cur += skip_fb * (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);

		pstr_long_block_list_entry *ble = (pstr_long_block_list_entry *)(intl_block_list_end_addr() - (((int)skip_fb + 1)*PSTR_LONG_BLOCK_LIST_ENTRY_SIZE));
		const int ofs_in_blk = (int)(ofs-cur) + PSTR_LONG_BLK_HDR_SIZE;
		U_ASSERT(ofs_in_blk > 0);
		U_ASSERT(ofs_in_blk < PAGE_SIZE);

		res = ble->str_blk + ofs_in_blk;
		blb_ind = skip_fb;

		return;
	}
	sz = skip_fb * (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
	cur += sz;
	U_ASSERT((int)skip_fb == intl_ftr.block_list_size);
	U_ASSERT(intl_ftr.cursor > 0);
	const int ofs_in_blk = (int)(ofs-cur) + PSTR_LONG_BLK_HDR_SIZE;
	U_ASSERT(ofs_in_blk > 0);
	U_ASSERT(ofs_in_blk < PAGE_SIZE);

	res = intl_last_blk + ofs_in_blk;
	blb_ind = skip_fb;
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
	intl_last_blk = ((struct t_dsc *)XADDR(desc))->data;
	pstr_long_off_t trunc_ofs = ((struct t_dsc *)XADDR(desc))->size - size;
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
			intl_ftr.cursor += size;
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
				if (IS_DATA_BLOCK(intl_last_blk))
				{
					hl_phys_log_change(&((pstr_long_blk_hdr*)XADDR(intl_last_blk))->next_blk, sizeof(((pstr_long_blk_hdr*)XADDR(intl_last_blk))->next_blk));
					//cursor is OLD value of ftr->cursor, log this part: [bl_start, cursor)
					if (bl_start < cursor)
						hl_phys_log_change((char*)XADDR(intl_last_blk) + bl_start, cursor - bl_start);
				}
				((pstr_long_blk_hdr*)XADDR(intl_last_blk))->next_blk = XNULL;

				memcpy((char*)XADDR(intl_last_blk) + bl_start, intl_last_block_list_entry, bls);
				memcpy((char*)XADDR(intl_last_blk) + map_start, intl_last_map_entry, mapsize);
				memcpy(ftr, &intl_ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);

				CHECKP(desc);
				VMM_SIGNAL_MODIFICATION(desc);
				if (IS_DATA_BLOCK(desc))
				{
					hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
					hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->data, sizeof(((struct t_dsc *)XADDR(desc))->data));
				}
				((struct t_dsc *)XADDR(desc))->size -= size;
				((struct t_dsc *)XADDR(desc))->data = intl_last_blk;

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

				if (IS_DATA_BLOCK(intl_last_blk))
				{
					hl_phys_log_change(&ent->char_count, sizeof(ent->char_count));
					hl_phys_log_change(&ftr->cursor, sizeof(ftr->cursor));
				}
				ent->char_count -= cnt;
				ftr->cursor = intl_ftr.cursor;
				if (pred_blk == BLOCKXPTR(intl_ftr.start))
				{
					if (IS_DATA_BLOCK(intl_last_blk))
						hl_phys_log_change(&ftr->first_blk_char_count, sizeof(ftr->first_blk_char_count));
					ftr->first_blk_char_count -= cnt;
				}

				CHECKP(desc);
				VMM_SIGNAL_MODIFICATION(desc);
				if (IS_DATA_BLOCK(desc))
					hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
				((struct t_dsc *)XADDR(desc))->size -= size;

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
			intl_ftr.cursor -= size;
			const int cnt = intl_char_counter->count_chars((char*)XADDR(intl_last_blk) + intl_ftr.cursor, size);
			
			if (IS_DATA_BLOCK(intl_last_blk))
			{
				hl_phys_log_change(&ftr->char_count, sizeof(ftr->char_count));
				hl_phys_log_change(&ftr->cursor, sizeof(ftr->cursor));
			}
			ftr->char_count -= cnt;
			ftr->cursor = intl_ftr.cursor;
			if (intl_last_blk == BLOCKXPTR(intl_ftr.start))
			{
				if (IS_DATA_BLOCK(intl_last_blk))
					hl_phys_log_change(&ftr->first_blk_char_count, sizeof(ftr->first_blk_char_count));
				ftr->first_blk_char_count -= cnt;
			}

			CHECKP(desc);
			VMM_SIGNAL_MODIFICATION(desc);
			if (IS_DATA_BLOCK(desc))
				hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
			((struct t_dsc *)XADDR(desc))->size -= size;

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

	

	intl_finalize_str(desc, true);
	charset_handler->free_char_counter(intl_char_counter);

	CHECKP(desc);
	VMM_SIGNAL_MODIFICATION(desc);
	if (IS_DATA_BLOCK(desc))
	{
		hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
		hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->data, sizeof(((struct t_dsc *)XADDR(desc))->data));
	}
	((struct t_dsc *)XADDR(desc))->size -= size;
	((struct t_dsc *)XADDR(desc))->data = intl_last_blk;
}


static void pstr_long_append_head(xptr desc,const char *data, pstr_long_off_t size0)
{
	U_TRACE(("desc.addr=%p,data=%p,size0=%ld\n", desc.addr, data, size0));
	U_ASSERT(size0 >= 0);
	if (size0 == 0)
		return;

	pstr_long_off_t size = size0;
	CHECKP(desc);
	intl_last_blk = ((struct t_dsc *)XADDR(desc))->data;

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
		intl_ftr.start -= size;
		if (IS_DATA_BLOCK(intl_last_blk))
		{
			hl_phys_log_change(&ftr->start, sizeof(ftr->start));
			hl_phys_log_change(&ftr->first_blk_char_count, sizeof(ftr->first_blk_char_count));
		}
		ftr->start = intl_ftr.start;
		ftr->first_blk_char_count = intl_ftr.first_blk_char_count;

		CHECKP(first_blk);
		VMM_SIGNAL_MODIFICATION(first_blk);
		//we don't overwrite data => no phys-log
		U_ASSERT(BLOCKXPTR(intl_ftr.start) == first_blk); // we changed ftr->start
		memcpy(XADDR(intl_ftr.start), data, size);

		CHECKP(desc);
		VMM_SIGNAL_MODIFICATION(desc);
		if (IS_DATA_BLOCK(desc))
			hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
		((struct t_dsc *)XADDR(desc))->size += size;

		charset_handler->free_char_counter(intl_char_counter);
		return;
	}

	const pstr_long_off_t part12 = size - first_blk_avail;
	const pstr_long_off_t blk_count = (part12-1) / (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE);
	const int part1 = (int)(part12 - (blk_count * (pstr_long_off_t)(PAGE_SIZE - PSTR_LONG_BLK_HDR_SIZE)));

	int new_blk_count = blk_count + 1;
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
	if (IS_DATA_BLOCK(first_blk))
		hl_phys_log_change(&((pstr_long_blk_hdr*)XADDR(first_blk))->prev_blk, sizeof(xptr));
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

		if (IS_DATA_BLOCK(intl_last_blk))
			hl_phys_log_change(&first_me->char_count, sizeof(first_me->char_count));
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
		intl_finalize_str(desc, true);

		CHECKP(desc);
		VMM_SIGNAL_MODIFICATION(desc);
		if (IS_DATA_BLOCK(desc))
		{
			hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
			hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->data, sizeof(((struct t_dsc *)XADDR(desc))->data));
		}
		((struct t_dsc *)XADDR(desc))->size += size0;
		((struct t_dsc *)XADDR(desc))->data = intl_last_blk;

		charset_handler->free_char_counter(intl_char_counter);
		return;
	}
	U_ASSERT(intl_ftr.block_list_map_size == 0);
	intl_ftr.block_list_map_size = real_maps;
	intl_ftr.block_list_size = real_bls;

	if (IS_DATA_BLOCK(intl_last_blk))
		hl_phys_log_change(ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);
	memcpy(ftr, &intl_ftr, PSTR_LONG_LAST_BLK_FTR_SIZE);

	CHECKP(desc);
	VMM_SIGNAL_MODIFICATION(desc);
	if (IS_DATA_BLOCK(desc))
		hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
	((struct t_dsc *)XADDR(desc))->size += size0;

	charset_handler->free_char_counter(intl_char_counter);
}

void pstr_long_append_head(xptr desc,const void *data, pstr_long_off_t size, text_type ttype)
{
	switch (ttype)
	{
	case text_mem:
		pstr_long_append_head(desc, (char*)data, size);
		return;
	case text_estr:
		{
			//FIXME!!!!!
			char *tmp = (char*)malloc(size); //FIXME? sb
			estr_copy_to_buffer(tmp, *(xptr*)data, size);
			pstr_long_append_head(desc, tmp, size);
			free(tmp);
			return;
		}
	case text_doc:
		if (size <= PSTRMAXSIZE)
		{
			char *tmp = (char*)malloc(size); //FIXME? sb
			const xptr ptr=*(xptr*)data;
			CHECKP(ptr);
			memcpy(tmp, (char*)XADDR(ptr), size);
			pstr_long_append_head(desc, tmp, size);
			free(tmp);
			return;
		}
		else
		{
			//TODO!!! - test it
			char *tmp = (char*)malloc(PAGE_SIZE); //FIXME? sb
			const xptr ptr=*(xptr*)data;
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
	//TODO - error
}

void pstr_long_delete_head(xptr desc, pstr_long_off_t size)
{	
	U_TRACE(("desc.addr=%p, size=%ld\n", desc.addr, size));
	U_ASSERT(size >= 0);
	CHECKP(desc);
	intl_last_blk = ((struct t_dsc *)XADDR(desc))->data;
	U_ASSERT(size <= ((struct t_dsc *)XADDR(desc))->size);

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

		CHECKP(intl_last_blk);
		VMM_SIGNAL_MODIFICATION(intl_last_blk);
		if (IS_DATA_BLOCK(intl_last_blk))
		{
			hl_phys_log_change(&ftr->start, sizeof(ftr->start));
			hl_phys_log_change(&ftr->first_blk_char_count, sizeof(ftr->first_blk_char_count));
		}
		ftr->start = intl_ftr.start;
		ftr->first_blk_char_count = intl_ftr.first_blk_char_count;

		CHECKP(desc);
		VMM_SIGNAL_MODIFICATION(desc);
		if (IS_DATA_BLOCK(desc))
			hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
		((struct t_dsc *)XADDR(desc))->size -= size;

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
	intl_finalize_str(desc, true);

	CHECKP(desc);
	VMM_SIGNAL_MODIFICATION(desc);
	if (IS_DATA_BLOCK(desc))
	{
		hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->size, sizeof(((struct t_dsc *)XADDR(desc))->size));
		hl_phys_log_change(&((struct t_dsc *)XADDR(desc))->data, sizeof(((struct t_dsc *)XADDR(desc))->data));
	}
	((struct t_dsc *)XADDR(desc))->size -= size;
	((struct t_dsc *)XADDR(desc))->data = intl_last_blk;

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
		if (((struct pstr_long_blk_hdr *)XADDR(blk))->next_blk == intl_last_blk && intl_ftr.cursor < 0)
			fn((char*)XADDR(blk) + ofs, -ofs-intl_ftr.cursor, p);
		else
			fn((char*)XADDR(blk) + ofs, PAGE_SIZE-ofs, p);
		blk = ((struct pstr_long_blk_hdr *)XADDR(blk))->next_blk;
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
	CHECKP(desc);
	pstr_long_feed2(((struct t_dsc *)XADDR(desc))->data, fn, p);
}

void pstr_long_copy_to_buffer(char *buf, const xptr &data, pstr_long_off_t size)
{
	intl_last_blk = data;

	CHECKP(intl_last_blk);
	xptr start = (PSTR_LONG_LAST_BLK_FTR(intl_last_blk))->start;

	pstr_long_cursor cur(start, intl_last_blk);

	while (cur.blk != cur.last_blk)
	{
		CHECKP(cur.blk);
		if (((struct pstr_long_blk_hdr *)XADDR(cur.blk))->next_blk == cur.last_blk && cur.cursor < 0)
		{
			if (-cur.ofs-cur.cursor >= size)
			{
				memcpy(buf, (char*)XADDR(cur.blk) + cur.ofs, size);
				return;
			}
			memcpy(buf, (char*)XADDR(cur.blk) + cur.ofs, -cur.ofs-cur.cursor);
			buf += -cur.ofs-cur.cursor;
			size -= -cur.ofs-cur.cursor;
		}
		else
		{
			if (PAGE_SIZE-cur.ofs >= size)
			{
				memcpy(buf, (char*)XADDR(cur.blk) + cur.ofs, size);
				return;
			}
			memcpy(buf, (char*)XADDR(cur.blk) + cur.ofs, PAGE_SIZE-cur.ofs);
			buf += PAGE_SIZE-cur.ofs;
			size -= PAGE_SIZE-cur.ofs;
		}
		cur.blk = ((struct pstr_long_blk_hdr *)XADDR(cur.blk))->next_blk;
		cur.ofs = PSTR_LONG_BLK_HDR_SIZE;
	}
	if (cur.cursor > 0)
	{
		CHECKP(cur.blk);
		if (cur.cursor-cur.ofs > size)
			memcpy(buf, (char*)XADDR(cur.blk) + cur.ofs, size);
		else
			memcpy(buf, (char*)XADDR(cur.blk) + cur.ofs, cur.cursor-cur.ofs);
	}
}

void pstr_long_copy_to_buffer(char *buf, xptr desc)
{
	CHECKP(desc);
	pstr_long_copy_to_buffer(buf, ((struct t_dsc *)XADDR(desc))->data, ((struct t_dsc *)XADDR(desc))->size);
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

#ifdef PSTR_LONG_TEST
void pstr_long_str_info(xptr desc)
{
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
}
#endif
