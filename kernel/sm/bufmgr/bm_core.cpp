/*
 * File:  bm_core.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <iostream>
#include "u/uutils.h"
#include "common/xptr/sm_vmm_data.h"
#include "sm/bufmgr/bm_core.h"
#include "common/llcommon/llMain.h"
#include "sm/sm_globals.h"
#include "common/errdbg/d_printf.h"
#include "sm/wu/wu.h"

using namespace std;

#define IS_CHANGED(blk)		blk->is_changed

/*******************************************************************************
********************************************************************************
  VARIABLES
********************************************************************************
*******************************************************************************/

// IO statistics
struct bm_core_io_stats {
  int64_t reads;
  int64_t writes;
  
  void reset() { reads = 0; writes = 0; }
};

static bm_core_io_stats buf_io_stats = {};

// Buffer memory starts with this address
void* buf_mem_addr = NULL;

// Buffer memory table that translates xptr addresses to buffer offsets
t_buffer_table buffer_table;

// Table with physical xptrs
t_xptr_info *phys_xptrs;

// Lists of blocks
t_ramoffs_list free_mem;
t_ramoffs_list used_mem;
t_ramoffs_list blocked_mem;

// A single buffer that is protected from beeng flushed
ramoffs buffer_on_stake = RAMOFFS_OUT_OFF_BOUNDS;

// File mappings
UShMem  file_mapping;
UShMem p_sm_callback_file_mapping;

#ifdef LRU
// LRU global stamp counter
UShMem lru_global_stamp_file_mapping;
#endif

// Global VMM-SM semaphore
USemaphore vmm_sm_sem;

// Map that contains information about transactions
tr_info_map trs;

// Shared memory for SM callback call (used for callbacking VMMs - stores args)
void * p_sm_callback_data = NULL;

#ifdef LRU
// Shared memory for global LRU stamp
LRU_stamp *lru_global_stamp_data;
#endif

// Identifier of transaction that is in exclusive mode
// if it is set up in -1 then there is no transaction in exclusive mode
session_id xmode_sid = -1;

// Synchronization semaphore for intering exclusive mode
USemaphore xmode;

// Syncronization semaphores
USemaphore cat_nametable_sem;
USemaphore cat_master_sem;
/*
USemaphore indirection_table_sem;
USemaphore metadata_sem;
USemaphore index_sem;
#ifdef SE_ENABLE_FTSEARCH
USemaphore ft_index_sem;
#endif
#ifdef SE_ENABLE_TRIGGERS
USemaphore trigger_sem;
#endif
*/
// File handlers
UFile data_file_handle;
UFile tmp_file_handle;

// Master block
char bm_master_block_buf[MASTER_BLOCK_SIZE * 2];
bm_masterblock *mb = NULL;


char system_data_buf[VMM_SM_BLK_HDR_MAX_SIZE * 2];
char *system_data_aligned_ptr = NULL;





/*******************************************************************************
********************************************************************************
 FUNCTIONS
********************************************************************************
*******************************************************************************/

////////////////////////////////////////////////////////////////////////////////
/// Masterblock functions
////////////////////////////////////////////////////////////////////////////////
void read_master_block()
{
    if (uSetFilePointer(data_file_handle, (int64_t)0, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
        throw USER_ENV_EXCEPTION("Cannot read master block", false);

    unsigned int number_of_bytes_read = 0;
    int res = uReadFile(data_file_handle, mb, MASTER_BLOCK_SIZE, &number_of_bytes_read, __sys_call_error);
    if (res == 0 || number_of_bytes_read != MASTER_BLOCK_SIZE)
        throw USER_ENV_EXCEPTION("Cannot read master block", false);
    buf_io_stats.reads++;
}

void flush_master_block()
{
    if (uSetFilePointer(data_file_handle, (int64_t)0, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
        throw USER_ENV_EXCEPTION("Cannot write master block", false);

    unsigned int number_of_bytes_written = 0;
    int res = uWriteFile(data_file_handle, mb, MASTER_BLOCK_SIZE, &number_of_bytes_written, __sys_call_error);
    if (res == 0 || number_of_bytes_written != MASTER_BLOCK_SIZE)
        throw USER_ENV_EXCEPTION("Cannot write master block", false);
    buf_io_stats.writes++;

    if (uFlushBuffers(data_file_handle, __sys_call_error) == 0)
        throw SYSTEM_EXCEPTION("Cannot flush buffers (master block)");
}



////////////////////////////////////////////////////////////////////////////////
/// Internal functions
////////////////////////////////////////////////////////////////////////////////

static
void calculate_offset_and_file_handler(const xptr &p,
                                       int64_t *dsk_offs, 
                                       UFile *file_handler,
                                       bool check)
{
    if (IS_DATA_BLOCK(p)) 
    {
        //d_printf1("calculate_offset_and_file_handler: data block\n");
        *dsk_offs = ABS_DATA_OFFSET(p) + (int64_t)PAGE_SIZE;
        
        if (check && !((int64_t)PAGE_SIZE <= *dsk_offs &&
                *dsk_offs <= mb->data_file_cur_size - (int64_t)PAGE_SIZE))
        {
            throw SYSTEM_EXCEPTION("Offset is out of range");
        }
        *file_handler = data_file_handle;
    }
    else 
    {
        //d_printf1("calculate_offset_and_file_handler: tmp block\n");
        *dsk_offs = ABS_TMP_OFFSET(p);
        if (check && !((int64_t)0 <= *dsk_offs &&
                *dsk_offs <= mb->tmp_file_cur_size - (int64_t)PAGE_SIZE))
        {
            throw SYSTEM_EXCEPTION("Offset is out of range");
        }
        *file_handler = tmp_file_handle;
    }
}

void read_block_addr(const xptr &p, void *buf, unsigned int size, bool check)
{
    int64_t dsk_offs;
    UFile file_handler;
    unsigned int number_of_bytes_read;
    int res;

    calculate_offset_and_file_handler(p, &dsk_offs, &file_handler, check);

    if (uSetFilePointer(file_handler, dsk_offs, NULL, U_FILE_BEGIN,
            __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");

    res = uReadFile(file_handler, buf, size, &number_of_bytes_read,
            __sys_call_error);
    if (res == 0 || number_of_bytes_read != size)
        throw SYSTEM_ENV_EXCEPTION("Cannot read block");
}

static void read_block(const xptr &p, ramoffs offs)
{
    vmm_sm_blk_hdr *blk = (vmm_sm_blk_hdr *)OFFS2ADDR(offs);

    read_block_addr(p, blk, PAGE_SIZE, true);

    blk->roffs = offs;
    blk->is_changed = false;
    buf_io_stats.reads++;
}

void write_block_addr(const xptr &p, const void *ptr, unsigned int size,
        bool check)
{
    int64_t dsk_offs;
    UFile file_handler;
    unsigned int number_of_bytes_written;
    int res;

    calculate_offset_and_file_handler(p, &dsk_offs, &file_handler, check);

    if (uSetFilePointer(file_handler, dsk_offs, NULL, U_FILE_BEGIN,
            __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");

    res = uWriteFile(file_handler, ptr, size,
            &number_of_bytes_written, __sys_call_error);
    if (res == 0 || number_of_bytes_written != size)
        throw SYSTEM_ENV_EXCEPTION("Cannot write block");
}

static void write_block(const xptr &p, ramoffs offs)
{
    write_block_addr(p, OFFS2ADDR(offs), PAGE_SIZE, true);
    buf_io_stats.writes++;
}

////////////////////////////////////////////////////////////////////////////////
/// Buffer functions
////////////////////////////////////////////////////////////////////////////////

bool unmap_block_in_tr(const xptr &p, tr_info *info, bool use_layer)
{
    *(xptr*)p_sm_callback_data = p;
    *(int*)((xptr*)p_sm_callback_data + 1) = (int)use_layer;

    USemaphoreUp(info->sm_to_vmm_callback_sem1, __sys_call_error);
    USemaphoreDown(info->sm_to_vmm_callback_sem2, __sys_call_error);

    return *(bool*)p_sm_callback_data;
}

bool unmap_block_in_trs(session_id sid, const xptr &p, bool use_layer)
{
    // approve of xptr 
    tr_info_map::iterator it;
    for (it = trs.begin(); it != trs.end(); it++)
    {
        if (it->first != sid)
        {
            if (!(unmap_block_in_tr(p, it->second, use_layer)))
                return false;
        }
    }

    return true;
}

xptr get_free_buffer(session_id sid, ramoffs /*out*/ *offs)
{
    int res = 0;
    res = free_mem.pop(offs);
    if (res == 0) return XNULL; // we have found the free block in the list of free blocks

    // there is no free buffer, so we should free one
    xptr cur_p = XNULL;
    while (true)
    {
#ifdef LRU
        t_ramoffs_list::iterator um_it;
        LRU_stamp min_stamp = 1000000000, cur_stamp = 0;
        for (um_it = used_mem.begin(); um_it != used_mem.end(); ++um_it)
        {
            if (((vmm_sm_blk_hdr*)OFFS2ADDR(*um_it))->lru < min_stamp) *offs = *um_it;
        }

        if (used_mem.find_remove(*offs) != 0)
            throw SYSTEM_EXCEPTION("Error in LRU algorithm");
#else
        res = used_mem.pop(offs);
        if (res != 0) throw SYSTEM_EXCEPTION("There is absolutely no buffer memory");
#endif
		bool approved = false;
		
		if (*offs != buffer_on_stake)
		{

			cur_p = ((vmm_sm_blk_hdr*)OFFS2ADDR(*offs))->p;
			/*
			// approve of offs 
			tr_info_map::iterator it;
			for (it = trs.begin(); it != trs.end(); it++)
			{
				if (it->first != sid)
				{
					*(xptr*)p_sm_callback_data = cur_p;

					USemaphoreUp(it->second->sm_to_vmm_callback_sem1, __sys_call_error);
					USemaphoreDown(it->second->sm_to_vmm_callback_sem2, __sys_call_error);

					if (!(*(bool*)p_sm_callback_data))
					{
						used_mem.push(*offs);
						break;
					}
				}
			}
			*/
			approved = unmap_block_in_trs(sid, cur_p, false);
		}
        if (approved) break;
        else used_mem.push(*offs);
        //if (it == trs.end()) break; // successfully approved
    }

    flush_buffer(*offs, false);
	buffer_table.remove((*phys_xptrs)[*offs/PAGE_SIZE]);
	
	(*phys_xptrs)[*offs/PAGE_SIZE]=XNULL;
	
    return cur_p;
}

bool find_block_in_buffers(const xptr &p,
						   ramoffs *offs)
{
	int res = 0;
	
	res = buffer_table.find(p, offs);
	return (res == 0); // we have found the block in memory?
}


xptr put_block_to_buffer(session_id sid, 
                         const xptr &p, 
                         ramoffs /*out*/ *offs, 
                         bool read_block_from_disk)
{
    if (find_block_in_buffers(p, offs)) return xptr(); // we have found the block in memory
	
//d_printf1("put 2\n");
    // this block is not in memory
    // try to find free buffer for the block
    xptr swapped = get_free_buffer(sid, offs);
    //since that offs references free buffer
          
//d_printf1("put 3\n");
    buffer_table.insert(p, *offs);
    used_mem.push(*offs);

//d_printf1("put 4\n");
    // read block from disk
    if (read_block_from_disk) 
	{
		read_block(p, *offs);
		vmm_sm_blk_hdr *hdr = (vmm_sm_blk_hdr*)OFFS2ADDR(*offs);
		hdr->trid_wr_access = -1;
        U_ASSERT(hdr->p.getOffs() < LAYER_ADDRESS_SPACE_SIZE);
	}
	else
	{
		vmm_sm_blk_hdr *hdr = (vmm_sm_blk_hdr*)OFFS2ADDR(*offs);
		vmm_sm_blk_hdr::init(hdr);
		hdr->p = p;
		hdr->roffs = *offs;
	}

//d_printf1("put 5\n");

	// store information about physical xptr
	size_t ind = (*offs) / PAGE_SIZE;
	(*phys_xptrs)[ind] = p;

    return swapped;
}

bool flush_buffer(ramoffs offs, bool sync)
{
	vmm_sm_blk_hdr *blk = NULL;
	size_t ind = offs / PAGE_SIZE;
	xptr physXptr;

    blk = (vmm_sm_blk_hdr*)OFFS2ADDR(offs);

    if (IS_CHANGED(blk)) 
	{
		physXptr = (*phys_xptrs)[ind];
		if (IS_DATA_BLOCK(physXptr)) 
		{
			llFlushLsn(blk->lsn);
		}
		WuOnFlushBufferExn(physXptr);

		// sync barrier
		if (sync && uFlushBuffers(data_file_handle, __sys_call_error) == 0)
			throw SYSTEM_EXCEPTION("Cannot sync-flush buffer");

        write_block(physXptr, offs);

		// sync barrier
		if (sync && uFlushBuffers(data_file_handle, __sys_call_error) == 0)
			throw SYSTEM_EXCEPTION("Cannot sync-flush buffer");

		blk->is_changed = false;
		/*	TODO: it will introduce bugs if flushed a buffer which a transaction is modifying now,
			anyway it will lead to unpredictable results even w/o is_changed issue */ 
        return true;
	}

    return false;
}

void flush_buffers()
{
	vmm_sm_blk_hdr *blk = NULL;
    t_buffer_table::iterator it;

    for (it = buffer_table.begin(); it != buffer_table.end(); ++it)
    {
        blk = (vmm_sm_blk_hdr*)OFFS2ADDR((ramoffs)(*it));
        blk->p.print();
		flush_buffer((ramoffs)(*it), false);
    }

    d_printf1("Flush buffers: complete\n");
}

/* TODO: 
	rename to flush_buffers_on_checkpoint
	flush only blocks from the pers snapshot */ 
void flush_data_buffers()
{
    t_buffer_table::iterator it;
    vmm_sm_blk_hdr *blk = NULL;
    int flushed = 0;

    elog(EL_DBG, ("Flushing buffers on checkpoint started"));

    for (it = buffer_table.begin(); it != buffer_table.end(); ++it)
    {
        blk = (vmm_sm_blk_hdr*)OFFS2ADDR((ramoffs)(*it));

        if (IS_DATA_BLOCK(blk->p))
        {
		// here we flush them without sync, but look below
            if (flush_buffer((ramoffs)(*it), false)) flushed++;
        }
    }

    elog(EL_DBG, ("Wrote back %d dirty buffers, going to sync", flushed));

    // sync barrier
    if (uFlushBuffers(data_file_handle, __sys_call_error) == 0)
        throw SYSTEM_EXCEPTION("Cannot sync-flush buffers (checkpoint)");

    elog(EL_DBG, ("Flushing buffers on checkpoint complete"));
}


////////////////////////////////////////////////////////////////////////////////
/// Helpers for debug and statistics managment
////////////////////////////////////////////////////////////////////////////////

#if defined(__GNUC__) && (__GNUC__ >= 4) && (__GNUC_MINOR__ >= 2)
#pragma GCC diagnostic ignored "-Wformat"
#endif /* GNUC */

void dump_bufmgr_state()
{
	unsigned bufsNum = 0, i = 0;
	xptr physXptr, logXptr;
	void *ptr=NULL;
	vmm_sm_blk_hdr *hdr=NULL;
	ramoffs offs=0;
	char auxInfo[2048]="";
	char flags[16]="";

	bufsNum = (unsigned)sm_globals::bufs_num;

	fprintf(stderr,"---STARTING DUMP OF BUFMGR STATE---\n");
	for (i=0; i<bufsNum; ++i)
	{
		strcpy(flags,"");
		ptr = (char*)buf_mem_addr + i*PAGE_SIZE;
		hdr = (vmm_sm_blk_hdr*)ptr;
		physXptr = phys_xptrs->at(i);
		logXptr = hdr->p;
		if (0==free_mem.find(i*PAGE_SIZE))
		{
			/* skip - it's free buf */ 
		}
		else if (0==buffer_table.find(physXptr, &offs))
		{
			if (offs!=i*PAGE_SIZE)
			{
				fprintf(stderr,"%4d error - expected offset %6x, offset %6x according to buffer_table\n", i, i*PAGE_SIZE, offs);
			}
			if (hdr->is_changed) strcat(flags,"D");
			fprintf(stderr, "%4d xptr:%8x%08p lxptr:%8x%08p %4s %s\n",
				i, physXptr.layer, physXptr.getOffs(), logXptr.layer, logXptr.getOffs(), flags, auxInfo);
		}
	}
	fprintf(stderr,"---FINISHED DUMP OF BUFMGR STATE---\n");
}

#if defined(__GNUC__) && (__GNUC__ >= 4) && (__GNUC_MINOR__ >= 2)
#pragma GCC diagnostic warning "-Wformat"
#endif /* GNUC */

void bm_reset_io_statistics()
{
    buf_io_stats.reset();
}

void bm_log_out_io_statistics()
{
    char reads_buf[20];
    char writes_buf[20];
    
    u_i64toa(buf_io_stats.reads, reads_buf, 10);
    u_i64toa(buf_io_stats.writes, writes_buf, 10);

    elog(EL_INFO, ("IO block reads:%s, writes:%s", reads_buf, writes_buf));
}
