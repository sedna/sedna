/*
 * File:  bm_core.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <iostream>
#include "common/sm_vmm_data.h"
#include "common/ph/pers_heap.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/plmgr/plmgr.h"
#include "sm/sm_globals.h"
#include "common/errdbg/d_printf.h"

using namespace std;

#define IS_CHANGED(blk)		blk->is_changed

/*******************************************************************************
********************************************************************************
  VARIABLES
********************************************************************************
*******************************************************************************/
// Buffer memory starts with this address
void* buf_mem_addr = NULL;

// Buffer memory table that translates xptr addresses to buffer offsets
t_buffer_table buffer_table;

// Lists of blocks
t_ramoffs_list free_mem;
t_ramoffs_list used_mem;
t_ramoffs_list blocked_mem;

// File mappings
UMMap  file_mapping;
UShMem p_sm_callback_file_mapping;
UShMem itfe_file_mapping;

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
USemaphore indirection_table_sem;
USemaphore metadata_sem;
USemaphore index_sem;
#ifdef SE_ENABLE_FTSEARCH
USemaphore ft_index_sem;
#endif
#ifdef SE_ENABLE_TRIGGERS
USemaphore trigger_sem;
#endif

// Pointer to shared memory where xptr to indirection table is stored
xptr* indirection_table_free_entry = NULL;

// File handlers
UFile data_file_handler;
UFile tmp_file_handler;

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
    if (uSetFilePointer(data_file_handler, (__int64)0, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
        throw USER_ENV_EXCEPTION("Cannot read master block", false);

    int number_of_bytes_read = 0;
    int res = uReadFile(data_file_handler, mb, MASTER_BLOCK_SIZE, &number_of_bytes_read, __sys_call_error);
    if (res == 0 || number_of_bytes_read != MASTER_BLOCK_SIZE)
        throw USER_ENV_EXCEPTION("Cannot read master block", false);

    *indirection_table_free_entry = mb->indirection_table_free_entry;
}

void flush_master_block(bool is_write_plog)
{
    mb->indirection_table_free_entry = *indirection_table_free_entry;
	
	if (is_write_plog)
		ll_phys_log_master_blk(mb, sizeof(bm_masterblock));

    if (uSetFilePointer(data_file_handler, (__int64)0, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
        throw USER_ENV_EXCEPTION("Cannot write master block", false);

    int number_of_bytes_written = 0;
    int res = uWriteFile(data_file_handler, mb, MASTER_BLOCK_SIZE, &number_of_bytes_written, __sys_call_error);
    if (res == 0 || number_of_bytes_written != MASTER_BLOCK_SIZE)
        throw USER_ENV_EXCEPTION("Cannot write master block", false);
}



////////////////////////////////////////////////////////////////////////////////
/// Internal functions
////////////////////////////////////////////////////////////////////////////////

void calculate_offset_and_file_handler(const xptr &p, 
                                       __int64 *dsk_offs, 
                                       UFile *file_handler)
{
    if (IS_DATA_BLOCK(p)) 
    {
        //d_printf1("calculate_offset_and_file_handler: data block\n");
        *dsk_offs = ABS_DATA_OFFSET(p) + (__int64)PAGE_SIZE;
        
        if (!((__int64)PAGE_SIZE <= *dsk_offs && *dsk_offs <= mb->data_file_cur_size - (__int64)PAGE_SIZE))
        {
            throw SYSTEM_EXCEPTION("Offset is out of range");
        }
        *file_handler = data_file_handler;
    }
    else 
    {
        //d_printf1("calculate_offset_and_file_handler: tmp block\n");
        *dsk_offs = ABS_TMP_OFFSET(p);
        if (!((__int64)0 <= *dsk_offs && *dsk_offs <= mb->tmp_file_cur_size - (__int64)PAGE_SIZE))
        {
            throw SYSTEM_EXCEPTION("Offset is out of range");
        }
        *file_handler = tmp_file_handler;
    }
}

void read_block(const xptr &p, ramoffs offs) throw (SednaException)
{
    __int64 dsk_offs = 0;
    UFile file_handler;
    calculate_offset_and_file_handler(p, &dsk_offs, &file_handler);

    // read block
    if (uSetFilePointer(file_handler, dsk_offs, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");

    vmm_sm_blk_hdr *blk = (vmm_sm_blk_hdr*)OFFS2ADDR(offs);

    int number_of_bytes_read = 0;
    int res = uReadFile(file_handler, blk, PAGE_SIZE, &number_of_bytes_read, __sys_call_error);
    if (res == 0 || number_of_bytes_read != PAGE_SIZE)
        throw SYSTEM_ENV_EXCEPTION("Cannot read block");

    blk->roffs = offs;
    blk->is_changed = false;
}

void write_block(const xptr &p, ramoffs offs, bool sync_phys_log = true) throw (SednaException)
{
    vmm_sm_blk_hdr *blk = (vmm_sm_blk_hdr*)OFFS2ADDR(offs);

    blk->roffs = 0;
    blk->is_changed = false;

    if (IS_DATA_BLOCK(p)) 
        ll_phys_log_flush_blk(blk, sync_phys_log);

    // write block
    __int64 dsk_offs = 0;
    UFile file_handler;
    calculate_offset_and_file_handler(p, &dsk_offs, &file_handler);

    if (uSetFilePointer(file_handler, dsk_offs, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");

    int number_of_bytes_written = 0;
    int res = uWriteFile(file_handler, blk, PAGE_SIZE, &number_of_bytes_written, __sys_call_error);
    if (res == 0 || number_of_bytes_written != PAGE_SIZE)
        throw SYSTEM_ENV_EXCEPTION("Cannot write block");
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
    res = free_mem.pop(*offs);
    if (res == 0) return XNULL; // we have found the free block in the list of free blocks

    // there is no free buffer, so we should free one
    xptr cur_p;
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
        res = used_mem.pop(*offs);
        if (res != 0) throw SYSTEM_EXCEPTION("There is absolutely no buffer memory");
#endif

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
        bool approved = unmap_block_in_trs(sid, cur_p, false);
        if (approved) break;
        else used_mem.push(*offs);
        //if (it == trs.end()) break; // successfully approved
    }

    buffer_table.remove(cur_p);
    // store block to disk if it was changed
    vmm_sm_blk_hdr *blk = NULL;
    blk = (vmm_sm_blk_hdr*)OFFS2ADDR(*offs);
    if (IS_CHANGED(blk)) write_block(cur_p, *offs);
    return cur_p;
}

xptr put_block_to_buffer(session_id sid, 
                         const xptr &p, 
                         ramoffs /*out*/ *offs, 
                         bool read_block_from_disk)
{
    int res = 0;

//d_printf1("put 1\n");
    res = buffer_table.find(p, *offs);
    if (res == 0) return xptr(); // we have found the block in memory

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
    if (read_block_from_disk) read_block(p, *offs);

//d_printf1("put 5\n");
    return swapped;
}

void flush_buffers(bool sync_phys_log)
{
    t_buffer_table::iterator it;
    vmm_sm_blk_hdr *blk = NULL;

    //d_printf1("Flush buffers: starting...\n");

    for (it = buffer_table.begin(); it != buffer_table.end(); ++it)
    {
        blk = (vmm_sm_blk_hdr*)OFFS2ADDR((ramoffs)(*it));

        //d_printf2("record 		(offs = %d) xptr = ", (ramoffs)(*it));
        blk->p.print();

        if (IS_CHANGED(blk)) 
        {
            //d_printf2("write record 	(offs = %d) xptr = ", (ramoffs)(*it));
            blk->p.print();

            write_block(blk->p, (ramoffs)(*it), sync_phys_log);
        }
    }

    d_printf1("Flush buffers: complete\n");
}

void flush_data_buffers()
{
    t_buffer_table::iterator it;
    vmm_sm_blk_hdr *blk = NULL;

    d_printf1("Flush data buffers: starting...\n");

    for (it = buffer_table.begin(); it != buffer_table.end(); ++it)
    {
        blk = (vmm_sm_blk_hdr*)OFFS2ADDR((ramoffs)(*it));

        if (IS_DATA_BLOCK(blk->p))
        {
            d_printf2("record 		(offs = %d) xptr = ", (ramoffs)(*it));
            blk->p.print();

            if (IS_CHANGED(blk)) 
            {
                d_printf2("write record 	(offs = %d) xptr = ", (ramoffs)(*it));
                blk->p.print();

                write_block(blk->p, (ramoffs)(*it));
            }
        }
    }

    d_printf1("Flush data buffers: complete\n");
}


////////////////////////////////////////////////////////////////////////////////
/// PH functions
////////////////////////////////////////////////////////////////////////////////
void flush_ph()
{
    if (pers_flush() != 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot flush persistent heap");
}

void backup_ph()
{
    string ph_file_name    = string(db_files_path) + string(db_name) + ".seph";
    string ph_bu_file_name = string(db_files_path) + string(db_name) + ".ph.sebu";

    if (uCopyFile(ph_file_name.c_str(), ph_bu_file_name.c_str(), false, __sys_call_error) == 0)
        throw USER_EXCEPTION2(SE4049, (ph_file_name + " to " + ph_bu_file_name).c_str());
}


