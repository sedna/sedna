/*
 * File:  bm_core.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _BM_CORE_H
#define _BM_CORE_H

#include "common/sedna.h"

#include "u/usem.h"
#include "u/uhdd.h"
#include "u/ushm.h"
#include "u/ummap.h"

#include "common/xptr/XptrHash.h"
#include "auxiliary/offs_list.h"

#include <map>
#include <vector>

/*******************************************************************************
********************************************************************************
  MACROSES
********************************************************************************
*******************************************************************************/

#define MASTER_BLOCK_SIZE          4096U
#define VMM_SM_BLK_HDR_MAX_SIZE    4096U

extern lsize_t layerAddressSpaceSize;

/*
 * NOTE: xptr layers start with 1, but data-file layers start with 0
 */
#define ABS_DATA_OFFSET(p)       ((uint64_t)((p).layer - 1) *                  \
                                 (uint64_t)(layerAddressSpaceSize) +        \
                                 (uint64_t)((p).getOffs()))

#define ABS_TMP_OFFSET(p)        ((uint64_t)((p).layer - TMP_LAYER_STARTS_WITH) * \
                                 (uint64_t)(layerAddressSpaceSize) +           \
                                 (uint64_t)((p).getOffs()))


#define OFFS2ADDR(offs)          ((char*)buf_mem_addr + (offs))


/*******************************************************************************
********************************************************************************
  TYPES AND VARIABLES
********************************************************************************
*******************************************************************************/
// Buffer memory starts with this address
extern void* buf_mem_addr;

// Buffer memory table that translates xptr addresses to buffer offsets
typedef XptrHash<ramoffs, 16, 16> t_buffer_table;
extern t_buffer_table buffer_table;

// Hashed list of buffers offsets
typedef offs_list<16, 16> t_ramoffs_list;

// Lists of blocks
extern t_ramoffs_list free_mem;
extern t_ramoffs_list used_mem;
extern t_ramoffs_list blocked_mem;

// A single buffer that is protected from beeng flushed
extern ramoffs buffer_on_stake;

// File mappings
extern UShMem  file_mapping;
extern UShMem p_sm_callback_file_mapping;

#ifdef LRU
// LRU global stamp counter
extern UShMem lru_global_stamp_file_mapping;
#endif

// Global VMM-SM semaphore
extern USemaphore vmm_sm_sem;

// Information about transaction
struct tr_info
{
    USemaphore sm_to_vmm_callback_sem1, sm_to_vmm_callback_sem2;
    xptr freed_data_blocks;
    xptr allocated_tmp_blocks;
};

// Map that contains information about transactions
typedef std::map<session_id, tr_info*> tr_info_map;
extern tr_info_map trs;

// Vector that contains information about physical XPTRs
typedef std::vector<xptr> t_xptr_info;
extern t_xptr_info *phys_xptrs;

// Shared memory for SM callback call (used for callbacking VMMs - stores args)
extern void * p_sm_callback_data;

#ifdef LRU
// Shared memory for global LRU stamp
extern LRU_stamp *lru_global_stamp_data;
#endif

// Identifier of transaction that is in exclusive mode
// if it is set up in -1 then there is no transaction in exclusive mode
extern session_id xmode_sid;

// Synchronization semaphore for intering exclusive mode
extern USemaphore xmode;

// Syncronization semaphores
extern USemaphore cat_nametable_sem;
extern USemaphore cat_master_sem;

/*
extern USemaphore indirection_table_sem;
extern USemaphore metadata_sem;
extern USemaphore index_sem;
#ifdef SE_ENABLE_FTSEARCH
extern USemaphore ft_index_sem;
#endif
#ifdef SE_ENABLE_TRIGGERS
extern USemaphore trigger_sem;
#endif
*/
// File handlers
extern UFile data_file_handle;
extern UFile tmp_file_handle;


// Master block structure definition
struct bm_masterblock
{
    xptr free_data_blocks;
    xptr free_tmp_blocks;

    int64_t data_file_cur_size;
    int64_t tmp_file_cur_size;

    int64_t data_file_max_size;  /* size in PAGES, 0 means unlimited size */
    int64_t tmp_file_max_size;   /* size in PAGES, 0 means unlimited size */

    // in PAGE_SIZE
    int data_file_extending_portion;
    int tmp_file_extending_portion;

    int transaction_flags;

    xptr catalog_masterdata_block;

    lsize_t layer_size; /* layer size for the database */
};

// Master block
extern char bm_master_block_buf[];
extern bm_masterblock *mb;

extern char system_data_buf[];
extern char *system_data_aligned_ptr;


/*******************************************************************************
********************************************************************************
  FUNCTIONS
********************************************************************************
*******************************************************************************/


////////////////////////////////////////////////////////////////////////////////
/// Masterblock functions
////////////////////////////////////////////////////////////////////////////////
/*
void init_master_block();
void release_master_block();
*/
void read_master_block();
void flush_master_block();

////////////////////////////////////////////////////////////////////////////////
/// Buffer functions
////////////////////////////////////////////////////////////////////////////////
bool unmap_block_in_tr(const xptr &p, tr_info *info, bool use_layer);
bool unmap_block_in_trs(session_id sid, const xptr &p, bool use_layer);
xptr put_block_to_buffer(session_id sid, 
                         const xptr &p, 
                         ramoffs /*out*/ *offs,
                         bool read_block_from_disk = true);

bool find_block_in_buffers(const xptr &p,
						   ramoffs *offs);

/*
 * this function flushes block with a given offset from the buffer
 * if 'sync' is true there will be a sync barrier, which guarantees (hopefully)
 * that the block is flushed on disk right now, without reordering.
 * Returns true if block was actually flushed (was dirty).
 */
bool flush_buffer(ramoffs offs, bool sync);
void flush_buffers();
void flush_data_buffers();

////////////////////////////////////////////////////////////////////////////////
/// Functions to work with data file
////////////////////////////////////////////////////////////////////////////////
void write_block_addr(const xptr &p, const void *ptr, unsigned int size,
        bool check);
void read_block_addr(const xptr &p, void *buf, unsigned int size, bool check);

////////////////////////////////////////////////////////////////////////////////
/// Helpers for debug and statistics managment
////////////////////////////////////////////////////////////////////////////////
void bm_reset_io_statistics();
void bm_log_out_io_statistics();

#endif
