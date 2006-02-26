/*
 * File:  bm_core.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _BM_CORE_H
#define _BM_CORE_H

#include <map>
#include "exceptions.h"
#include "XptrHash.h"
#include "IntList.h"
#include "usem.h"
#include "uhdd.h"
#include "ushm.h"
#include "ummap.h"
#include "persistent_db_data.h"


/*******************************************************************************
********************************************************************************
  MACROSES
********************************************************************************
*******************************************************************************/

#define MASTER_BLOCK_SIZE		4096

#define ABS_DATA_OFFSET(p)		((__int64)((p).layer) *												\
                                 (__int64)(LAYER_ADDRESS_SPACE_SIZE) +								\
                                 (__int64)((uint32)((p).addr) - LAYER_ADDRESS_SPACE_START_ADDR_INT))

#define ABS_TMP_OFFSET(p)		((__int64)((p).layer - TMP_LAYER_STARTS_WITH) *						\
                                 (__int64)(LAYER_ADDRESS_SPACE_SIZE) +								\
                                 (__int64)((uint32)((p).addr) - LAYER_ADDRESS_SPACE_START_ADDR_INT))

#define DATA_FILE_OFFS2XPTR(s)	xptr((t_layer)((__int64)(s) / (__int64)(LAYER_ADDRESS_SPACE_SIZE)),								\
                                     (void*)((uint32)((__int64)(s) % (__int64)(LAYER_ADDRESS_SPACE_SIZE)) + LAYER_ADDRESS_SPACE_START_ADDR_INT))

#define TMP_FILE_OFFS2XPTR(s)	xptr((t_layer)((__int64)(s) / (__int64)(LAYER_ADDRESS_SPACE_SIZE)) + TMP_LAYER_STARTS_WITH,		\
                                     (void*)((uint32)((__int64)(s) % (__int64)(LAYER_ADDRESS_SPACE_SIZE)) + LAYER_ADDRESS_SPACE_START_ADDR_INT))

#define OFFS2ADDR(offs)			((char*)buf_mem_addr + (offs))


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
typedef IntList<16, 16> t_ramoffs_list;

// Lists of blocks
extern t_ramoffs_list free_mem;
extern t_ramoffs_list used_mem;
extern t_ramoffs_list blocked_mem;

// File mappings
extern UMMap  file_mapping;
extern UShMem p_sm_callback_file_mapping;
extern UShMem itfe_file_mapping;

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
extern USemaphore indirection_table_sem;
extern USemaphore metadata_sem;
extern USemaphore index_sem;
#ifdef SE_ENABLE_FTSEARCH
extern USemaphore ft_index_sem;
#endif

// Pointer to shared memory where xptr to indirection table is stored
extern xptr* indirection_table_free_entry;

// File handlers
extern UFile data_file_handler;
extern UFile tmp_file_handler;


// Master block structure definition
struct bm_masterblock
{
    xptr free_data_blocks;
    xptr free_tmp_blocks;

    __int64 data_file_cur_size;
    __int64 tmp_file_cur_size;

    __int64 data_file_max_size;
    __int64 tmp_file_max_size;

    // in PAGE_SIZE
    int data_file_extending_portion;
    int tmp_file_extending_portion;

    xptr indirection_table_free_entry;

    persistent_db_data* pdb; // persistent db data
};

// Master block
extern bm_masterblock *mb;


/*******************************************************************************
********************************************************************************
  FUNCTIONS
********************************************************************************
*******************************************************************************/


////////////////////////////////////////////////////////////////////////////////
/// Masterblock functions
////////////////////////////////////////////////////////////////////////////////
void init_master_block();
void release_master_block();

void read_master_block();
void flush_master_block(bool is_write_plog = true);

////////////////////////////////////////////////////////////////////////////////
/// Buffer functions
////////////////////////////////////////////////////////////////////////////////
xptr get_free_buffer(session_id sid, ramoffs /*out*/ *offs);
xptr put_block_to_buffer(session_id sid, 
                         const xptr &p, 
                         ramoffs /*out*/ *offs,
                         bool read_block_from_disk = true);


void flush_buffers(bool sync_phys_log = true);
void flush_data_buffers();


////////////////////////////////////////////////////////////////////////////////
/// PH functions
////////////////////////////////////////////////////////////////////////////////
void flush_ph();
void backup_ph();

#endif

