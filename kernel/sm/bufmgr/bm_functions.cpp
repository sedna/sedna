/*
 * File:  bm_functions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


/******************************************************************************
The following is left to implement:
1. I do not _reserve_ address space for data. It can bring us to unpredictable
   errors.

*******************************************************************************/


#include "sedna.h"
#include <string>
#include "sm_vmm_data.h"
#include "pers_heap.h"
#include "bm_functions.h"
#include "bm_core.h"
#include "blk_mngmt.h"
#include "sm_globals.h"
#include "d_printf.h"

using namespace std;

/*******************************************************************************
********************************************************************************
  ACTIVATE/DEACTIVATE FUNCTIONS
********************************************************************************
*******************************************************************************/

#if (defined(_WIN32) && defined(REQUIRE_ROOT))
#define EBS_WORKING_SET_SIZE	(20 * 1024 * 1024)
SIZE_T MinimumWorkingSetSize_orig = 0, MaximumWorkingSetSize_orig = 0;
#endif

void _bm_init_buffer_pool()
{
#if (defined(_WIN32) && defined(REQUIRE_ROOT))
    int working_set_size = bufs_num * PAGE_SIZE + EBS_WORKING_SET_SIZE;
    BOOL res = 0;

    res = GetProcessWorkingSetSize(
                    GetCurrentProcess(),        // handle to the process
                    &MinimumWorkingSetSize_orig,// minimum working set size
                    &MaximumWorkingSetSize_orig // maximum working set size
          );
    if (res == 0) 
        throw USER_EXCEPTION(SE1015);

    res = SetProcessWorkingSetSize(
                    GetCurrentProcess(),		// handle to process
                    working_set_size,			// minimum working set size
                    working_set_size			// maximum working set size
          );
    if (res == 0) 
        throw USER_EXCEPTION(SE1015);
#endif

    file_mapping = uCreateFileMapping(U_INVALID_FD, bufs_num * PAGE_SIZE, CHARISMA_BUFFER_SHARED_MEMORY_NAME, NULL, __sys_call_error);
    if (U_INVALID_FILEMAPPING(file_mapping))
        throw USER_EXCEPTION(SE1015);

    buf_mem_addr = uMapViewOfFile(file_mapping, NULL, bufs_num * PAGE_SIZE, 0, __sys_call_error);
    if (buf_mem_addr == NULL)
        throw USER_EXCEPTION(SE1015);

#ifdef REQUIRE_ROOT
    if (uMemLock(buf_mem_addr, bufs_num * PAGE_SIZE, __sys_call_error) == -1)
        throw USER_EXCEPTION(SE1016);
#endif

    for (int i = 0; i < bufs_num; i++) free_mem.push(i * PAGE_SIZE);
}

void _bm_release_buffer_pool()
{
    buffer_table.clear();
    free_mem.clear();
    used_mem.clear();
    blocked_mem.clear();

#ifdef REQUIRE_ROOT
    if (uMemUnlock(buf_mem_addr, bufs_num * PAGE_SIZE, __sys_call_error) == -1)
        throw USER_ENV_EXCEPTION("Cannot release system structures", false);
#endif

    if (uUnmapViewOfFile(file_mapping, buf_mem_addr, bufs_num * PAGE_SIZE, __sys_call_error) == -1)
        throw USER_ENV_EXCEPTION("Cannot release system structures", false);

    if (uReleaseFileMapping(file_mapping, CHARISMA_BUFFER_SHARED_MEMORY_NAME, __sys_call_error) == -1)
        throw USER_ENV_EXCEPTION("Cannot release system structures", false);

#if (defined(_WIN32) && defined(REQUIRE_ROOT))
    BOOL res = 0;

    res = SetProcessWorkingSetSize(
                    GetCurrentProcess(),       // handle to the process
                    MinimumWorkingSetSize_orig,// minimum working set size
                    MaximumWorkingSetSize_orig // maximum working set size
          );
    if (res == 0)
        throw USER_ENV_EXCEPTION("Cannot release system structures", false);
#endif
}

void bm_startup() throw (SednaException)
{
    // open data and tmp files
    string data_file_name = string(db_files_path) + string(db_name) + ".data";
    data_file_handler = uOpenFile(data_file_name.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, __sys_call_error);
    if (data_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION2(SE4042, data_file_name.c_str());

    string tmp_file_name = string(db_files_path) + string(db_name) + ".tmp";
    tmp_file_handler = uOpenFile(tmp_file_name.c_str(), 0, U_READ_WRITE, U_NO_BUFFERING, __sys_call_error);
    if (tmp_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION2(SE4042, tmp_file_name.c_str());

    // create buffer pool
    _bm_init_buffer_pool();
    // buffer pool has been created

    // create semaphores
    if (USemaphoreCreate(&vmm_sm_sem, 1, 1, VMM_SM_SEMAPHORE_STR, NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4010, "VMM_SM_SEMAPHORE_STR");

    if (USemaphoreCreate(&xmode, 1, 1, VMM_SM_EXCLUSIVE_MODE_SEM_STR, NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4010, "VMM_SM_EXCLUSIVE_MODE_SEM_STR");

    if (USemaphoreCreate(&indirection_table_sem, 1, 1, INDIRECTION_TABLE_SEMAPHORE_STR, NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4010, "INDIRECTION_TABLE_SEMAPHORE_STR");

    if (USemaphoreCreate(&metadata_sem, 1, 1, METADATA_SEMAPHORE_STR, NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4010, "METADATA_SEMAPHORE_STR");

    if (USemaphoreCreate(&index_sem, 1, 1, INDEX_SEMAPHORE_STR, NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4010, "INDEX_SEMAPHORE_STR");
#ifdef SE_ENABLE_FTSEARCH
    if (USemaphoreCreate(&ft_index_sem, 1, 1, FT_INDEX_SEMAPHORE_STR, NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4010, "FT_INDEX_SEMAPHORE_STR");
#endif

    // Create shared memory
    if (uCreateShMem(&p_sm_callback_file_mapping, CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME, sizeof(xptr), NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4016, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

    p_sm_callback_data = uAttachShMem(p_sm_callback_file_mapping, NULL, sizeof(xptr), __sys_call_error); 
    if (p_sm_callback_data == NULL) 
        throw USER_EXCEPTION2(SE4023, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");
#ifdef LRU
    if (uCreateShMem(&lru_global_stamp_file_mapping, CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME, sizeof(LRU_stamp), NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4016, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");

    lru_global_stamp_data = (LRU_stamp*)uAttachShMem(lru_global_stamp_file_mapping, NULL, sizeof(LRU_stamp), __sys_call_error); 
    if (lru_global_stamp_data == NULL) 
        throw USER_EXCEPTION2(SE4023, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");
    *lru_global_stamp_data = 0;
#endif
    if (uCreateShMem(&itfe_file_mapping, CHARISMA_ITFE_SHARED_MEMORY_NAME, sizeof(xptr), NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4016, "CHARISMA_ITFE_SHARED_MEMORY_NAME");

    indirection_table_free_entry = (xptr*)uAttachShMem(itfe_file_mapping, NULL, sizeof(xptr), __sys_call_error);
    if (indirection_table_free_entry == NULL) 
        throw USER_EXCEPTION2(SE4023, "CHARISMA_ITFE_SHARED_MEMORY_NAME");

    string ph_file_name = string(db_files_path) + string(db_name) + ".ph";
    if (pers_open(ph_file_name.c_str(), CHARISMA_PH_SHARED_MEMORY_NAME, 
                  PERS_HEAP_SEMAPHORE_STR, PH_ADDRESS_SPACE_START_ADDR, 0) != 0)
        throw USER_ENV_EXCEPTION("Cannot open persistent heap", false);


    read_master_block();
}

void bm_shutdown() throw (SednaException)
{
    if (pers_close() != 0)
        throw USER_ENV_EXCEPTION("Cannot close persistent heap", false);

    flush_buffers();

    flush_master_block();
    d_printf1("Flush master block: complete\n");

    if (uDettachShMem(p_sm_callback_file_mapping, p_sm_callback_data, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4024, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

    if (uReleaseShMem(p_sm_callback_file_mapping, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4020, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");
#ifdef LRU
    if (uDettachShMem(lru_global_stamp_file_mapping, lru_global_stamp_data, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4024, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");

    if (uReleaseShMem(lru_global_stamp_file_mapping, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4020, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");
#endif
    if (uDettachShMem(itfe_file_mapping, indirection_table_free_entry, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4024, "CHARISMA_ITFE_SHARED_MEMORY_NAME");

    if (uReleaseShMem(itfe_file_mapping, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4020, "CHARISMA_ITFE_SHARED_MEMORY_NAME");

    d_printf1("Release shared memory: complete\n");



    if (USemaphoreRelease(vmm_sm_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "VMM_SM_SEMAPHORE_STR");

    if (USemaphoreRelease(xmode, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "VMM_SM_EXCLUSIVE_MODE_SEM_STR");

    if (USemaphoreRelease(indirection_table_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "INDIRECTION_TABLE_SEMAPHORE_STR");

    if (USemaphoreRelease(metadata_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "METADATA_SEMAPHORE_STR");

    if (USemaphoreRelease(index_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "INDEX_SEMAPHORE_STR");
#ifdef SE_ENABLE_FTSEARCH
    if (USemaphoreRelease(ft_index_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "FT_INDEX_SEMAPHORE_STR");
#endif
    d_printf1("Release semaphores: complete\n");


    _bm_release_buffer_pool();
    d_printf1("Release shared memory: complete\n");


    if (uCloseFile(data_file_handler, __sys_call_error) == 0)
        throw USER_EXCEPTION2(SE4043, ".data file");

    if (uCloseFile(tmp_file_handler, __sys_call_error) == 0)
        throw USER_EXCEPTION2(SE4043, ".tmp file");
    d_printf1("Close database files: complete\n");
}

void bm_register_session(session_id sid, persistent_db_data** pdb) throw (SednaException)
{
    tr_info_map::iterator it = trs.find(sid);
    if (it != trs.end()) throw USER_EXCEPTION(SE1018);

    char buf[100];
    USemaphore sm_to_vmm_callback_sem1, sm_to_vmm_callback_sem2;

    d_printf2("Register session with sid = %d\n", sid);

    if (USemaphoreCreate(&sm_to_vmm_callback_sem1, 0, 1, SM_TO_VMM_CALLBACK_SEM1_BASE_STR(sid, db_name, buf, 100), NULL, __sys_call_error) != 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot create SM_TO_VMM_CALLBACK_SEM1_BASE_STR");

    if (USemaphoreCreate(&sm_to_vmm_callback_sem2, 0, 1, SM_TO_VMM_CALLBACK_SEM2_BASE_STR(sid, db_name, buf, 100), NULL, __sys_call_error) != 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot create SM_TO_VMM_CALLBACK_SEM2_BASE_STR");

    tr_info *ti = new tr_info;
    ti->sm_to_vmm_callback_sem1 = sm_to_vmm_callback_sem1;
    ti->sm_to_vmm_callback_sem2 = sm_to_vmm_callback_sem2;
    ti->freed_data_blocks = XNULL;
    ti->allocated_tmp_blocks = XNULL;


    trs.insert(tr_info_map::value_type(sid, ti));

    *pdb = mb->pdb;
}

void bm_unregister_session(session_id sid) throw (SednaException)
{
    d_printf2("Unregister session with sid = %d\n", sid);
    tr_info_map::iterator it = trs.find(sid);
    if (it == trs.end()) throw USER_EXCEPTION(SE1018);

    if (USemaphoreRelease(it->second->sm_to_vmm_callback_sem1, __sys_call_error) != 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot release SM_TO_VMM_CALLBACK_SEM1_BASE_STR");

    if (USemaphoreRelease(it->second->sm_to_vmm_callback_sem2, __sys_call_error) != 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot release SM_TO_VMM_CALLBACK_SEM2_BASE_STR");

	tr_info *info = it->second;
	trs.erase(it);
    delete (info);
}

void bm_register_transaction(session_id sid, transaction_id trid) throw (SednaException)
{
    // for now trid is not used in any extent
    d_printf2("Register transaction with trid = %d\n", trid);
}

void bm_unregister_transaction(session_id sid, transaction_id trid)  throw (SednaException)
{
    // for now trid is not used in any extent
    d_printf2("Unregister transaction with trid = %d\n", trid);
    tr_info_map::iterator it = trs.find(sid);
    if (it == trs.end()) throw USER_EXCEPTION(SE1018);

    xptr p;
	tr_info *info = it->second;

	//info->freed_data_blocks.print();
    while (pop_from_persistent_free_blocks_stack(&(info->freed_data_blocks), &p) == 0)
        delete_data_block(p);

	//info->allocated_tmp_blocks.print();
    while (pop_from_persistent_used_blocks_stack(&(info->allocated_tmp_blocks), &p) == 0)
        delete_tmp_block(p, info);
}

void bm_delete_tmp_blocks(session_id sid) throw (SednaException)
{
    tr_info_map::iterator it = trs.find(sid);
    if (it == trs.end()) throw USER_EXCEPTION(SE1018);

    xptr p;
	tr_info *info = it->second;

    while (pop_from_persistent_used_blocks_stack(&(info->allocated_tmp_blocks), &p) == 0)
        delete_tmp_block(p, info);
}

void bm_allocate_data_block(session_id sid,
                            xptr /*out*/ *p,
                            ramoffs /*out*/ *offs,
                            xptr /*out*/ *swapped) throw (SednaException)
{
    //d_printf1("bm_allocate_data_block: begin\n");
//d_printf1("allocate 1\n");
    new_data_block(p);

//d_printf1("allocate 2\n");
    *swapped = put_block_to_buffer(sid, *p, offs, false);

    //d_printf2("offset %d\n", *offs);

    //*swapped = get_free_buffer(sid, offs);
    //buffer_table.insert(*p, *offs);
    //used_mem.push(*offs);

//d_printf1("allocate 3\n");
    vmm_sm_blk_hdr *hdr = (vmm_sm_blk_hdr*)OFFS2ADDR(*offs);
	vmm_sm_blk_hdr::init(hdr);
    hdr->p = *p;
    hdr->roffs = *offs;
//printf("allocate 4\n");
    //d_printf1("bm_allocate_data_block: end\n");
}

void bm_allocate_tmp_block(session_id sid,
                           xptr /*out*/ *p,
                           ramoffs /*out*/ *offs,
                           xptr /*out*/ *swapped) throw (SednaException)
{
    tr_info_map::iterator it = trs.find(sid);
    if (it == trs.end()) throw USER_EXCEPTION(SE1018);

    new_tmp_block(p);

    push_to_persistent_used_blocks_stack(&(it->second->allocated_tmp_blocks), *p);
	//it->second->allocated_tmp_blocks.print();

    *swapped = put_block_to_buffer(sid, *p, offs, false);

    //*swapped = get_free_buffer(sid, offs);
    //buffer_table.insert(*p, *offs);
    //used_mem.push(*offs);

    vmm_sm_blk_hdr *hdr = (vmm_sm_blk_hdr*)OFFS2ADDR(*offs);
	vmm_sm_blk_hdr::init(hdr);
    hdr->p = *p;
    hdr->roffs = *offs;
}

void bm_delete_block(session_id sid,
                     xptr p) throw (SednaException)
{
    //d_printf1("sm_delete_block: begin\n");
    tr_info_map::iterator it = trs.find(sid);
    if (it == trs.end()) throw USER_EXCEPTION(SE1018);

    int res = 0;
    ramoffs offs;

    res = buffer_table.find_remove(p, offs);
    if (res == 0) 
    {
        used_mem.find_remove(offs);
        free_mem.push(offs);
    }

    if (IS_DATA_BLOCK(p))
    {
        push_to_persistent_free_blocks_stack(&(it->second->freed_data_blocks), p);
    }
    else 
    {
        // we will delete it on unregister transaction or by calling special method using list of allocated tmp blocks
    }
    //d_printf1("sm_delete_block: end\n");
}

void bm_get_block(session_id sid,
                  xptr p,
                  ramoffs /*out*/ *offs,
                  xptr /*out*/ *swapped) throw (SednaException)
{
    *swapped = put_block_to_buffer(sid, p, offs);
}

void bm_enter_exclusive_mode(session_id sid,
                             int *number_of_potentially_allocated_blocks) throw (SednaException)
{
    if (xmode_sid != -1) throw SYSTEM_EXCEPTION("Exclusive mode already obtained by another transaction");

    xmode_sid = sid;

    *number_of_potentially_allocated_blocks = bufs_num - max_trs_num;
}

void bm_exit_exclusive_mode(session_id sid) throw (SednaException)
{
    if (xmode_sid != sid) throw SYSTEM_EXCEPTION("Transaction is not in exclusive mode");

    xmode_sid = -1;

    ramoffs offs = 0;
    while (blocked_mem.pop(offs) == 0) used_mem.push(offs);
}

void bm_memlock_block(session_id sid, xptr p) throw (SednaException)
{
    if (xmode_sid != sid) throw SYSTEM_EXCEPTION("Transaction is not in exclusive mode");

    int res = 0;
    ramoffs offs = 0;

    res = buffer_table.find(p, offs);
    if (res == 0) 
    { // we have found the block in memory
        res = blocked_mem.find(offs);
        if (res == 0) return; // block already blocked

        if (blocked_mem.size() >= bufs_num - max_trs_num)
            throw USER_EXCEPTION(SE1020);
    }
    else throw USER_EXCEPTION(SE1021);

    res = used_mem.find_remove(offs);
    if (res != 0)
        throw SYSTEM_EXCEPTION("Error working with internal structures");

    res = blocked_mem.push(offs);
    if (res != 0)
        throw SYSTEM_EXCEPTION("Error working with internal structures");
}

void bm_memunlock_block(session_id sid, xptr p) throw (SednaException)
{
    if (xmode_sid != sid) throw SYSTEM_EXCEPTION("Transaction is not in exclusive mode");

    int res = 0;
    ramoffs offs = 0;

    res = buffer_table.find(p, offs);
    if (res == 0)
    {
        res = blocked_mem.find_remove(offs);
        if (res != 0)
            throw SYSTEM_EXCEPTION("Transaction is trying to unlock block that has not been locked");

        res = used_mem.push(offs);
        if (res != 0)
        throw SYSTEM_EXCEPTION("Error working with internal structures");
    }
    else
        throw SYSTEM_EXCEPTION("Transaction is trying to unlock block that has not been locked");
}

void bm_pseudo_allocate_data_block(session_id sid,
                                   xptr /*out*/ *p) throw (SednaException)
{
    new_data_block(p);
}

void bm_pseudo_delete_data_block(session_id sid,
                                 xptr p) throw (SednaException)
{
    tr_info_map::iterator it = trs.find(sid);
    if (it == trs.end()) throw USER_EXCEPTION(SE1018);

    push_to_persistent_free_blocks_stack(&(mb->free_data_blocks), p);

    // !!! MASTER BLOCK HAS BEEN CHANGED
}

void bm_block_statistics(sm_blk_stat *stat) throw (SednaException)
{
    stat->free_data_blocks_num = count_elems_of_persistent_free_blocks_stack(mb->free_data_blocks);
    stat->free_tmp_blocks_num = count_elems_of_persistent_free_blocks_stack(mb->free_tmp_blocks);

    stat->used_data_blocks_num = (int)((mb->data_file_cur_size - (__int64)PAGE_SIZE) / (__int64)PAGE_SIZE) - stat->free_data_blocks_num;
    stat->used_tmp_blocks_num = (int)(mb->tmp_file_cur_size / (__int64)PAGE_SIZE) - stat->free_tmp_blocks_num;
}


