/*
 * File:  bm_functions.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include <string>
#include "common/xptr/sm_vmm_data.h"
#include "sm/bufmgr/bm_functions.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/blk_mngmt.h"
#include "sm/sm_globals.h"
#include "common/errdbg/d_printf.h"

using namespace std;

/*******************************************************************************
********************************************************************************
  ACTIVATE/DEACTIVATE FUNCTIONS
********************************************************************************
*******************************************************************************/

#define EBS_WORKING_SET_SIZE	(20 * 1024 * 1024)
static size_t MinimumWorkingSetSize_orig = 0, MaximumWorkingSetSize_orig = 0;
static int lock_memory = 1;

static bool is_recovery_mode = false;


void _bm_set_working_set_size()
{
    if ( (unsigned)sm_globals::bufs_num >= (SIZE_MAX - EBS_WORKING_SET_SIZE)/PAGE_SIZE) {
        elog(EL_WARN, ("Can't set working set size. Too big bufs-num value is provided."));
        return;
    }

    size_t working_set_size = (size_t)sm_globals::bufs_num * PAGE_SIZE + EBS_WORKING_SET_SIZE;
    int res = 0;

    res = uGetCurProcessWorkingSetSize(
                        &MinimumWorkingSetSize_orig,
                        &MaximumWorkingSetSize_orig,
                        __sys_call_error);
    if (res != 0)
        elog(EL_WARN, ("Can't get working set size. Possibly, there are not enough privileges."));

    res = uSetCurProcessWorkingSetSize(
                        working_set_size,
                        working_set_size,
                        __sys_call_error);

    if (res != 0)
        elog(EL_WARN, ("Can't set working set size. Possibly, there are not enough privileges."));
}

void _bm_restore_working_set_size()
{
    if(MaximumWorkingSetSize_orig == 0 && MinimumWorkingSetSize_orig == 0)
        return;

    int res = 0;
    res = uSetCurProcessWorkingSetSize(
                        MinimumWorkingSetSize_orig,
                        MaximumWorkingSetSize_orig,
                        __sys_call_error);

    if (res != 0)
        elog(EL_WARN, ("Can't restore working set size. Possibly, there are not enough privileges."));
}

void _bm_init_buffer_pool()
{
    _bm_set_working_set_size();

    if ( (unsigned)sm_globals::bufs_num >= SIZE_MAX / PAGE_SIZE)
        throw USER_EXCEPTION2(SE1015, "Too big buffers number value.");
    
    size_t buffer_size = (size_t)sm_globals::bufs_num * PAGE_SIZE;
    
    if (uCreateShMem(&file_mapping, CHARISMA_BUFFER_SHARED_MEMORY_NAME,  buffer_size , NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION(SE1015);

#ifndef _WIN32
    /* we don't want any guarantees for now, we'll manually call the service later */
    file_mapping.to_guarantee = 0;
#endif

    buf_mem_addr = uAttachShMem(&file_mapping, NULL, 0,  __sys_call_error);
    if (buf_mem_addr == NULL)
        throw USER_EXCEPTION2(SE1015, "Cannot map view of file.");

    if (lock_memory)
    {
        if (uMemLock(buf_mem_addr, buffer_size, __sys_call_error) == -1)
        {
#ifndef _WIN32
            elog(EL_WARN, ("Can't lock memory. It is not supported without root, RLIMIT_MEMLOCK exceeded or there are not enough system resources."));
            /* we want to guarantee all the buffer pool though */
            if (uGuaranteeSharedMemory(buf_mem_addr, buffer_size, __sys_call_error))
                throw USER_EXCEPTION(SE1015);
#else
            elog(EL_WARN, ("Can't lock memory. Process does not have enouth privileges."));
#endif
            lock_memory = 0;
        }
    }

    for (int i = 0; i < sm_globals::bufs_num; i++)
        free_mem.push((size_t)i * PAGE_SIZE);
}

void _bm_release_buffer_pool()
{
    buffer_table.clear();
    free_mem.clear();
    used_mem.clear();
    blocked_mem.clear();

    size_t buffer_size = (size_t)sm_globals::bufs_num * PAGE_SIZE;
    
    if (lock_memory)
    {
        if (uMemUnlock(buf_mem_addr, buffer_size, __sys_call_error) == -1)
            throw USER_ENV_EXCEPTION("Cannot release system structures", false);
    }

    if (uDettachShMem(&file_mapping, buf_mem_addr, __sys_call_error) != 0)
        throw USER_ENV_EXCEPTION("Cannot release system structures", false);

    buf_mem_addr = NULL;

    if (uReleaseShMem(&file_mapping, CHARISMA_BUFFER_SHARED_MEMORY_NAME, __sys_call_error) != 0)
        throw USER_ENV_EXCEPTION("Cannot release system structures", false);

    _bm_restore_working_set_size();
}

void bm_startup()
{
    /*
     * Open data and tmp files
     *
     * We MUST open data file with WRITE_THROUGH option since when flushing
     * versions the order of flushing matters. One example would be flushing
     * relocated persistent version. We absolutely MUST flush relocated version
     * first and only then newly created version. We can guarantee it only
     * if we use WRITE_THROUGH logic. Otherwise recovery may fail in case
     * newly created version is flushed, but persistent is still in buffers.
     *
     * For now, we're trying to refine it somehow. Consider example above. We'll
     * do occasional fsyncs, to guarantee consistency. In this case, we can open
     * data file without O_SYNC.
     *
     * TODO: we should make other strategies available. For example, choosing
     * between O_SYNC/fsync would be nice, either on configuration or user level
     *
     * For tmp file defaults would be sufficient, of course.
     */
    string data_file_name = string(sm_globals::db_files_path) +
            string(sm_globals::db_name) + ".sedata";
    data_file_handler = uOpenFile(data_file_name.c_str(), U_SHARE_READ,
            U_READ_WRITE, 0, __sys_call_error);
    if (data_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION2(SE4042, data_file_name.c_str());

    string tmp_file_name = string(sm_globals::db_files_path) +
            string(sm_globals::db_name) + ".setmp";
    tmp_file_handler = uOpenFile(tmp_file_name.c_str(), U_SHARE_READ,
            U_READ_WRITE, 0, __sys_call_error);
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

    if (USemaphoreCreate(&cat_nametable_sem, 1, 1, CATALOG_NAMETABLE_SEMAPHORE_STR, NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4010, "CATALOG_NAMETABLE_SEMAPHORE_STR");

    if (USemaphoreCreate(&cat_master_sem, 1, 1, CATALOG_MASTER_SEMAPHORE_STR, NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4010, "CATALOG_MASTER_SEMAPHORE_STR");

    // Create shared memory
    if (uCreateShMem(&p_sm_callback_file_mapping, CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME, sizeof(xptr) + sizeof(int), NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4016, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

    p_sm_callback_data = uAttachShMem(&p_sm_callback_file_mapping, NULL, 0, __sys_call_error);
    if (p_sm_callback_data == NULL)
        throw USER_EXCEPTION2(SE4023, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");
#ifdef LRU
    if (uCreateShMem(&lru_global_stamp_file_mapping, CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME, sizeof(LRU_stamp), NULL, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4016, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");

    lru_global_stamp_data = (LRU_stamp*)uAttachShMem(lru_global_stamp_file_mapping, NULL, 0, __sys_call_error);
    if (lru_global_stamp_data == NULL)
        throw USER_EXCEPTION2(SE4023, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");
    *lru_global_stamp_data = 0;
#endif

    // init physical xptrs table
    phys_xptrs = se_new t_xptr_info(sm_globals::bufs_num);

    mb = (bm_masterblock*)(((uintptr_t)bm_master_block_buf + MASTER_BLOCK_SIZE) / MASTER_BLOCK_SIZE * MASTER_BLOCK_SIZE);
    read_master_block();

    LAYER_ADDRESS_SPACE_SIZE = mb->layer_size;

    U_ASSERT(LAYER_ADDRESS_SPACE_SIZE != 0);
}

void bm_shutdown()
{
    flush_buffers();

    flush_master_block();
    d_printf1("Flush master block: complete\n");

    if (uDettachShMem(&p_sm_callback_file_mapping, p_sm_callback_data, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4024, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

    p_sm_callback_data = NULL;

    if (uReleaseShMem(&p_sm_callback_file_mapping, CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4020, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");
#ifdef LRU
    if (uDettachShMem(&lru_global_stamp_file_mapping, lru_global_stamp_data, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4024, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");

    if (uReleaseShMem(&lru_global_stamp_file_mapping, CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4020, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");
#endif
    d_printf1("Release shared memory: complete\n");



    if (USemaphoreRelease(vmm_sm_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "VMM_SM_SEMAPHORE_STR");

    if (USemaphoreRelease(xmode, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "VMM_SM_EXCLUSIVE_MODE_SEM_STR");

    if (USemaphoreRelease(cat_nametable_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "CATALOG_NAMETABLE_SEMAPHORE_STR");

    if (USemaphoreRelease(cat_master_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4011, "CATALOG_MASTER_SEMAPHORE_STR");

    d_printf1("Release semaphores: complete\n");

    _bm_release_buffer_pool();
    d_printf1("Release shared memory: complete\n");


    if (uCloseFile(data_file_handler, __sys_call_error) == 0)
        throw USER_EXCEPTION2(SE4043, ".sedata file");

    if (uCloseFile(tmp_file_handler, __sys_call_error) == 0)
        throw USER_EXCEPTION2(SE4043, ".setmp file");
    d_printf1("Close database files: complete\n");

    tr_info_map::iterator it;
    for (it = trs.begin(); it != trs.end(); it++)
    {
        if (USemaphoreRelease(it->second->sm_to_vmm_callback_sem1, __sys_call_error) != 0)
            throw SYSTEM_ENV_EXCEPTION("Cannot release SM_TO_VMM_CALLBACK_SEM1_BASE_STR");

        if (USemaphoreRelease(it->second->sm_to_vmm_callback_sem2, __sys_call_error) != 0)
            throw SYSTEM_ENV_EXCEPTION("Cannot release SM_TO_VMM_CALLBACK_SEM2_BASE_STR");

        tr_info *info = it->second;
        trs.erase(it);
        delete (info);
    }

    // release phys xptrs table
    se_delete(phys_xptrs);
}

void bm_register_session(session_id sid, int is_rcv_mode)
{
    tr_info_map::iterator it = trs.find(sid);
    if (it != trs.end()) throw USER_EXCEPTION(SE1018);

    char buf[100];
    USemaphore sm_to_vmm_callback_sem1, sm_to_vmm_callback_sem2;

    d_printf2("Register session with sid = %d\n", sid);

    if (USemaphoreCreate(&sm_to_vmm_callback_sem1, 0, 1, SM_TO_VMM_CALLBACK_SEM1_BASE_STR(sid, buf, 100), NULL, __sys_call_error) != 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot create SM_TO_VMM_CALLBACK_SEM1_BASE_STR");

    if (USemaphoreCreate(&sm_to_vmm_callback_sem2, 0, 1, SM_TO_VMM_CALLBACK_SEM2_BASE_STR(sid, buf, 100), NULL, __sys_call_error) != 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot create SM_TO_VMM_CALLBACK_SEM2_BASE_STR");

    tr_info *ti = new tr_info;
    ti->sm_to_vmm_callback_sem1 = sm_to_vmm_callback_sem1;
    ti->sm_to_vmm_callback_sem2 = sm_to_vmm_callback_sem2;
    ti->freed_data_blocks = XNULL;
    ti->allocated_tmp_blocks = XNULL;

    trs.insert(tr_info_map::value_type(sid, ti));

    if (is_rcv_mode) is_recovery_mode = true;
}

void bm_unregister_session(session_id sid)
{
    d_printf2("Unregister session with sid = %d\n", sid);
    tr_info_map::iterator it = trs.find(sid);
    if (it == trs.end()) throw USER_EXCEPTION(SE1018);

	/*	A workaround to wakeup VMM callback thread when session is unregistered.
		We can't do it in TRN since a semaphore is destroyed as a part of bm_unregister_session
		call. We can't shutdown VMM callback thread *before* calling bm_unregister_session
		because SM can callback TRN *after* the callback thread is already gone and will lock forever. */
	/* if (USemaphoreUp(it->second->sm_to_vmm_callback_sem1, __sys_call_error) != 0)
		throw SYSTEM_ENV_EXCEPTION("Cannot up SM_TO_VMM_CALLBACK_SEM1_BASE_STR"); */
	xptr special = {0, (lsize_t)-1};

	unmap_block_in_tr(special, it->second, true);

    if (USemaphoreRelease(it->second->sm_to_vmm_callback_sem1, __sys_call_error) != 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot release SM_TO_VMM_CALLBACK_SEM1_BASE_STR");

    if (USemaphoreRelease(it->second->sm_to_vmm_callback_sem2, __sys_call_error) != 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot release SM_TO_VMM_CALLBACK_SEM2_BASE_STR");

	tr_info *info = it->second;
	trs.erase(it);
    delete (info);

    is_recovery_mode = false;
}

void bm_register_transaction(session_id sid, transaction_id trid)
{
    // for now trid is not used in any extent
    d_printf2("Register transaction with trid = %d\n", trid);
}

void bm_unregister_transaction(session_id sid, transaction_id trid)
{
    // for now trid is not used in any extent
    d_printf2("Unregister transaction with trid = %d\n", trid);
    tr_info_map::iterator it = trs.find(sid);
    if (it == trs.end()) throw USER_EXCEPTION(SE1018);

    xptr p = XNULL;
	tr_info *info = it->second;

	//info->freed_data_blocks.print();
    while (pop_from_persistent_free_blocks_stack(&(info->freed_data_blocks), &p) == 0)
        delete_data_block(p);

	//info->allocated_tmp_blocks.print();
    while (pop_from_persistent_used_blocks_stack(&(info->allocated_tmp_blocks), &p) == 0)
        delete_tmp_block(p, info);
}

void bm_delete_tmp_blocks(session_id sid)
{
    tr_info_map::iterator it = trs.find(sid);
    if (it == trs.end()) throw USER_EXCEPTION(SE1018);

    xptr p = XNULL;
	tr_info *info = it->second;

    while (pop_from_persistent_used_blocks_stack(&(info->allocated_tmp_blocks), &p) == 0)
        delete_tmp_block(p, info);
}

void bm_allocate_data_block(session_id sid,
                            xptr /*out*/ *p,
                            ramoffs /*out*/ *offs,
                            xptr /*out*/ *swapped)
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
	hdr->is_changed = true;
//printf("allocate 4\n");
    //d_printf1("bm_allocate_data_block: end\n");
}

void bm_allocate_tmp_block(session_id sid,
                           xptr /*out*/ *p,
                           ramoffs /*out*/ *offs,
                           xptr /*out*/ *swapped)
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
	hdr->is_changed = true;
}

void bm_delete_block(session_id sid,
                     xptr p)
{
    //d_printf1("sm_delete_block: begin\n");
    tr_info_map::iterator it = trs.find(sid);
    if (it == trs.end()) throw USER_EXCEPTION(SE1018);

    int res = 0;
    ramoffs offs;

    res = buffer_table.find_remove(p, &offs);
    if (res == 0)
    {
        used_mem.find_remove(offs);
        free_mem.push(offs);
        phys_xptrs->at(offs/PAGE_SIZE)=xptr();
    }

    if (IS_DATA_BLOCK(p))
    {
        bool approved = unmap_block_in_trs(sid, p, true);
        if (!approved)
           throw SYSTEM_EXCEPTION("Trying to delete data block which is used in another transaction");
        if (is_recovery_mode)
        {
            push_to_persistent_free_blocks_stack(&(mb->free_data_blocks), p);
        }
        else
        {
            push_to_persistent_free_blocks_stack(&(it->second->freed_data_blocks), p);
        }
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
                  xptr /*out*/ *swapped)
{
    *swapped = put_block_to_buffer(sid, p, offs);
}

void bm_enter_exclusive_mode(session_id sid,
                             int *number_of_potentially_allocated_blocks)
{
    if (xmode_sid != -1) throw SYSTEM_EXCEPTION("Exclusive mode already obtained by another transaction");

    xmode_sid = sid;

    *number_of_potentially_allocated_blocks = sm_globals::bufs_num;
}

void bm_exit_exclusive_mode(session_id sid)
{
    if (xmode_sid != sid) throw SYSTEM_EXCEPTION("Transaction is not in exclusive mode");

    xmode_sid = -1;

    ramoffs offs = 0;
    while (blocked_mem.pop(&offs) == 0) used_mem.push(offs);
}

void bm_memlock_block(session_id sid, xptr p)
{
    if (xmode_sid != sid) throw SYSTEM_EXCEPTION("Transaction is not in exclusive mode");

    int res = 0;
    ramoffs offs = 0;

    res = buffer_table.find(p, &offs);
    if (res == 0)
    { // we have found the block in memory
        res = blocked_mem.find(offs);
        if (res == 0) return; // block already blocked

        if (blocked_mem.size() >= (unsigned)sm_globals::bufs_num)
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

void bm_memunlock_block(session_id sid, xptr p)
{
    if (xmode_sid != sid) throw SYSTEM_EXCEPTION("Transaction is not in exclusive mode");

    int res = 0;
    ramoffs offs = 0;

    res = buffer_table.find(p, &offs);
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
/*
void bm_pseudo_allocate_data_block(session_id sid,
                                   xptr *p)
{
    new_data_block(p);
}

void bm_pseudo_delete_data_block(session_id sid,
                                 xptr p)
{
    tr_info_map::iterator it = trs.find(sid);
    if (it == trs.end()) throw USER_EXCEPTION(SE1018);

    push_to_persistent_free_blocks_stack(&(mb->free_data_blocks), p);

    // !!! MASTER BLOCK HAS BEEN CHANGED
}
*/
void bm_block_statistics(sm_blk_stat *stat)
{
    stat->free_data_blocks_num = count_elems_of_persistent_free_blocks_stack(mb->free_data_blocks);
    stat->free_tmp_blocks_num = count_elems_of_persistent_free_blocks_stack(mb->free_tmp_blocks);

    stat->used_data_blocks_num = (mb->data_file_cur_size - PAGE_SIZE) / PAGE_SIZE - stat->free_data_blocks_num;
    stat->used_tmp_blocks_num  = (mb->tmp_file_cur_size / PAGE_SIZE) - stat->free_tmp_blocks_num;
}
