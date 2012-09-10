/*
 * File:  sm_vmm_data.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SM_VMM_DATA_H
#define _SM_VMM_DATA_H

#include "common/sedna.h"
#include "common/base.h"
#include "common/xptr/xptr.h"
#include "common/xptr/wustructures.h"
#include "common/llcommon/lfsGlobals.h"
#include "common/lockmantypes.h"

static global_name layerSizeReturn        = {GN_DATABASE, "layer_size_return"};
static global_name smMessageServerName    = {GN_DATABASE, "sm_msg_queue"};
static global_name transactionLockName    = {GN_SESSION, "trn_lock"};
static global_name bufferSharedMemoryName = {GN_DATABASE, "buffer_memory"};
static global_name xmodeSemaphoreName     = {GN_DATABASE, "xmode_sem"};
static global_name vmmLockName            = {GN_DATABASE, "vmm_lock"}; /* SM_VMM_SEMAPHORE */
static global_name catNametableLockName   = {GN_DATABASE, "catalog_nametable"};
static global_name catMasterLockName      = {GN_DATABASE, "catalog_masterlock"};
static global_name smCallbackName         = {GN_DATABASE, "sm_callback"}; /* SM_CALLBACK */
static global_name lruCounterName         = {GN_DATABASE, "lru_counter"}; /* LRU_SHMEM */
static global_name smCallbackSem1         = {GN_SESSION, "sm_callback_sem_1"}; /* SM_CALLBACK_SEM1 */
static global_name smCallbackSem2         = {GN_SESSION, "sm_callback_sem_2"}; /* SM_CALLBACK_SEM2 */
static global_name smShmMappingName       = {GN_DATABASE, "sm_shm_mapping"}; /* SEDNA_GLOBAL_MEMORY_MAPPING */

struct vmm_sm_blk_hdr
{
    xptr p;		// the first 4 bytes is the layer of the block
    LSN lsn;
	VersionsHeader versionsHeader;

	int blockType;  // (c) A. Kalinin
	ramoffs roffs;	// address of block in buffer memory
    int is_changed;
	int trid_wr_access; /* session id (sid) is actually stored here for historical reasons */

	static void init(vmm_sm_blk_hdr *hdr)
	{
		hdr->p = XNULL;
		hdr->lsn = LFS_INVALID_LSN;
		memset(hdr->versionsHeader.xptr, 0, sizeof hdr->versionsHeader.xptr);
		memset(hdr->versionsHeader.creatorTs, -1, sizeof hdr->versionsHeader.creatorTs);
		hdr->roffs = 0;
		memcpy(&hdr->blockType, "AKNZ", 4);
		hdr->is_changed = false;
		hdr->trid_wr_access = -1;
	}
};

/* Hot-Backup states and answers in messages */
enum hb_state
{
    HB_START,               // start hot-backup
    HB_START_CHECKPOINT,    // start hot-backup with preceding checkpoint
    HB_START_INCR,          // make primary increment copy
    HB_ADD_INCR,            // archive another portion of the db
    HB_STOP_INCR,           // disable increment mode for the given database
    HB_NONE_INCR,           // non-increment mode
    HB_CONT,                // sm answer: can continue
    HB_WAIT,                // answer: wait for checkpoint to finish
    HB_ARCHIVELOG,          // archive logical log (switch to the next one)
    HB_END,                 // end of the hot-backup process
    HB_ERR,                 // some error from sm
    HB_NEXTFILE,            // file request from hbp
    HB_GETPREVLOG           // get previous log file number
};

/**
 *
 * cmd list:
 * =========
 * response list:
 * ~~~~~~~~~~~~~~
 * 0 - success
 * otherwise (int > 0) - fail
 *
 * query list:
 * ~~~~~~~~~~~
 * 10 - soft shutdown
 * 11 - hard shutdown
 *
 * 21 - bm_register_session
 * 22 - bm_unregister_session
 * 23 - bm_allocate_data_block (xptr, offs)
 * 24 - bm_allocate_tmp_block (xptr, offs)
 * 25 - bm_delete_block (xptr)
 * 26 - bm_get_block (xptr, offs)
 * 27 - bm_enter_exclusive_mode (num)
 * 28 - bm_exit_exclusive_mode
 * 29 - bm_memlock_block (xptr)
 * 30 - bm_memunlock_block (xptr)
 * 31 - bm_block_statistics (sm_blk_stat)
 * 32 - bm_pseudo_allocate_data_block (xptr)
 * 33 - bm_pseudo_delete_data_block (xptr)
 * 34 - bm_delete_tmp_blocks
 * 35 - bm_register_transaction
 * 36 - bm_unregister_transaction
 * 37 - bm_create_new_version
 * 38 - transaction rollback
 * 39 - hot-backup procedure (receive: state, return: status, or log file numbers; use hb_struct for this)
 */

struct sm_blk_stat
{
    int64_t free_data_blocks_num;
    int64_t free_tmp_blocks_num;
    int64_t used_data_blocks_num;
    int64_t used_tmp_blocks_num;
};

enum sm_msg_messages {
    msg_ok = 0,

    msg_request_transaction_id = 1,
    msg_release_transaction_id = 2,

    msg_lock_object = 3,
    msg_release_locks = 4,
    msg_unlock_object = 5,

    msg_register_session = 21,
    msg_unregister_session = 22,

    msg_alloc_data_block = 23,
    msg_alloc_tmp_block = 24,
    msg_release_block = 25,
    msg_unswap_block = 26,

    msg_enter_excl_mode = 27,
    msg_exit_excl_mode = 28,
    msg_lock_block = 29,
    msg_unlock_block = 30,

    msg_get_stats = 31,

    msg_delete_tmp_blocks = 34,

    msg_register_transaction = 35,
    msg_unregister_transaction = 36,

    msg_create_version = 37,
    msg_transaction_finish_notification = 38,
    msg_hot_backup = 39,

    msg_error_max_data_file_size = 101,
    msg_error_max_tmp_file_size = 102,
    msg_error_extending_data_file = 103,
    msg_error_extending_tmp_file = 104,
    msg_error_transaction_exists = 105,
    msg_error_transaction_not_found = 106,
    msg_error_locked_blocks_limit = 107,
    msg_error_block_not_found = 108,
    
    msg_error = 255,
};

struct sm_msg_struct
{
    // cmd (what do you want to do)
    sm_msg_messages cmd;
    // transaction identifier
    transaction_id trid;

    //identifier of session
    session_id sid;

    // additional parameters
    union {
        struct {
            int num; // number of potentially allocated blocks in call to bm_enter_exclusive_mode
            xptr mptr; // pointer for catalog master block
            int transaction_flags;
            lsize_t layer_size; // layer size to report to trn
        } reg;

        xptr ptr; // xptr for deletion, locking and unlocking

        struct {
            xptr ptr;
            xptr swapped;
            ramoffs offs;
        } swap_data;

        struct {
            uint64_t lnumber;
            hb_state state;
            bool is_checkp;
            hb_state incr_state;
            TIMESTAMP ts;
        } hb_struct;

        bool want_snapshot;
        bool is_rollback;
        int64_t snp_ts;   // timestamp of snapshot, not used currently, but may be helpful later

        sm_blk_stat stat; // sm block statistics

        struct {
            lock_reply result;
            lock_mode mode;
            resource_kind rkind;
            char name[MAX_RESOURCE_NAME_LENGTH];
        } lock;

        struct {
            bool rdonly;
            bool excl;
        } trinfo;
    } data;
};

#endif
