/*
 * File: vmm.cpp
 * Copyright (C) ISP RAS 2010
 * The Institute for System Programming of the Russian Academy of Sciences
 *
 * Implementation of Virtual Memory Manager.
 * Provides interface for working with persistent memory - data in blocks.
 */

#include <set>

#include "common/sedna.h"

#include "common/u/usem.h"
#include "common/u/ushm.h"
#include "common/u/ummap.h"
#include "common/u/uthread.h"
#include "common/u/usafesync.h"
#include "common/utils.h"
#include "common/sm_vmm_data.h"
#include "common/bit_set.h"
#include "common/gmm.h"
#include "common/errdbg/d_printf.h"
#include "common/XptrHash.h"

#include "tr/vmm/vmm.h"
#include "tr/vmm/vmminternal.h"
#include "tr/tr_globals.h"
#include "tr/cat/catvars.h"

#define VMM_REGION_SEARCH_MAX_SIZE                      ((uint32_t)0x79C00000)
#define VMM_REGION_MAX_SIZE                             ((uint32_t)0x40000000)

static bool vmm_session_initialized = false;
static bool vmm_transaction_initialized = false;

static UShMem file_mapping;

/* Active block complete address */
xptr vmm_cur_xptr;

#ifdef VMM_DEBUG_CHECKP
xptr vmm_checkp_xptr;
#endif /* VMM_DEBUG_CHECKP */

/*
 * Active block offset. We need it since we can't guarantee atomicity of
 * assignment for vmm_cur_xptr variable and we don't want to use spin locks
 * in every CHECKP. At least now ...
 * Note, since we have to guarantee atomicity and memory barrier for this
 * variable it's better not to use void* or something else. Type should be the
 * same as for xptr's offset and must provide atomic assignment for supported
 * architectures and operating systems. Now we use uint32_t since it's likely
 * to be atomic. Anyway we also use IntelockedExchange and analogs to guarantee
 * atomicity and memory barrier.
 */
volatile lsize_t vmm_cur_offs;

/* vmm_callback thread */
static volatile bool shutdown_vmm_thread = false;

#define VMM_THREAD_STACK_SIZE       102400

static UTHANDLE vmm_thread_handle;  // handle to vmm thread
static UTHANDLE main_thread;        // handle to main thread

/* Pointer to SM communication manager */
static SSMMsg *ssmmsg;
static sm_msg_struct msg;

// semaphore for global VMM/SM synchronization
static USemaphore vmm_sm_sem;

/* vmm_callback semaphores and shared memory */
static UShMem p_sm_callback_file_mapping;
static void * p_sm_callback_data;
static USemaphore sm_to_vmm_callback_sem1, sm_to_vmm_callback_sem2; // callback semaphores for SM

static bit_set * mapped_pages = NULL;
static bit_set * mtrBlocks = NULL;
static VMMMicrotransaction * activeMT = NULL;


void __vmm_set_sigusr_handler();

/* Sets current pointer to XNULL (gently) */
inline static void __vmm_init_current_xptr()
{
#ifdef VMM_LINUX_DEBUG_CHECKP
    if (vmm_cur_xptr != XNULL) {
        mprotect(XADDR(vmm_cur_xptr), PAGE_SIZE, PROT_NONE);
    }
#endif /* VMM_LINUX_DEBUG_CHECKP */

    vmm_cur_xptr = XNULL;
    vmm_cur_offs = 0;
}

static void vmm_remap(void *addr, ramoffs offs, enum vmm_map_protection_t p)
{
    int i = (int)(((char *)addr - (char *)LAYER_ADDRESS_SPACE_START_ADDR) / PAGE_SIZE);
    mapped_pages->setAt(i);

    if (activeMT != NULL) { mtrBlocks->setAt(i); }

    if (_uvmm_unmap(addr) || _uvmm_map(addr, offs, &file_mapping, p)) {
        throw USER_EXCEPTION(SE1035);
    }
}

void vmm_unmap(void *addr)
{
    int i = (int)(((char *)addr - (char *)LAYER_ADDRESS_SPACE_START_ADDR) / PAGE_SIZE);

    if (mapped_pages->testAt(i)) {
        mapped_pages->clearAt(i);

        if (_uvmm_unmap(addr) || _uvmm_map(addr, 0, &global_memory_mapping, access_null)) {
            throw USER_EXCEPTION(SE1035);
        }
    }
}

/*
 * If you call it in the VMM_LINUX_DEBUG_CHECKP mode make sure
 * that block at vmm_cur_offs is readable. It's not true
 * only in one case: when it's called from the vmm thread
 * when the main thread hasn't finished CHECKP yet.
 */
inline static void vmm_swap_unmap_conditional(const xptr p) {
    if (p == XNULL) return;

#ifdef VMM_LINUX_DEBUG_CHECKP
    /*
     * Block at the vmm_cur_offs must be readable at least. Actually,
     * it's not true when call is made by vmm_thread, but in that case
     * it's guaranteed that vmm_cur_offs != p.getOffs().
     * Since p.getOffs() == ALIGN_OFFS(vmm_cur_offs) can be true only
     * when call is made from the main thread it's guaranteed also that
     * vmm_cur_xptr == XNULL check is consistent.
     */
    if (p.getOffs() != ALIGN_OFFS(vmm_cur_offs) || vmm_cur_xptr == XNULL) {
        mprotect(XADDR(p), PAGE_SIZE, PROT_READ);
    }
#endif /* VMM_LINUX_DEBUG_CHECKP */

    if ((*(xptr *) XADDR(p)) == p) {
        vmm_unmap(XADDR(p));
    }

#ifdef VMM_LINUX_DEBUG_CHECKP
    /* Revert protection level if it was changed.
     * Any block except current must be set as PROT_NONE */
    if (p.getOffs() != ALIGN_OFFS(vmm_cur_offs) || vmm_cur_xptr == XNULL) {
        mprotect(XADDR(p), PAGE_SIZE, PROT_NONE);
    }
#endif /* VMM_LINUX_DEBUG_CHECKP */
}


/* We always unmap the block that lay at the given address when
 * the block to be swapped may already be replaced by the other
 * one on SM by this call */
inline static void vmm_swap_unmap_unconditional(const xptr p) {
    if (p == XNULL) return;
    vmm_unmap(XADDR(p));
}


/* Locks the space for block mapping. This function needs
 * to be called as early as possible to prevent others from
 * locking this memory.
 *
 * I'm trying to move it below, in vmm_on_session_begin, according
 * to our plan of going only with per-base LAYER_SIZE.
 * In this case it's logical to receive it from se_sm.
 *
 * In case of any problems we should think of another solution
 */
void vmm_preliminary_call(lsize_t layer_size)
{
	int current_retry = 1;

	open_global_memory_mapping(SE4400);

    global_memory_mapping = get_global_memory_mapping();

	while (true) {

		if (__vmm_check_region(layer_size, &LAYER_ADDRESS_SPACE_START_ADDR,
			&LAYER_ADDRESS_SPACE_SIZE, false, NULL) != 1)
			throw SYSTEM_ENV_EXCEPTION("Cannot map vmm region in transaction!");

		/*
		 * At this point we have reserved the whole layer on Linux, but have it
		 * freed on Windows.
		 *
		 * There is a chance that kernel will load some library between unmap
		 * and map-of-null-pages calls. So these are "solutions":
		 *
		 * On Linux we can employ mmap-over-mmap technique, which seems to be
		 * supported on most platforms.
		 *
		 * For Windows, however, it seems impossible since we cannot do MapViewOfFile
		 * over reserved memory. We could try to postpone VirtualFree until later,
		 * but we perhaps will gain nothing, since we do null-page remapping only
		 * several instructions later. Another idea is to do VirtualFree-MapViewOfFile
		 * calls with page granularity instead of the whole layer.
		 */
		U_ASSERT(LAYER_ADDRESS_SPACE_SIZE == layer_size);

		LAYER_ADDRESS_SPACE_START_ADDR_INT = (uintptr_t)LAYER_ADDRESS_SPACE_START_ADDR;
		LAYER_ADDRESS_SPACE_BOUNDARY_INT = LAYER_ADDRESS_SPACE_START_ADDR_INT + layer_size;
		LAYER_ADDRESS_SPACE_BOUNDARY = (void *)LAYER_ADDRESS_SPACE_BOUNDARY_INT;

		mapped_pages = new bit_set(LAYER_ADDRESS_SPACE_SIZE / PAGE_SIZE); // constructor zeroes it
		mtrBlocks = new bit_set(LAYER_ADDRESS_SPACE_SIZE / PAGE_SIZE); // constructor zeroes it

		uintptr_t cur;
		bool success = true;
		for (cur = LAYER_ADDRESS_SPACE_START_ADDR_INT;
			cur < LAYER_ADDRESS_SPACE_BOUNDARY_INT;
			cur += (uint32_t)PAGE_SIZE)
		{
			if (_uvmm_map((void*)cur, 0, &global_memory_mapping, access_null) == -1)
			{
				success = false;
				if (current_retry < 5)
				{
					uintptr_t undo_cur;
					for (undo_cur = LAYER_ADDRESS_SPACE_START_ADDR_INT;
						undo_cur < cur;
						undo_cur += (uint32_t)PAGE_SIZE)
					{
						if (_uvmm_unmap((void*)undo_cur) == -1) 
						{
							throw USER_EXCEPTION(SE1031);
						}
					}

					++current_retry;
					elog(EL_WARN, ("preliminary call failed, retrying %d: layer address space size = 0x%x, "
						"layer start address = 0x%" PRIXPTR
						", failed address = 0x%" PRIXPTR,
						current_retry, LAYER_ADDRESS_SPACE_SIZE, LAYER_ADDRESS_SPACE_START_ADDR_INT, cur));
					break;
				}
				else 
				{
					throw USER_EXCEPTION(SE1031);
				}
			}
		}
		
		if (success) break;
	}

    elog(EL_DBG,  ("preliminary call: layer address space start addr = 0x%" PRIXPTR, LAYER_ADDRESS_SPACE_START_ADDR_INT));
    elog(EL_DBG,  ("preliminary call: layer address space size = 0x%x", LAYER_ADDRESS_SPACE_SIZE));
}

/* SM query check function */
void _vmm_process_sm_error(int cmd)
{
    if (cmd != 0)
    {
        switch (cmd)
        {
        case 21: throw USER_EXCEPTION(SE1011);
        case 22: throw USER_EXCEPTION(SE1012);
        case 23: throw USER_EXCEPTION(SE1013);
        case 24: throw USER_EXCEPTION(SE1014);
        case 25: throw USER_EXCEPTION(SE1018);
        case 26: throw USER_EXCEPTION(SE1019);
        case 27: throw USER_EXCEPTION(SE1020);
        case 28: throw USER_EXCEPTION(SE1021);
        case  1: throw USER_EXCEPTION(SE1023);
        default: throw SYSTEM_EXCEPTION("Unknown SM error");
        }
    }
}

inline static int send_sm_message(int cmd) {
    int result;

    msg.cmd = cmd; // bm_get_block
    msg.trid = tr_globals::trid;
    msg.sid  = tr_globals::sid;

    if ((result = ssmmsg->send_msg(&msg)) != 0) { return result; }
    if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

    return 0;
}

enum sm_command_t {
    smc_alloc_data = 23,
    smc_alloc_temp = 24,
    smc_delete_block = 25,
    smc_unswap_block = 26,
    smc_delete_temp_blocks = 34,
    smc_create_version = 37
};

/* Asking SM to read the block */
void vmm_unswap_block(xptr p)
{
    SafeSemaphore sem(vmm_sm_sem);

    check_bounds(p);
    sem.Aquire();
    p = block_xptr(p);

    msg.data.swap_data.ptr = p;
    if (send_sm_message(smc_unswap_block) != 0) { throw USER_EXCEPTION(SE1034); }

    ramoffs offs = msg.data.swap_data.offs;
    xptr swapped = msg.data.swap_data.swapped;

    VMM_TRACE_UNSWAP(p, swapped);

    vmm_swap_unmap_unconditional(swapped);
    vmm_remap(XADDR(p), offs, access_readonly);

    sem.Release();
}

/* Informing SM of writing to the block */
void vmm_unswap_block_write(xptr p)
{
    SafeSemaphore sem(vmm_sm_sem);

    check_bounds(p);
    sem.Aquire();
    p = block_xptr(p);

    msg.data.swap_data.ptr = p;
    if (send_sm_message(smc_create_version) != 0) { throw USER_EXCEPTION(SE1034); }

    ramoffs offs = msg.data.swap_data.offs;
    xptr swapped = msg.data.swap_data.swapped;

    VMM_TRACE_UNSWAP(p, swapped);

    vmm_swap_unmap_unconditional(swapped);
    vmm_remap(XADDR(p), offs, access_readwrite);

    sem.Release();
}

/* Allocate new block */
void vmm_request_alloc_block(xptr *p, bool is_data)
{
    SafeSemaphore sem(vmm_sm_sem);

    sem.Aquire();

    if (send_sm_message(is_data ? smc_alloc_data : smc_alloc_temp) != 0) { throw USER_EXCEPTION(SE1034); }

    *p = msg.data.swap_data.ptr;
    xptr swapped = msg.data.swap_data.swapped;
    ramoffs offs = *(ramoffs*)(&(msg.data.swap_data.offs));

    VMM_TRACE_ALLOC_BLOCK(*p, swapped);

    vmm_swap_unmap_unconditional(swapped);
    vmm_remap(XADDR(*p), offs, access_readonly);

    sem.Release();

    CHECKP(*p);
}

void vmm_delete_block(xptr p)
{
    SafeSemaphore sem(vmm_sm_sem);

    sem.Aquire();
    p = block_xptr(p);
    VMM_TRACE_DELETE_BLOCK(p);

    /* If layer of the current block at XADDR(p) is the same we must unmap it */
    vmm_swap_unmap_conditional(p);

    /* If current block is being deleted, the pointer may break something.
     * So we must reinit it. */
    if (same_block(vmm_cur_xptr, p)) { __vmm_init_current_xptr(); }

    /* Notify SM about deletion of the block */
    msg.data.ptr = p;
    if (send_sm_message(smc_delete_block) != 0) {
        throw USER_EXCEPTION(SE1034);
    }

    sem.Release();
}

void vmm_delete_tmp_blocks()
{
    SafeSemaphore sem(vmm_sm_sem);

    sem.Aquire();
    __vmm_init_current_xptr();

    if (send_sm_message(smc_delete_temp_blocks) != 0) {
        throw USER_EXCEPTION(SE1034);
    }

    sem.Release();
}

/* Log file for determine region */
static FILE * f_se_trn_log;
#define VMM_SE_TRN_LOG "se_trn_log"

void read_write_cdb_layer_size(lsize_t *data, bool write)
{
    UShMem p_cdb_callback_file_mapping;
    lsize_t *p_cdb_callback_data;

    if (uOpenShMem(&p_cdb_callback_file_mapping, CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4021, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

    p_cdb_callback_data = (lsize_t *)uAttachShMem(&p_cdb_callback_file_mapping, NULL, 0, __sys_call_error);
    if (p_cdb_callback_data == NULL)
        throw USER_EXCEPTION2(SE4023, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

    if (write)
        *p_cdb_callback_data = *data;
    else
        *data = *p_cdb_callback_data;

    if (uDettachShMem(&p_cdb_callback_file_mapping, p_cdb_callback_data, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4024, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

    if (uCloseShMem(&p_cdb_callback_file_mapping, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4022, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");
}
/*
 * vmm_determine_region is called the first time the Sedna server starts
 * to find active layer region.
 *
 * Some comments:
 *
 * - Mapping with readonly and both read/write permissions in some systems
 *   behave differently. Since vmm_determine_region intended only to find free
 *   continuous region it's better to use readonly access protection.
 * - Similarly, in Linux/UNIX it is better to use MAP_PRIVATE.
 * - MAP_NORESERVE in Linux can be used since we will never commit
 *   simultaneously more than database buffers size (100MiB is default).
 * - mmap() of /dev/zero can be used in systems on which MAP_ANON(YMOUS) flag
 *   is not supported. Example:
 *   int fd_dev_zero;  // Don't forget to properly close fd_dev_zero!
 *   if ((fd_dev_zero = open("/dev/zero", O_RDWR)) == -1)
 *   {
 *       perror("Can't open /dev/zero");
 *       if(log) fprintf(f_se_trn_log,
 *                       "Can't open /dev/zero.\nError: %d\n",
 *                       errno);
 *       else vmm_determine_region(true);
 *       return;
 *   }
 */
void vmm_determine_region(bool log)
{
    if (log) {
        f_se_trn_log = fopen(VMM_SE_TRN_LOG, "w");
        if (f_se_trn_log == NULL) {
            printf("Can't open file se_trn_log\n");
            return;
        }
    }

    lsize_t cur = 0;
    lsize_t segment_size = 0, start_segment_size;

    void *res_addr = NULL;

    read_write_cdb_layer_size(&start_segment_size, false /* read */);

    if (start_segment_size == 0)
        start_segment_size = VMM_REGION_SEARCH_MAX_SIZE;

    for (cur = start_segment_size; cur >= VMM_REGION_MIN_SIZE; cur -= (uint32_t)PAGE_SIZE)
    {
        if (log) fprintf(f_se_trn_log, "Probing size %u... ", cur);
        if (__vmm_check_region(cur, &res_addr, &segment_size, log, f_se_trn_log)) { break; }
    }

    if (log) {
        if (0 == segment_size) fprintf(f_se_trn_log, "Nothing has been found\n");
        else fprintf(f_se_trn_log, "\nvmm_determine_region:\nregion size (in pages) = %u\nsystem given addr = %" PRIxPTR "\n", segment_size / (uint32_t)PAGE_SIZE, (uintptr_t)res_addr);
        if (fclose(f_se_trn_log) != 0) printf("Can't close file se_trn_log\n");
    } else {
        if (0 == segment_size) {
            printf("Nothing has been found\n");
            vmm_determine_region(true);
            throw USER_EXCEPTION(SE1040);
        }
        else
        {
            d_printf3("\nvmm_determine_region:\nregion size (in pages) = %u\nsystem given addr = %"PRIxPTR"\n", segment_size / (uint32_t)PAGE_SIZE, (uintptr_t)res_addr);

            if (segment_size > VMM_REGION_MAX_SIZE)
                segment_size = VMM_REGION_MAX_SIZE;

            // need to give the result back to cdb
            read_write_cdb_layer_size(&segment_size, true /* write */);
        }
    }
}

static uint64_t block_counter = 0;

void vmm_init_block_counter()
{
    block_counter = 0;
}

uint64_t vmm_get_block_counter() {
    return block_counter;
}


void vmm_alloc_data_block(xptr *p)
{
    vmm_request_alloc_block(p, true);
    ++block_counter;
}

void vmm_alloc_tmp_block(xptr *p)
{
    vmm_request_alloc_block(p, false);
}

void vmm_storage_block_statistics(sm_blk_stat /*out*/ *stat)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);

    try {
        msg.cmd = 31; // bm_block_statistics
        msg.trid = tr_globals::trid;
        msg.sid = tr_globals::sid;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

        *stat = msg.data.stat;
    } catch (ANY_SE_EXCEPTION) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }

    USemaphoreUp(vmm_sm_sem, __sys_call_error);
}

inline static void vmm_callback_unmap()
{
    /*
     * We check only vmm_cur_offs to be sure it is set if the main thread
     * have been stopped inside CHECKP. On all supported architectures (x86,
     * x86_64, ppc, ppc64) int 32 has atomic assignment for supported compilers
     * (gcc, msvc).
     * On Windows we'll read vmm_cur_offs in the VMM_THREAD so we need to
     * guarantee that we read the latest value. For this we use memory barrier.
     * On Unix like systems we use signals, so we will read vmm_cur_offs in
     * the same with CHECKP thread. We need only atomic assignment on them.
     */
#if defined(_WIN32) && defined(HAVE_AO_MB_FULL)
    AO_mb_full();
#endif
    if (ALIGN_OFFS(vmm_cur_offs) != (*(xptr*) p_sm_callback_data).getOffs()) {
        VMM_TRACE_CALLBACK(*(xptr*)p_sm_callback_data);
        /* Check that layer is the same and unmap it*/
        vmm_swap_unmap_conditional(*(xptr*)p_sm_callback_data);
        *(bool*)p_sm_callback_data = true;
    } else {
#ifdef VMM_LINUX_DEBUG_CHECKP
        *(bool*)p_sm_callback_data = (*(int*)((xptr*)p_sm_callback_data + 1));
#else
        *(bool*)p_sm_callback_data = ((*(int*)((xptr*)p_sm_callback_data + 1))
            && !LAYERS_EQUAL(XOFFS2ADDR(vmm_cur_offs), *(xptr*)p_sm_callback_data));
#endif /* VMM_LINUX_DEBUG_CHECKP */
    }
}

#ifndef _WIN32

void _vmm_signal_handler(int signo, siginfo_t *info, void *cxt)
{
    vmm_callback_unmap();
    USemaphoreUp(sm_to_vmm_callback_sem2, __sys_call_error);
}

#endif /* _WIN32 */

U_THREAD_PROC(_vmm_thread, arg)
{
    while (true) {
        USemaphoreDown(sm_to_vmm_callback_sem1, __sys_call_error);

        /* quick and dirty workaround - request unmapping of
         * 0xffffffff to stop VMM callback thread */
        if (((xptr*)p_sm_callback_data)->getOffs() == (lsize_t)-1 &&
            ((xptr*)p_sm_callback_data)->layer == 0) {
            *(bool*)p_sm_callback_data = true;
            USemaphoreUp(sm_to_vmm_callback_sem2, __sys_call_error);
            return 0;
        }

#ifndef _WIN32
        pthread_kill(main_thread, SIGUSR1);
#else
        uSuspendThread(main_thread, __sys_call_error);
        vmm_callback_unmap();
        uResumeThread(main_thread, __sys_call_error);
        USemaphoreUp(sm_to_vmm_callback_sem2, __sys_call_error);
#endif /* _WIN32 */
    }
}


#ifdef VMM_LINUX_DEBUG_CHECKP
void __vmmdcp_checkp(xptr p) {
    VMM_TRACE_CHECKP(p);

    check_if_null_xptr(p);

    if (!same_block(p, vmm_cur_xptr)) {
        if (vmm_cur_xptr != XNULL) { mprotect(XADDR(vmm_cur_xptr), PAGE_SIZE, PROT_NONE); }
        vmm_cur_xptr = block_xptr(p);
        vmm_cur_offs = p.getOffs();
        mprotect(XADDR(vmm_cur_xptr), PAGE_SIZE, PROT_READ);
        if (!TEST_XPTR(vmm_cur_xptr)) vmm_unswap_block(vmm_cur_xptr);
    }

    U_ASSERT(((vmm_sm_blk_hdr *) XADDR(vmm_cur_xptr))->p == vmm_cur_xptr);
}

void __vmmdcp_vmm_signal_modification(xptr p)
{
    VMM_TRACE_SIGNAL_MODIFICATION(p);

    if (TEST_XPTR(p)) {
        mprotect(XADDR(vmm_cur_xptr), PAGE_SIZE, PROT_READ | PROT_WRITE);
    }

    if (((vmm_sm_blk_hdr*)(XADDR(block_xptr(p))))->trid_wr_access != tr_globals::sid) {
        vmm_unswap_block_write(p);
    }

    ((vmm_sm_blk_hdr*)(XADDR(block_xptr(p))))->is_changed = true;
}
#endif /* VMM_LINUX_DEBUG_CHECKP */


void vmm_on_session_begin(SSMMsg *_ssmmsg_, bool is_rcv_mode)
{
    if (USemaphoreOpen(&vmm_sm_sem, VMM_SM_SEMAPHORE_STR, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4012, "VMM_SM_SEMAPHORE_STR");

    __vmm_set_sigusr_handler();
    __vmm_init_current_xptr();

    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {
        ssmmsg = _ssmmsg_;
        shutdown_vmm_thread = false;

        msg.cmd = 21; // bm_register_session
        msg.trid = 0; // trid is not defined in this point
        msg.sid = tr_globals::sid;
        msg.data.reg.num = is_rcv_mode ? 1 : 0;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

        catalog_masterblock = msg.data.reg.mptr;
        tr_globals::authentication = GET_FLAG(msg.data.reg.transaction_flags, TR_AUTHENTICATION_FLAG);
        tr_globals::authorization  = GET_FLAG(msg.data.reg.transaction_flags, TR_AUTHORIZATION_FLAG);

        vmm_preliminary_call(msg.data.reg.layer_size);

        /* Open buffer memory */
        if (uOpenShMem(&file_mapping, CHARISMA_BUFFER_SHARED_MEMORY_NAME, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE1037);

        char buf[100];
        if (USemaphoreOpen(&sm_to_vmm_callback_sem1,
            SM_TO_VMM_CALLBACK_SEM1_BASE_STR(tr_globals::sid, buf, 100), __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4012, "SM_TO_VMM_CALLBACK_SEM1_BASE_STR");

        if (USemaphoreOpen(&sm_to_vmm_callback_sem2,
            SM_TO_VMM_CALLBACK_SEM2_BASE_STR(tr_globals::sid, buf, 100), __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4012, "SM_TO_VMM_CALLBACK_SEM2_BASE_STR");

        if (uOpenShMem(&p_sm_callback_file_mapping, CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4021, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

        p_sm_callback_data = uAttachShMem(&p_sm_callback_file_mapping, NULL, 0, __sys_call_error);
        if (p_sm_callback_data == NULL)
            throw USER_EXCEPTION2(SE4023, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

        vmm_trace_start(NULL);

        main_thread = uGetCurrentThread(__sys_call_error);
        uResVal res = uCreateThread(_vmm_thread, NULL, &vmm_thread_handle, VMM_THREAD_STACK_SIZE, NULL, __sys_call_error);
        if (res != 0) throw USER_EXCEPTION2(SE4060, "VMM thread");
    } catch (ANY_SE_EXCEPTION) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);

    vmm_session_initialized = true;
}

void vmm_on_transaction_begin(bool is_query, TIMESTAMP &ts)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {
        __vmm_init_current_xptr();
        shutdown_vmm_thread = false;

        msg.cmd = 35; // bm_register_transaction
        msg.trid = tr_globals::trid;
        msg.sid = tr_globals::sid;
        msg.data.data[0] = is_query;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

        ts = msg.data.snp_ts;

    } catch (ANY_SE_EXCEPTION) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);

    vmm_transaction_initialized = true;
}

void vmm_on_session_end()
{
    if (!vmm_session_initialized) return;

    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {
        msg.cmd = 22; // bm_unregister_session
        msg.trid = tr_globals::trid;
        msg.sid = tr_globals::sid;

        shutdown_vmm_thread = true;

        /*  USemaphoreUp(sm_to_vmm_callback_sem1, __sys_call_error);
        SM will do it instead, see comments in bm_unregister_session function. */

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

        if (uThreadJoin(vmm_thread_handle, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE1039);

        if (uCloseThreadHandle(vmm_thread_handle, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4063, "VMM thread");

        if (uCloseShMem(&file_mapping, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE1038);

        if (uDettachShMem(&p_sm_callback_file_mapping, p_sm_callback_data, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4024, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

        if (uCloseShMem(&p_sm_callback_file_mapping, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4022, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

        USemaphoreClose(sm_to_vmm_callback_sem1, __sys_call_error);
        USemaphoreClose(sm_to_vmm_callback_sem2, __sys_call_error);

    } catch (ANY_SE_EXCEPTION) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);
    USemaphoreClose(vmm_sm_sem, __sys_call_error);

    close_global_memory_mapping();

    delete mapped_pages; mapped_pages = NULL;
    delete mtrBlocks; mtrBlocks = NULL;

    vmm_session_initialized = false;
    vmm_trace_stop();
}

static void unmapAllBlocks(bit_set * map)
{
    int p = -1;

    while ((p = map->getNextSetBitIdx(p + 1)) != -1)
        vmm_unmap((char *)LAYER_ADDRESS_SPACE_START_ADDR + PAGE_SIZE * p);

    map->clear();
}

void VMMMicrotransaction::begin() {
    if (activeMT != NULL || mStarted)
    {
        /* Nested microtransactions are not allowed */
        throw SYSTEM_EXCEPTION("Nested microtransactions detected");
    }
    mStarted = true;
    activeMT = this;

    mtrBlocks->clear();
}

void VMMMicrotransaction::end() {
    SafeSemaphore sem(vmm_sm_sem);

    mStarted = false;
    activeMT = NULL;

    sem.Aquire();
    unmapAllBlocks(mtrBlocks);
    __vmm_init_current_xptr();
    sem.Release();
}

VMMMicrotransaction::~VMMMicrotransaction() {
    if (mStarted) { end(); };
}

void vmm_unmap_all_blocks()
{
    SafeSemaphore sem(vmm_sm_sem);

    sem.Aquire();
    unmapAllBlocks(mapped_pages);
    sem.Release();
}

void vmm_on_transaction_end()
{
    if (!vmm_transaction_initialized) return;

    __vmm_init_current_xptr();

    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {

        msg.cmd = 36; /* bm_unregister_transaction */
        msg.trid = tr_globals::trid;
        msg.sid = tr_globals::sid;
        msg.data.ptr = catalog_masterblock;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

        /*
        * Reset blocks with write access from the current trid.
        * There was a bug here: reusing read-mapped versions between
        * transactions leads to problems because of old versions
        * temporary fix proposal: unmap the whole region (except INVALID_LAYER pages)
        * we use bitset here, because just reading INVALID_LAYER from block takes very
        * long time on MAC OS more efficient fix of the aforementioned bug
        */
        unmapAllBlocks(mapped_pages);
    } catch (ANY_SE_EXCEPTION) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);

    vmm_transaction_initialized = false;
}
