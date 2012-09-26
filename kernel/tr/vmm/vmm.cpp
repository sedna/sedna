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

#include "u/usem.h"
#include "u/ushm.h"
#include "u/ummap.h"
#include "u/uthread.h"
#include "u/usafesync.h"
#include "auxiliary/utils.h"
#include "common/xptr/sm_vmm_data.h"
#include "auxiliary/bit_set.h"
// #include "common/gmm.h"
#include "common/errdbg/d_printf.h"
#include "common/xptr/XptrHash.h"

#include "tr/vmm/vmm.h"
#include "tr/vmm/vmminternal.h"
#include "tr/tr_globals.h"
#include "tr/cat/catvars.h"

#define VMM_REGION_SEARCH_MAX_SIZE                      ((uint32_t)0x79C00000)
#define VMM_REGION_MAX_SIZE                             ((uint32_t)0x40000000)

void  *LAYER_ADDRESS_SPACE_START_ADDR;
void  *LAYER_ADDRESS_SPACE_BOUNDARY;
uintptr_t LAYER_ADDRESS_SPACE_START_ADDR_INT;
uintptr_t LAYER_ADDRESS_SPACE_BOUNDARY_INT;
lsize_t LAYER_ADDRESS_SPACE_SIZE;

static UShMem file_mapping;

UShMem smMemoryMapping;

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

static sedna::Bitset * mapped_pages = NULL;
static sedna::Bitset * mtrBlocks = NULL;
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

        if (_uvmm_unmap(addr) || _uvmm_map(addr, 0, &smMemoryMapping, access_null)) {
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
void VirtualMemoryManager::preliminaryCall(lsize_t layer_size)
{
    CHECK_ENV(uOpenShMem(&smMemoryMapping, smShmMappingName, __sys_call_error),
              SE4400, smShmMappingName.name);

    if (__vmm_check_region(layer_size, &LAYER_ADDRESS_SPACE_START_ADDR,
            &LAYER_ADDRESS_SPACE_SIZE) != 1)
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

    mapped_pages = new sedna::Bitset(LAYER_ADDRESS_SPACE_SIZE / PAGE_SIZE); // constructor zeroes it
    mtrBlocks = new sedna::Bitset(LAYER_ADDRESS_SPACE_SIZE / PAGE_SIZE); // constructor zeroes it

    uintptr_t cur;
    for (cur = LAYER_ADDRESS_SPACE_START_ADDR_INT;
        cur < LAYER_ADDRESS_SPACE_BOUNDARY_INT;
        cur += (uint32_t)PAGE_SIZE)
    {
        if (_uvmm_map((void*)cur, 0, &smMemoryMapping, access_null) == -1)
            throw USER_EXCEPTION(SE1031);
    }

    elog(EL_DBG,  ("preliminary call: layer address space start addr = 0x%"PRIXPTR, LAYER_ADDRESS_SPACE_START_ADDR_INT));
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

inline static int send_sm_message(sm_msg_messages cmd) {
    int result;

    msg.cmd = cmd; // bm_get_block
    msg.trid = tr_globals::trid;
    msg.sid  = tr_globals::sid;

    if ((result = ssmmsg->send_msg(&msg)) != 0) { return result; }
    if (msg.cmd != msg_ok) _vmm_process_sm_error(msg.cmd);

    return 0;
}

/* Asking SM to read the block */
void vmm_unswap_block(xptr p)
{
    SafeSemaphore sem(vmm_sm_sem);

    check_bounds(p);
    sem.Aquire();
    p = block_xptr(p);

    msg.data.swap_data.ptr = p;
    if (send_sm_message(msg_unswap_block) != 0) { throw USER_EXCEPTION(SE1034); }

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
    if (send_sm_message(msg_create_version) != 0) { throw USER_EXCEPTION(SE1034); }

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

    if (send_sm_message(is_data ? msg_alloc_data_block : msg_alloc_tmp_block) != 0) { throw USER_EXCEPTION(SE1034); }

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
    if (send_sm_message(msg_release_block) != 0) {
        throw USER_EXCEPTION(SE1034);
    }

    sem.Release();
}

void vmm_delete_tmp_blocks()
{
    SafeSemaphore sem(vmm_sm_sem);

    sem.Aquire();
    __vmm_init_current_xptr();

    if (send_sm_message(msg_delete_tmp_blocks) != 0) {
        throw USER_EXCEPTION(SE1034);
    }

    sem.Release();
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

void VirtualMemoryManager::readWriteLayerSizeOnCreateDatabase(lsize_t* data, bool write)
{
    UShMem p_cdb_callback_file_mapping;
    lsize_t *p_cdb_callback_data;

    if (uOpenShMem(&p_cdb_callback_file_mapping, smCallbackName, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4021, "smCallbackName");

    p_cdb_callback_data = (lsize_t *)uAttachShMem(&p_cdb_callback_file_mapping, NULL, 0, __sys_call_error);
    if (p_cdb_callback_data == NULL)
        throw USER_EXCEPTION2(SE4023, "smCallbackName");

    if (write)
        *p_cdb_callback_data = *data;
    else
        *data = *p_cdb_callback_data;

    if (uDettachShMem(&p_cdb_callback_file_mapping, p_cdb_callback_data, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4024, "smCallbackName");

    if (uCloseShMem(&p_cdb_callback_file_mapping, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4022, "smCallbackName");    
}

#define VMM_SE_TRN_LOG "se_trn_log"
void VirtualMemoryManager::determineRegion()
{
    lsize_t cur = 0;
    lsize_t segment_size = 0, start_segment_size;

    void *res_addr = NULL;

    readWriteLayerSizeOnCreateDatabase(&start_segment_size, false /* read */);

    if (start_segment_size == 0) {
        start_segment_size = VMM_REGION_SEARCH_MAX_SIZE;
    }

    for (cur = start_segment_size; cur >= VMM_REGION_MIN_SIZE; cur -= (uint32_t)PAGE_SIZE)
    {
#ifdef EL_DEBUG
        elog(EL_TRN, ("Probing size %u... ", cur));
#endif /* EL_DEBUG */
        if (__vmm_check_region(cur, &res_addr, &segment_size)) { 
            break; 
        }
    }
    
    if (0 == segment_size) {
        elog(EL_TRN, ("Nothing has been found\n"));
        throw USER_EXCEPTION(SE1040);
    } else {
#ifdef EL_DEBUG
        d_printf3("\nvmm_determine_region:\nregion size (in pages) = %u\nsystem given addr = %"PRIxPTR"\n", segment_size / (uint32_t)PAGE_SIZE, (uintptr_t)res_addr);
#endif /* EL_DEBUG */
        if (segment_size > VMM_REGION_MAX_SIZE)
            segment_size = VMM_REGION_MAX_SIZE;

        // need to give the result back to cdb
        readWriteLayerSizeOnCreateDatabase(&segment_size, true /* write */);
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
        msg.cmd = msg_get_stats; // bm_block_statistics
        msg.trid = tr_globals::trid;
        msg.sid = tr_globals::sid;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != msg_ok) _vmm_process_sm_error(msg.cmd);

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
static void unmapAllBlocks(sedna::Bitset * map)
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

transaction_id VirtualMemoryManager::obtainTransactionId()
{
    sm_msg_struct msg;

    msg.trid = -1;

    for (;;)
    {
        msg.cmd = msg_request_transaction_id; // give me a trid
        msg.sid = tr_globals::sid;
        msg.data.trinfo.rdonly = tr_globals::is_ro_mode;
        
        /* TODO: CHECK IT */
        msg.data.trinfo.excl = tr_globals::is_log_less_mode;

        if (ssmmsgClient->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE3034);

        U_ASSERT(msg.cmd == msg_ok || msg.cmd == msg_request_transaction_id);

        if (tr_globals::is_log_less_mode || msg.cmd == msg_request_transaction_id)
        {
            // wait for sm to unblock
            int res;
            for (;;)
            {
                res = USemaphoreDownTimeout(tr_globals::wait_sem, 1000, __sys_call_error);
                if (res == 0) //unblocked
                {
                    break;
                } else {// error
                    throw USER_EXCEPTION2(SE4015, "SEDNA_TRANSACTION_LOCK");
                }
            }
                    
        }

        // we've obtained a trid
        if (msg.cmd == 0) break;
    }

    if (msg.trid == -1)
        throw USER_EXCEPTION(SE4607);

    d_printf2("get trid=%d\n", msg.trid);
    return msg.trid;
}

static void
release_transaction_id(SSMMsg* sm_server)
{
        if (tr_globals::trid < 0 || tr_globals::trid >= SEDNA_MAX_TRN_NUMBER) return;

        d_printf2("return trid=%d\n", tr_globals::trid);
        sm_msg_struct msg;
        msg.cmd = msg_release_transaction_id;
        msg.trid = tr_globals::trid;
        msg.sid = tr_globals::sid;
        msg.data.trinfo.rdonly = tr_globals::is_ro_mode;

        if (sm_server->send_msg(&msg) !=0 )
            throw USER_EXCEPTION(SE3034);
}


void VirtualMemoryManager::onSessionBegin()
{
    if (USemaphoreOpen(&vmm_sm_sem, vmmLockName, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4012, "vmmLockName");

    __vmm_set_sigusr_handler();
    __vmm_init_current_xptr();

    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {
        ssmmsgClient =  new SSMMsg(SSMMsg::Client,
                                   sizeof (sm_msg_struct),
                                   smMessageServerName,
                                   SM_NUMBER_OF_SERVER_THREADS,
                                   U_INFINITE);
        CHECK_ENV(ssmmsgClient->init(), SE4200, "Failed to connect to SSMMsg server");
        ssmmsg = ssmmsgClient;
        shutdown_vmm_thread = false;

        msg.cmd = msg_register_session; // bm_register_session
        msg.trid = 0; // trid is not defined in this point
        msg.sid = tr_globals::sid;
        msg.data.reg.num = isRunRecovery ? 1 : 0;

        if (ssmmsgClient->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != msg_ok) _vmm_process_sm_error(msg.cmd);

        catalog_masterblock = msg.data.reg.mptr;
        tr_globals::authentication = GET_FLAG(msg.data.reg.transaction_flags, TR_AUTHENTICATION_FLAG);
        tr_globals::authorization  = GET_FLAG(msg.data.reg.transaction_flags, TR_AUTHORIZATION_FLAG);

        preliminaryCall(msg.data.reg.layer_size);

        /* Open buffer memory */
        if (uOpenShMem(&file_mapping, bufferSharedMemoryName, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE1037);

        char buf[100];
        if (USemaphoreOpen(&sm_to_vmm_callback_sem1,
            smCallbackSem1, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4012, "smCallbackSem1");

        if (USemaphoreOpen(&sm_to_vmm_callback_sem2,
            smCallbackSem2, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4012, "smCallbackSem2");

        if (uOpenShMem(&p_sm_callback_file_mapping, smCallbackName, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4021, "smCallbackName");

        p_sm_callback_data = uAttachShMem(&p_sm_callback_file_mapping, NULL, 0, __sys_call_error);
        if (p_sm_callback_data == NULL)
            throw USER_EXCEPTION2(SE4023, "smCallbackName");

        vmm_trace_start(NULL);

        main_thread = uGetCurrentThread(__sys_call_error);
        uResVal res = uCreateThread(_vmm_thread, NULL, &vmm_thread_handle, VMM_THREAD_STACK_SIZE, NULL, __sys_call_error);
        if (res != 0) throw USER_EXCEPTION2(SE4060, "VMM thread");
    } catch (ANY_SE_EXCEPTION) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);

    isSessionInitialized = true;
}

void VirtualMemoryManager::onTransactionBegin()
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {
        __vmm_init_current_xptr();
        shutdown_vmm_thread = false;

        msg.cmd = msg_register_transaction; // bm_register_transaction
        msg.trid = tr_globals::trid;
        msg.sid = tr_globals::sid;
        msg.data.trinfo.rdonly = isReadOnlyQuery;

        if (ssmmsgClient->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != msg_ok) _vmm_process_sm_error(msg.cmd);

        timeStamp = msg.data.snp_ts;

    } catch (ANY_SE_EXCEPTION) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);

    isTransactionInitialized = true;
}

void VirtualMemoryManager::onTransactionEnd() {
    if (!isTransactionInitialized) return;

    __vmm_init_current_xptr();

    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {

        msg.cmd = msg_unregister_transaction; /* bm_unregister_transaction */
        msg.trid = tr_globals::trid;
        msg.sid = tr_globals::sid;
        msg.data.ptr = catalog_masterblock;

        if (ssmmsgClient->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != msg_ok) _vmm_process_sm_error(msg.cmd);

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

    isTransactionInitialized = false;
}

void VirtualMemoryManager::onSessionEnd()
{
    if (!isSessionInitialized) return;

    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {
        msg.cmd = msg_unregister_session; // bm_unregister_session
        msg.trid = tr_globals::trid;
        msg.sid = tr_globals::sid;

        shutdown_vmm_thread = true;

        /*  USemaphoreUp(sm_to_vmm_callback_sem1, __sys_call_error);
        SM will do it instead, see comments in bm_unregister_session function. */

        if (ssmmsgClient->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != msg_ok) _vmm_process_sm_error(msg.cmd);

        if (uThreadJoin(vmm_thread_handle, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE1039);

        if (uCloseThreadHandle(vmm_thread_handle, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4063, "VMM thread");

        if (uCloseShMem(&file_mapping, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE1038);

        if (uDettachShMem(&p_sm_callback_file_mapping, p_sm_callback_data, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4024, "SEDNA_SM_CALLBACK_SHARED_MEMORY_NAME");

        if (uCloseShMem(&p_sm_callback_file_mapping, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4022, "SEDNA_SM_CALLBACK_SHARED_MEMORY_NAME");

        USemaphoreClose(sm_to_vmm_callback_sem1, __sys_call_error);
        USemaphoreClose(sm_to_vmm_callback_sem2, __sys_call_error);

    } catch (ANY_SE_EXCEPTION) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);
    USemaphoreClose(vmm_sm_sem, __sys_call_error);

    CHECK_ENV(uCloseShMem(&smMemoryMapping, __sys_call_error),
              SE4077, smShmMappingName.name);

    delete mapped_pages; mapped_pages = NULL;
    delete mtrBlocks; mtrBlocks = NULL;

    d_printf1("Deleting SSMMsg...");
    ssmmsgClient->shutdown();
    delete ssmmsgClient;
    ssmmsgClient = NULL;
    ssmmsg = NULL;
    d_printf1("SSMMsg client shut down ok\n");
    
    isSessionInitialized = false;
    vmm_trace_stop();
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
