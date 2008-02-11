/*
 * File:  vmm.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


// !!! NOTE: 1. fix bug with reserving space for persistent heap (in Linux)
//              What will happen if persistent heap is to be extended? (Windows, Linux)
//           2. Unmap tmp blocks after vmm_delete_tmp_blocks call
//           3. call to REFRESH_LRU_STAMP has problems when p is not an xptr but 
//              rather an expression which depends on vmm block that is in vmm memory


#include <set>

#include "common/sedna.h"

#include "common/u/usem.h"
#include "common/u/ushm.h"
#include "common/u/ummap.h"
#include "common/utils.h"
#include "common/sm_vmm_data.h"
#include "tr/vmm/vmm.h"
#include "tr/tr_globals.h"
#include "tr/structures/pers_map.h"
#include "tr/structures/schema.h"
#include "common/gmm.h"
#include "common/errdbg/d_printf.h"


using namespace std;

#define VMM_ACCURATE


/*******************************************************************************
********************************************************************************
  TYPES AND VARIABLES
********************************************************************************
*******************************************************************************/

static UMMap file_mapping;
extern UMMap global_memory_mapping;

#ifdef _WIN32
#define VMM_THREAD_STACK_SIZE       10240
#else
#define VMM_THREAD_STACK_SIZE       102400
//uspinlock __vmm_spin_lock;
//uspinlock *vmm_spin_lock = &__vmm_spin_lock;
bool vmm_is_busy_called = false;
#endif


xptr vmm_cur_xptr;
volatile void * vmm_cur_ptr;

// pointer to SSMMsg - link to SM
static SSMMsg *ssmmsg;

// semaphore for global VMM/SM synchronization
static USemaphore vmm_sm_sem;

// callback semaphores and shared memory
static UShMem p_sm_callback_file_mapping;
static void * p_sm_callback_data;
static USemaphore sm_to_vmm_callback_sem1, sm_to_vmm_callback_sem2;	// callback semaphores for SM

// exclusive mode
static USemaphore xmode;
static bool is_exclusive_mode = false;	// indicates is this VMM in exclusive mode

#ifdef LRU
// LRU global stamp counter
static UShMem lru_global_stamp_file_mapping;
LRU_stamp *lru_global_stamp_data;
#endif

// threads
static volatile bool shutdown_vmm_thread = false;
static UTHANDLE vmm_thread_handle;	// handle to vmm thread
static UTHANDLE main_thread;		// handle to main thread

static bool vmm_session_initialized = false;
static bool vmm_transaction_initialized = false;

static sm_msg_struct msg;

static bool vmm_is_recovery_mode = false;
// indirection block set (used for recovery)
static std::set<xptr> indir_block_set;

// if vmm_determine_region failes is writes out log to this stream
FILE *f_se_trn_log;
#define VMM_SE_TRN_LOG			"se_trn_log"


#ifdef VMM_DEBUG_CHECKP
xptr vmm_checkp_xptr;
#endif /*VMM_DEBUG_CHECKP*/


#ifdef VMM_TRACE

static FILE* trace_file = NULL;

void vmm_trace_xptr(const xptr& p)
{
    fprintf(trace_file, "%d %u", p.layer, (unsigned int)p.addr);
}

void vmm_trace_CHECKP(const xptr& p)
{
    fprintf(trace_file, "c");
    vmm_trace_xptr(p);
    fprintf(trace_file, "\n");
}

void vmm_trace_signal_modification(const xptr& p)
{
    fprintf(trace_file, "s");
    vmm_trace_xptr(p);
    fprintf(trace_file, "\n");
}

void vmm_trace_alloc_data_block()
{
    fprintf(trace_file, "d\n");
}

void vmm_trace_alloc_tmp_block()
{
    fprintf(trace_file, "t\n");
}

void vmm_trace_unswap(const xptr& p)
{
    fprintf(trace_file, "u");
    vmm_trace_xptr(p);
    fprintf(trace_file, "\n");
}

void vmm_trace_delete_block(const xptr& p)
{
    fprintf(trace_file, "e");
    vmm_trace_xptr(p);
    fprintf(trace_file, "\n");
}

#endif



/*******************************************************************************
********************************************************************************
  VMM MAP/REMAP/UNMAP FUNCTIONS
********************************************************************************
*******************************************************************************/
#ifdef VMM_ACCURATE
static ramoffs default_ram = RAMOFFS_OUT_OFF_BOUNDS;
#endif

// functions return -1 in case of error
int __vmm_map(void *addr, ramoffs offs)
{
#ifdef _WIN32
    HANDLE m;
#else
    int m;
#endif

    if (offs == RAMOFFS_OUT_OFF_BOUNDS)
    {
        m = global_memory_mapping.map;
        offs = 0;
    }
    else
    {
        m = file_mapping.map;
    }
    
#ifdef _WIN32
    addr = MapViewOfFileEx(
              m,						// handle to file-mapping object
              FILE_MAP_ALL_ACCESS,		// access mode
              0,						// high-order DWORD of offset
              offs,						// low-order DWORD of offset
              PAGE_SIZE,				// number of bytes to map
              addr						// starting address
           );
#else
    addr = mmap(addr, PAGE_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, m, offs);
#endif
#ifdef _WIN32
    if (addr == NULL)
    {
        d_printf1("MapViewOfFileEx failed\n");
        d_printf3("Error %d; addr = 0x%x\n", GetLastError(), (int)(addr));
#else
    if (addr == MAP_FAILED)
    {
        d_perror("mmap failed");
        d_printf2("Addr = 0x%x\n", (int)(addr));
#endif
        return -1;
    }
    return 0;
}

inline int __vmm_unmap(void *addr)
{
#ifdef _WIN32
    return (UnmapViewOfFile(addr) == 0 ? -1 : 0);
#else
    return munmap(addr, PAGE_SIZE);
#endif
}

void _vmm_preinit_region()
{
#ifdef VMM_ACCURATE
    __uint32 cur;
    for (cur = LAYER_ADDRESS_SPACE_START_ADDR_INT; 
         cur < LAYER_ADDRESS_SPACE_BOUNDARY_INT;
         cur += (__uint32)PAGE_SIZE)
    {
        if (__vmm_map((void*)cur, default_ram) == -1)
            throw USER_EXCEPTION(SE1031);
    }
#else
#   ifdef _WIN32
        void *start = NULL;
        start = VirtualAlloc(
                       LAYER_ADDRESS_SPACE_START_ADDR,			// region to reserve or commit
                       LAYER_ADDRESS_SPACE_SIZE,				// size of region
                       MEM_RESERVE,								// type of allocation
                       PAGE_READWRITE							// type of access protection
                );
        if (start == NULL) 
        {
            d_printf2("VirtualAlloc failed with error %d\n", GetLastError());
            throw USER_EXCEPTION(SE1031);
        }
#   endif
#endif
}

void _vmm_init_region()
{
#ifdef VMM_ACCURATE
#else
#   ifdef _WIN32
        void *start = LAYER_ADDRESS_SPACE_START_ADDR;
        if (VirtualFree(
                   start,					// address of region
                   0,						// size of region
                   MEM_RELEASE				// operation type
            ) == 0) 
        {
            d_printf2("VirtualFree failed with error %d\n", GetLastError());
            throw USER_EXCEPTION(SE1031);
        }
#   endif
#endif
}

inline void _vmm_map(void *addr, ramoffs offs)
{
#ifdef VMM_ACCURATE
    if (__vmm_unmap(addr) || __vmm_map(addr, offs))
        throw USER_EXCEPTION(SE1035);
#else
    if (__vmm_map(addr, offs) == -1)
        throw USER_EXCEPTION(SE1035);
#endif
}

inline void _vmm_remap(void *addr, ramoffs offs)
{
#ifdef VMM_ACCURATE
    if (__vmm_unmap(addr) || __vmm_map(addr, offs))
        throw USER_EXCEPTION(SE1035);
#else
    __vmm_unmap(addr);
    if (__vmm_map(addr, offs))
        throw USER_EXCEPTION(SE1035);
#endif
}

#ifndef VMM_DEBUG_CHECKP
inline 
#endif
void _vmm_unmap_severe(void *addr)
{
#ifdef VMM_ACCURATE
    if (__vmm_unmap(addr) || __vmm_map(addr, default_ram))
        throw USER_EXCEPTION(SE1035);
#else
    if (__vmm_unmap(addr) == -1)
        throw USER_EXCEPTION(SE1035);
#endif
}

inline void _vmm_unmap_decent(void *addr)
{
#ifdef VMM_ACCURATE
    if (__vmm_unmap(addr) || __vmm_map(addr, default_ram))
        throw USER_EXCEPTION(SE1035);
#else
    __vmm_unmap(addr);
#endif
}


/*******************************************************************************
********************************************************************************
  VMM HANDLER THREAD
********************************************************************************
*******************************************************************************/

#ifndef _WIN32
void _vmm_signal_handler(int signo, siginfo_t *info, void *cxt)
{
//    d_printf1("_vmm_signal_handler\n");
/*
#ifndef VMM_UNIX_LIGHT_CHECKP
    if (ALIGN_ADDR(vmm_cur_ptr) == XADDR(*(xptr*)p_sm_callback_data))
        *(bool*)p_sm_callback_data = false;
    else
#endif
        {
            _vmm_unmap_decent(XADDR(*(xptr*)p_sm_callback_data));
            //munmap(XADDR(*(xptr*)p_sm_callback_data), PAGE_SIZE);
            *(bool*)p_sm_callback_data = true;
        }
*/

#ifndef VMM_UNIX_LIGHT_CHECKP
        if (ALIGN_ADDR(vmm_cur_ptr) != XADDR(*(xptr*)p_sm_callback_data))
#endif
        {
            _vmm_unmap_decent(XADDR(*(xptr*)p_sm_callback_data));
            *(bool*)p_sm_callback_data = true;
        }
#ifndef VMM_UNIX_LIGHT_CHECKP
        else
        {
            *(bool*)p_sm_callback_data = ((*(int*)((xptr*)p_sm_callback_data + 1)) // use_layer 
                                          && 
                                          !LAYERS_EQUAL(vmm_cur_ptr, *(xptr*)p_sm_callback_data));
        }
#endif

    USemaphoreUp(sm_to_vmm_callback_sem2, __sys_call_error);
}
#endif


U_THREAD_PROC(_vmm_thread, arg)
{
    //printf("main_thread %d\n", main_thread);
    //printf("vmm_thread %d\n", GetCurrentThread());

    //printf("sm_to_vmm_callback_sem1 = %d\n", sm_to_vmm_callback_sem1);
    //printf("sm_to_vmm_callback_sem2 = %d\n", sm_to_vmm_callback_sem2);

    while (true) 
    {
        USemaphoreDown(sm_to_vmm_callback_sem1, __sys_call_error);
        //printf("vmm_thread");fflush(stdout);

        if (shutdown_vmm_thread) 
        {
            return 0;
        }

        VMM_INC_NUMBER_OF_SM_CALLBACKS;

#ifndef _WIN32
        pthread_kill(main_thread, SIGUSR1);
        continue;
#endif

//#ifdef _WIN32
        uSuspendThread(main_thread, __sys_call_error);
//#else
//        uSpinLock(vmm_spin_lock);
//#endif

/*
        if (ALIGN_ADDR(vmm_cur_ptr) == XADDR(*(xptr*)p_sm_callback_data))
            *(bool*)p_sm_callback_data = false;
        else
        {
            _vmm_unmap_decent(XADDR(*(xptr*)p_sm_callback_data));
            *(bool*)p_sm_callback_data = true;
        }
*/
        if (ALIGN_ADDR(vmm_cur_ptr) != XADDR(*(xptr*)p_sm_callback_data))
        {
            _vmm_unmap_decent(XADDR(*(xptr*)p_sm_callback_data));
            *(bool*)p_sm_callback_data = true;
        }
        else
        {
            *(bool*)p_sm_callback_data = ((*(int*)((xptr*)p_sm_callback_data + 1)) // use_layer 
                                          && 
                                          !LAYERS_EQUAL(vmm_cur_ptr, *(xptr*)p_sm_callback_data));
        }

//#ifdef _WIN32
        uResumeThread(main_thread, __sys_call_error);
//#else
//        uSpinUnlock(vmm_spin_lock);
//#endif

        USemaphoreUp(sm_to_vmm_callback_sem2, __sys_call_error);
    }
}


/*******************************************************************************
********************************************************************************
  INTERNAL FUNCTIONS
********************************************************************************
*******************************************************************************/
bool _vmm_is_address_busy(void * p)
{
    bool is_busy = true;

#   ifdef _WIN32
    try {
        if (((vmm_sm_blk_hdr*)p)->p == NULL) is_busy = false;
    } catch (win32_access_violation& e) {
        is_busy = false;
    }
#   else
    vmm_is_busy_called = true;
    if (setjmp(vmm_is_busy_env) != 0) is_busy = false;
    else
    {
        if (((vmm_sm_blk_hdr*)p)->p == NULL) is_busy = false;
    }
    vmm_is_busy_called = false;
#   endif

    return is_busy;
}

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

void _vmm_alloc_block(xptr *p, bool is_data) 
{
    msg.cmd = is_data ? 23 : 24; // bm_alloc_data_block / bm_alloc_tmp_block
    msg.trid = trid;
    msg.sid = sid;

    if (ssmmsg->send_msg(&msg) != 0)
        throw USER_EXCEPTION(SE1034);

    if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

    *p = *(xptr*)(&(msg.data.swap_data.ptr));
    xptr swapped = *(xptr*)(&(msg.data.swap_data.swapped));
    ramoffs offs = *(ramoffs*)(&(msg.data.swap_data.offs));

    if (swapped != NULL) _vmm_unmap_decent(XADDR(swapped));
    _vmm_remap(XADDR(*p), offs);
}


/*******************************************************************************
********************************************************************************
  VMM REGION RESERVING FUNCTIONS
********************************************************************************
*******************************************************************************/

void vmm_preliminary_call() throw (SednaException)
{
    open_global_memory_mapping(SE4400);
    get_vmm_region_values();

    global_memory_mapping = get_global_memory_mapping();
    _vmm_preinit_region();

#ifdef _WIN32
        void *start;
        start = VirtualAlloc(
                       PH_ADDRESS_SPACE_START_ADDR,												// region to reserve or commit
                       LAYER_ADDRESS_SPACE_START_ADDR_INT - PH_ADDRESS_SPACE_START_ADDR_INT,	// size of region
                       MEM_RESERVE,																// type of allocation
                       PAGE_READWRITE															// type of access protection
                );
        if (start == NULL) 
        {
            d_printf2("VirtualAlloc failed with error %d\n", GetLastError());
            throw USER_EXCEPTION(SE1031);
        }
#endif
}

void vmm_determine_region(bool log) throw (SednaException)
{
    if (log)
    {
        f_se_trn_log = fopen(VMM_SE_TRN_LOG, "w");
        if (f_se_trn_log == NULL)
        {
            printf("Can't open file se_trn_log\n");
            return;
        }
    }

    __uint32 cur = 0;              
    __uint32 segment_size = 0;
    void *res_addr = NULL;

#ifndef _WIN32
    int fd_dev_zero;
    if ((fd_dev_zero = open("/dev/zero", O_RDWR)) == -1) 
    {
        perror("Can't open /dev/zero");
        if(log) fprintf(f_se_trn_log, "Can't open /dev/zero.\nError: %d\n", errno);
        else vmm_determine_region(true);
        return;       
    }
#endif /* _WIN32 */
    
    for (cur  = VMM_REGION_SEARCH_MAX_SIZE; 
         cur >= PH_SIZE + VMM_REGION_MIN_SIZE; 
         cur -= (__uint32)PAGE_SIZE)
    {
        // trying to allocate a continuous region of size "cur" ...
        if (log) fprintf(f_se_trn_log, "Probing size %ud... ", cur); 
#ifdef _WIN32
        res_addr = VirtualAlloc(
                   NULL,                      // system determines where to allocate the region
                   cur + (__uint32)PAGE_SIZE, // size of region
                   MEM_RESERVE,               // type of allocation
                   PAGE_READWRITE             // type of access protection
            );
        if (res_addr)
        {
            if (log) fprintf(f_se_trn_log, "PASSED\n");
            segment_size = cur;
            VirtualFree(res_addr, 0, MEM_RELEASE);
	    res_addr = (void*)(((__uint32)res_addr + (__uint32)PAGE_SIZE) & PAGE_BIT_MASK);
            break;
        }
        else if(log) fprintf(f_se_trn_log, "FAILED with error %d\n", GetLastError());
#else /* _WIN32 */
        res_addr = mmap(0, cur + (__uint32)PAGE_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd_dev_zero, 0);
        if (res_addr != MAP_FAILED)
        {
            if (log) fprintf(f_se_trn_log, "PASSED\n");
            segment_size = cur;
            munmap(res_addr, cur);
            res_addr = (void*)(((__uint32)res_addr + (__uint32)PAGE_SIZE) & PAGE_BIT_MASK);
            break;
        }
        else if(log) fprintf(f_se_trn_log, "FAILED with error %d\n", errno);
#endif /* _WIN32 */
    }
    
    if(log)
    {
        if (0 == segment_size) fprintf(f_se_trn_log, "Nothing has been found\n");
        else fprintf(f_se_trn_log, "\nvmm_determine_region:\nregion size (in pages) = %d\nsystem given addr = 0x%x\n", segment_size / (__uint32)PAGE_SIZE, (__uint32)res_addr);
        if (fclose(f_se_trn_log) != 0) printf("Can't close file se_trn_log\n");
    }
    else
    {
        if (0 == segment_size)
        {
            printf("Nothing has been found\n");
            vmm_determine_region(true);
#ifndef _WIN32
            if (close(fd_dev_zero) == -1) printf("Can't close file fd_dev_zero\n");           
#endif /* _WIN32 */
            throw USER_EXCEPTION(SE1040);
        }
        else
        {
            d_printf3("\nvmm_determine_region:\nregion size (in pages) = %d\nsystem given addr = 0x%x\n", segment_size / (__uint32)PAGE_SIZE, (__uint32)res_addr);
    
            if(segment_size > PH_SIZE + VMM_REGION_MAX_SIZE)
            {
                LAYER_ADDRESS_SPACE_SIZE = VMM_REGION_MAX_SIZE;

                int pages_in_founded_segment = segment_size / (__uint32)PAGE_SIZE;
                int pages_in_needed_region   = (VMM_REGION_MAX_SIZE + PH_SIZE) / (__uint32)PAGE_SIZE;
                int pages_left_shift         = (pages_in_founded_segment - pages_in_needed_region) / 2;

                LAYER_ADDRESS_SPACE_BOUNDARY_INT = (__uint32)res_addr + 
                                                   (pages_left_shift * (__uint32)PAGE_SIZE)+
                                                   (LAYER_ADDRESS_SPACE_SIZE + PH_SIZE);
            }
            else /* PH_SIZE + VMM_REGION_MAX_SIZE <= segment_size >= PH_SIZE + VMM_REGION_MIN_SIZE */
            {
                LAYER_ADDRESS_SPACE_SIZE = segment_size - PH_SIZE;
                LAYER_ADDRESS_SPACE_BOUNDARY_INT = (__uint32)res_addr + segment_size; 
            }
            
            LAYER_ADDRESS_SPACE_START_ADDR_INT = LAYER_ADDRESS_SPACE_BOUNDARY_INT - LAYER_ADDRESS_SPACE_SIZE;
            PH_ADDRESS_SPACE_START_ADDR_INT = LAYER_ADDRESS_SPACE_START_ADDR_INT - PH_SIZE;
            
            LAYER_ADDRESS_SPACE_START_ADDR = (void*)LAYER_ADDRESS_SPACE_START_ADDR_INT;
            LAYER_ADDRESS_SPACE_BOUNDARY = (void*)LAYER_ADDRESS_SPACE_BOUNDARY_INT;
            PH_ADDRESS_SPACE_START_ADDR = (void*)PH_ADDRESS_SPACE_START_ADDR_INT;

            open_global_memory_mapping(SE4400);
            set_vmm_region_values();
            close_global_memory_mapping();
        } 
    }

#ifndef _WIN32
    if (close(fd_dev_zero) == -1) printf("Can't close file fd_dev_zero\n");           
#endif /* _WIN32 */
}


/*******************************************************************************
********************************************************************************
  ACTIVATE/DEACTIVATE FUNCTIONS
********************************************************************************
*******************************************************************************/

persistent_db_data *vmm_on_session_begin(SSMMsg *_ssmmsg_, bool is_rcv_mode) throw (SednaException)
{
    vmm_cur_ptr = NULL;

    persistent_db_data *db_data_ptr = NULL;

    if (USemaphoreOpen(&vmm_sm_sem, VMM_SM_SEMAPHORE_STR, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4012, "VMM_SM_SEMAPHORE_STR");

    if (USemaphoreOpen(&xmode, VMM_SM_EXCLUSIVE_MODE_SEM_STR, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4012, "VMM_SM_EXCLUSIVE_MODE_SEM_STR");

#ifndef _WIN32
    struct sigaction sigsegv_act;
                                                                                                                             
    memset(&sigsegv_act, '\0', sizeof(struct sigaction));
    sigsegv_act.sa_sigaction = _vmm_signal_handler;
    sigsegv_act.sa_flags = SA_SIGINFO;
    if (sigaction(SIGUSR1, &sigsegv_act, NULL) == -1)
        throw USER_EXCEPTION(SE1033);
#endif

    vmm_cur_xptr = XNULL;

//#ifndef _WIN32
//    uSpinInit(vmm_spin_lock);
//#endif

    vmm_is_recovery_mode = is_rcv_mode;


    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {

        ssmmsg = _ssmmsg_;
        shutdown_vmm_thread = false;

        msg.cmd = 21; // bm_register_session
        msg.trid = 0; // trid is not defined in this point
        msg.sid = sid;
        msg.data.reg.num = vmm_is_recovery_mode ? 1 : 0;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

        db_data_ptr = (persistent_db_data *)(msg.data.reg.mptr);
        int bufs_num = msg.data.reg.num;

        /// persistent_db_data initialization /////////////////////////////////
        if (!(db_data_ptr->nslist))
        {
		    db_data_ptr->index = pers_sset<index_cell, unsigned short>::init();
            db_data_ptr->idx_counter = 1;
			#ifdef SE_ENABLE_FTSEARCH
			db_data_ptr->ft_index = pers_sset<ft_index_cell, unsigned short>::init();
            db_data_ptr->ft_idx_counter = 1;
         #endif
   			#ifdef SE_ENABLE_TRIGGERS
			db_data_ptr->trigger = pers_sset<trigger_cell, unsigned short>::init();
         #endif
		    db_data_ptr->last_nid = NULL;
		    db_data_ptr->last_nid_size = 0;
		    //support namespaces list
		    db_data_ptr->nslist=pers_sset<xml_ns, unsigned short>::init();
		    db_data_ptr->metadata=pers_sset<sn_metadata_cell, unsigned short>::init();
		    xml_ns* ns = xml_ns::init(NULL,"xml",true);
		    db_data_ptr->nslist->put(ns);
            db_data_ptr->is_first_trn = true;
        }
        /// persistent_db_data initialization /////////////////////////////////


       _vmm_init_region();

        // Open buffer memory
        file_mapping = uOpenFileMapping(U_INVALID_FD, bufs_num * PAGE_SIZE, CHARISMA_BUFFER_SHARED_MEMORY_NAME, __sys_call_error);
        if (U_INVALID_FILEMAPPING(file_mapping))
            throw USER_EXCEPTION(SE1037);
        // Buffer memory has been opened 

        char buf[100];
        if (USemaphoreOpen(&sm_to_vmm_callback_sem1, 
                           SM_TO_VMM_CALLBACK_SEM1_BASE_STR(sid, ((gov_config_struct*)gov_shm_pointer)->gov_vars.os_primitives_id_min_bound, buf, 100), __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4012, "SM_TO_VMM_CALLBACK_SEM1_BASE_STR");

        if (USemaphoreOpen(&sm_to_vmm_callback_sem2, 
                           SM_TO_VMM_CALLBACK_SEM2_BASE_STR(sid, ((gov_config_struct*)gov_shm_pointer)->gov_vars.os_primitives_id_min_bound,  buf, 100), __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4012, "SM_TO_VMM_CALLBACK_SEM2_BASE_STR");

        if (uOpenShMem(&p_sm_callback_file_mapping, CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME, sizeof(xptr) + sizeof(int), __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4021, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

        p_sm_callback_data = uAttachShMem(p_sm_callback_file_mapping, NULL, sizeof(xptr), __sys_call_error);
        if (p_sm_callback_data == NULL) 
            throw USER_EXCEPTION2(SE4023, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");
#ifdef LRU
        if (uOpenShMem(&lru_global_stamp_file_mapping, CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME, sizeof(LRU_stamp), __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4021, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");

        lru_global_stamp_data = (LRU_stamp*)uAttachShMem(lru_global_stamp_file_mapping, NULL, sizeof(LRU_stamp), __sys_call_error);
        if (lru_global_stamp_data == NULL) 
            throw USER_EXCEPTION2(SE4023, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");
#endif
        is_exclusive_mode = false;


        main_thread = uGetCurrentThread(__sys_call_error);
        uResVal res = uCreateThread(_vmm_thread, NULL, &vmm_thread_handle, VMM_THREAD_STACK_SIZE, NULL, __sys_call_error);
        if (res != 0) throw USER_EXCEPTION2(SE4060, "VMM thread");

#ifdef VMM_TRACE
        trace_file = fopen(TRACE_FILE, "wt");
        if (trace_file == NULL) throw USER_ENV_EXCEPTION("Error opening VMM trace file");
#endif

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);

    vmm_session_initialized = true;

    return db_data_ptr;
}

void vmm_on_transaction_begin() throw (SednaException)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {

        vmm_cur_ptr = NULL;
        vmm_cur_xptr = XNULL;
        shutdown_vmm_thread = false;


        msg.cmd = 35; // bm_register_transaction
        msg.trid = trid;
        msg.sid = sid;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);

    vmm_transaction_initialized = true;
}

void vmm_on_session_end() throw (SednaException)
{
    if (!vmm_session_initialized) return;

    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {
        shutdown_vmm_thread = true;
        USemaphoreUp(sm_to_vmm_callback_sem1, __sys_call_error);

        if (uThreadJoin(vmm_thread_handle, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE1039);


        if (uCloseThreadHandle(vmm_thread_handle, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4063, "VMM thread");

        if (uCloseFileMapping(file_mapping, __sys_call_error) == -1)
            throw USER_EXCEPTION(SE1038);

        if (uDettachShMem(p_sm_callback_file_mapping, p_sm_callback_data, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4024, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");

        if (uCloseShMem(p_sm_callback_file_mapping, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4022, "CHARISMA_SM_CALLBACK_SHARED_MEMORY_NAME");
#ifdef LRU
        if (uDettachShMem(lru_global_stamp_file_mapping, lru_global_stamp_data, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4024, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");

        if (uCloseShMem(lru_global_stamp_file_mapping, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4022, "CHARISMA_LRU_STAMP_SHARED_MEMORY_NAME");
#endif

        USemaphoreClose(sm_to_vmm_callback_sem1, __sys_call_error);
        USemaphoreClose(sm_to_vmm_callback_sem2, __sys_call_error);

        msg.cmd = 22; // bm_unregister_session
        msg.trid = trid;
        msg.sid = sid;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);

//#ifndef _WIN32
//    uSpinDestroy(vmm_spin_lock);
//#endif

    USemaphoreClose(vmm_sm_sem, __sys_call_error);
    USemaphoreClose(xmode, __sys_call_error);

    close_global_memory_mapping();

    vmm_session_initialized = false;

#ifdef VMM_TRACE
    if (fclose(trace_file) != 0) throw USER_ENV_EXCEPTION("Error closing VMM trace file");
#endif
}

void vmm_on_transaction_end() throw (SednaException)
{
    if (!vmm_transaction_initialized) return;

    vmm_cur_ptr = NULL;
    vmm_cur_xptr = XNULL;

    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {

        msg.cmd = 36; // bm_unregister_transaction
        msg.trid = trid;
        msg.sid = sid;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);

    vmm_transaction_initialized = false;
}


/*******************************************************************************
********************************************************************************
  INTERFACE FUNCTIONS
********************************************************************************
*******************************************************************************/

void _vmm_alloc_data_block(xptr *p) throw (SednaException)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {
        _vmm_alloc_block(p, true);
    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);

    CHECKP(*p);

}

void vmm_alloc_data_block(xptr *p) throw (SednaException)
{
    if (vmm_is_recovery_mode)
    {
        vector<xptr> d;
        int i = 0;

        while (true)
        {
            _vmm_alloc_data_block(p);
            if (indir_block_set.find(*p) == indir_block_set.end())
                break;
            else
                d.push_back(*p);
        }

        for (i = 0; i < d.size(); i++)
            vmm_delete_block(d[i]);
    }
    else
    {
        _vmm_alloc_data_block(p);

        VMM_INC_DATA_BLOCK_COUNT
        VMM_TRACE_ALLOC_DATA_BLOCK
        //printf("vmm_alloc_data_block (%d, 0x%x)\n", p->layer, p->addr);
    }
}

void vmm_alloc_tmp_block(xptr *p) throw (SednaException)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {
        _vmm_alloc_block(p, false);
    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);

    CHECKP(*p);

    VMM_INC_TMP_BLOCK_COUNT
    VMM_TRACE_ALLOC_TMP_BLOCK
}

void vmm_delete_block(xptr p) throw (SednaException)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {
        p = block_xptr(p);

        VMM_TRACE_DELETE_BLOCK(p)

        // now we should recognize is the p.addr busy...
        bool is_busy = _vmm_is_address_busy(XADDR(p));

        // ... and if it busy we should free it
        if (is_busy && TEST_XPTR(p))
        { // address is busy and block has the same layer
            _vmm_unmap_severe(XADDR(p));
        }
        else ; // the block to release is not in memory

        // Anyway we have to notify SM about deletion of the block
        msg.cmd = 25; // bm_delete_block
        msg.trid = trid;
        msg.sid = sid;
        msg.data.ptr = *(__int64*)(&p);

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);
}

void vmm_delete_tmp_blocks() throw (SednaException)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {

        // TODO: This may be ineffective. Think about other ways (AF)
/*
        __uint32 cur;
        for (cur = LAYER_ADDRESS_SPACE_START_ADDR_INT; 
             cur < LAYER_ADDRESS_SPACE_BOUNDARY_INT;
             cur += (__uint32)PAGE_SIZE)
        {
            //if (IS_TMP_BLOCK(*(xptr*)cur)) 
            _vmm_unmap_severe((void*)cur);
        }
*/
        vmm_cur_ptr = NULL;
        vmm_cur_xptr = XNULL;

        // Anyway we have to notify SM about deletion of the block
        msg.cmd = 34; // bm_delete_tmp_blocks
        msg.trid = trid;
        msg.sid = sid;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);
}

void vmm_enter_exclusive_mode(int *number_of_potentially_allocated_blocks) throw (SednaException)
{
    USemaphoreDown(xmode, __sys_call_error);
    USemaphoreDown(vmm_sm_sem, __sys_call_error);

    try {
        msg.cmd = 27; // bm_enter_exclusive_mode
        msg.trid = trid;
        msg.sid = sid;
        msg.data.reg.num = 0;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

        *number_of_potentially_allocated_blocks = msg.data.reg.num;

        is_exclusive_mode = true;

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }

    USemaphoreUp(vmm_sm_sem, __sys_call_error);
}

void vmm_exit_exclusive_mode() throw (SednaException)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);

    try {
        msg.cmd = 28; // bm_exit_exclusive_mode
        msg.trid = trid;
        msg.sid = sid;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

        is_exclusive_mode = false;

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }

    USemaphoreUp(vmm_sm_sem, __sys_call_error);
    USemaphoreUp(xmode, __sys_call_error);
}

void vmm_memlock_block(xptr p) throw (SednaException)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);

    try {
        p = block_xptr(p);
        
        bool is_busy = _vmm_is_address_busy(XADDR(p));

        if (!(is_busy && TEST_XPTR(p))) 
            throw USER_EXCEPTION(SE1021);

        msg.cmd = 29; // bm_memlock_block
        msg.trid = trid;
        msg.sid = sid;
        msg.data.ptr = *(__int64*)(&p);

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }

    USemaphoreUp(vmm_sm_sem, __sys_call_error);
}

void vmm_memunlock_block(xptr p) throw (SednaException)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);

    try {
        p = block_xptr(p);
        
        bool is_busy = _vmm_is_address_busy(XADDR(p));

        if (!(is_busy && TEST_XPTR(p)))
            throw USER_EXCEPTION(SE1022);

        msg.cmd = 30; // bm_memunlock_block
        msg.trid = trid;
        msg.sid = sid;
        msg.data.ptr = *(__int64*)(&p);

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }

    USemaphoreUp(vmm_sm_sem, __sys_call_error);
}

void vmm_storage_block_statistics(sm_blk_stat /*out*/ *stat) throw (SednaException)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);

    try {
        msg.cmd = 31; // bm_block_statistics
        msg.trid = trid;
        msg.sid = sid;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

        *stat = msg.data.stat;

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }

    USemaphoreUp(vmm_sm_sem, __sys_call_error);
}

/*******************************************************************************
********************************************************************************
  RECOVERY FUNCTIONS
********************************************************************************
*******************************************************************************/
/*
void _vmm_pseudo_alloc_data_block(xptr *p) throw (SednaException)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);

    try {
        msg.cmd = 32; // bm_pseudo_allocate_data_block
        msg.trid = trid;
        msg.sid = sid;

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

        *p = *(xptr*)(&(msg.data.ptr));

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }

    USemaphoreUp(vmm_sm_sem, __sys_call_error);
}

void _vmm_pseudo_delete_block(xptr p) throw (SednaException)
{
    USemaphoreDown(vmm_sm_sem, __sys_call_error);
    try {
        p = block_xptr(p);

        msg.cmd = 33; // bm_pseudo_delete_data_block
        msg.trid = trid;
        msg.sid = sid;
        msg.data.ptr = *(__int64*)(&p);

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }
    USemaphoreUp(vmm_sm_sem, __sys_call_error);
}

void vmm_rcv_alloc_data_block(xptr *p) throw (SednaException)
{
    vector<xptr> d;
    int i = 0;

    while (true)
    {
        __vmm_alloc_data_block(p);
        if (indir_block_set.find(*p) == indir_block_set.end())
            break;
        else
            d.push_back(*p);
    }

    for (i = 0; i < d.size(); i++)
        vmm_delete_block(d[i]);
}
*/
void vmm_rcv_alloc_indir_block(xptr p) throw (SednaException)
{
    vector<xptr> d;
    int i = 0;
    xptr cur;

    while (true)
    {
        _vmm_alloc_data_block(&cur);
        if (cur == p)
            break;
        else
            d.push_back(cur);
    }

    for (i = 0; i < d.size(); i++)
        vmm_delete_block(d[i]);
}

void vmm_rcv_add_to_indir_block_set(xptr p) throw (SednaException)
{
    indir_block_set.insert(p);
}

void vmm_rcv_clear_indir_block_set() throw (SednaException)
{
    indir_block_set.clear();
}


/*******************************************************************************
********************************************************************************
  INTERNAL FUNCTIONS
********************************************************************************
*******************************************************************************/
void vmm_unswap_block(xptr p) throw (SednaException)
{
    if (!(LAYER_ADDRESS_SPACE_START_ADDR_INT <= (int)XADDR(p) && 
          (int)XADDR(p) < LAYER_ADDRESS_SPACE_BOUNDARY_INT))
        throw USER_EXCEPTION(SE1036);

    USemaphoreDown(vmm_sm_sem, __sys_call_error);

    try {

        p = block_xptr(p);

#ifdef VMM_GATHER_STATISTICS
        pair<t_block_usage::iterator, bool> ins_res = _vmm_block_usage.insert(pair<xptr, int>(p, 1));
        if (!ins_res.second) ins_res.first->second++;
#endif
        VMM_TRACE_UNSWAP(p)

        msg.cmd = 26; // bm_get_block
        msg.trid = trid;
        msg.sid = sid;
        msg.data.swap_data.ptr = *(__int64*)(&p);

        if (ssmmsg->send_msg(&msg) != 0)
            throw USER_EXCEPTION(SE1034);

        if (msg.cmd != 0) _vmm_process_sm_error(msg.cmd);

        ramoffs offs = msg.data.swap_data.offs;
        xptr swapped = *(xptr*)(&(msg.data.swap_data.swapped));

        if (swapped != NULL) _vmm_unmap_decent(XADDR(swapped));
        _vmm_remap(XADDR(p), offs);

    } catch (...) {
        USemaphoreUp(vmm_sm_sem, __sys_call_error);
        throw;
    }

    USemaphoreUp(vmm_sm_sem, __sys_call_error);
}


int vmm_data_block_count = 0;
int vmm_data_blocks_allocated()
{
    return vmm_data_block_count;
}

#ifdef VMM_GATHER_STATISTICS

t_block_usage _vmm_block_usage;
t_block_usage _vmm_block_modif;
int _vmm_tmp_block_count = 0;
int _vmm_number_of_checkp_calls = 0;
int _vmm_number_of_sm_callbacks = 0;

void _vmm_inc_number_of_modifications(xptr p)
{
    p = block_xptr(p);
    pair<t_block_usage::iterator, bool> ins_res = _vmm_block_modif.insert(pair<xptr, int>(p, 1));
    if (!ins_res.second) ins_res.first->second++;
}

int vmm_different_blocks_touched() 
{ 
    return _vmm_block_usage.size(); 
}

int vmm_blocks_touched()
{
    int num = 0;
    for (t_block_usage::iterator it = _vmm_block_usage.begin(); it != _vmm_block_usage.end(); it++)
        num += it->second;
    return num;
}

int vmm_different_blocks_modified()
{
    return _vmm_block_modif.size(); 
}

int vmm_blocks_modified()
{
    int num = 0;
    for (t_block_usage::iterator it = _vmm_block_modif.begin(); it != _vmm_block_modif.end(); it++)
        num += it->second;
    return num;
}

int vmm_tmp_blocks_allocated()
{
    return _vmm_tmp_block_count;
}

int vmm_number_of_checkp_calls()
{
    return _vmm_number_of_checkp_calls;
}

int vmm_number_of_sm_callbacks()
{
    return _vmm_number_of_sm_callbacks;
}


#endif



