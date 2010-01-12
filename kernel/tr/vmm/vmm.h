/*
* File:  vmm.h
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/


#ifndef _VMM_H
#define _VMM_H

#include "common/sedna.h"

#include "common/xptr.h"
#include "common/wutypes.h"
#include "common/SSMMsg.h"
#include "common/u/uthread.h"

#include "tr/vmm/os_exceptions.h"
#include "tr/rcv/rcv_test_tr.h"

namespace tr_globals {
    extern session_id sid;
}

//#define VMM_GATHER_STATISTICS
//#define VMM_TRACE
//#define VMM_DEBUG_CHECKP
#define VMM_DEBUG_VERSIONS

#ifdef VMM_DEBUG_CHECKP

#define CHECKP(p)    {                                                                           \
                         VMM_INC_NUMBER_OF_CHECKP_CALLS                                          \
                         vmm_checkp_xptr = p;                                                    \
                         VMM_TRACE_CHECKP(vmm_checkp_xptr)                                       \
                         check_if_null_xptr(vmm_checkp_xptr);                                    \
                         if (vmm_cur_ptr) _vmm_unmap_severe(ALIGN_ADDR(vmm_cur_ptr));            \
                         vmm_cur_ptr = XADDR(vmm_checkp_xptr);                                   \
                         vmm_cur_xptr = vmm_checkp_xptr;                                         \
                         if (!TEST_XPTR(vmm_checkp_xptr)) vmm_unswap_block(vmm_checkp_xptr);     \
                         REFRESH_LRU_STAMP(vmm_checkp_xptr)                                      \
                     }

#else /* ! VMM_DEBUG_CHECKP */

#define CHECKP(p)    {                                                                           \
                         VMM_INC_NUMBER_OF_CHECKP_CALLS                                          \
                         VMM_TRACE_CHECKP(p)                                                     \
                         vmm_cur_ptr = XADDR(p);                                                 \
                         vmm_cur_xptr = p;                                                       \
                         if (!TEST_XPTR(p)) vmm_unswap_block(p);                                 \
                         REFRESH_LRU_STAMP(p)                                                    \
                     }


#endif /* VMM_DEBUG_CHECKP */



#define VMM_SIGNAL_MODIFICATION(p)    {                                                          \
                                          VMM_INC_NUMBER_OF_MODIFICATIONS(p)                     \
                                          VMM_TRACE_SIGNAL_MODIFICATION(p)                       \
                                          if (((vmm_sm_blk_hdr*)((int)(XADDR(p)) & PAGE_BIT_MASK))->trid_wr_access != tr_globals::sid) \
                                          vmm_unswap_block_write(p);                             \
                                          ((vmm_sm_blk_hdr*)((int)(XADDR(p)) & PAGE_BIT_MASK))->is_changed = true;                     \
                                          RECOVERY_CRASH;                                        \
                                      }

#define WRITEP(x) CHECKP(x); VMM_SIGNAL_MODIFICATION(x);

// External interface to VMM
void vmm_preliminary_call() throw (SednaException);
void vmm_determine_region(bool log = false) throw (SednaException);

void vmm_on_session_begin(SSMMsg *_ssmmsg_, bool is_rcv_mode) throw (SednaException);
void vmm_on_session_end() throw (SednaException);
void vmm_on_transaction_begin(bool is_query, TIMESTAMP &ts) throw (SednaException);
void vmm_on_transaction_end() throw (SednaException);

// VMM alloc/delete functions
void vmm_alloc_data_block(xptr /*out*/ *p) throw (SednaException);
void vmm_alloc_tmp_block(xptr /*out*/ *p) throw (SednaException);
void vmm_delete_block(xptr p) throw (SednaException);
void vmm_delete_tmp_blocks() throw (SednaException);

// VMM exclusive mode functions
void vmm_enter_exclusive_mode(int *number_of_potentially_allocated_blocks) throw (SednaException);
void vmm_memlock_block(xptr p) throw (SednaException);
void vmm_memunlock_block(xptr p) throw (SednaException);
void vmm_exit_exclusive_mode() throw (SednaException);

void vmm_storage_block_statistics(sm_blk_stat /*out*/ *stat) throw (SednaException);

// Internal VMM functions
void vmm_unswap_block(xptr p) throw (SednaException);
void vmm_unswap_block_write(xptr p) throw (SednaException);

#ifdef VMM_DEBUG_CHECKP
void _vmm_unmap_severe(void *addr);
#endif /*VMM_DEBUG_CHECKP*/


#ifndef _WIN32
#ifdef HAVE_SPINLOCKS
extern uspinlock *vmm_spin_lock;
#endif
extern bool vmm_is_busy_called;
#endif /*_WIN32*/

extern volatile void * vmm_cur_ptr;
extern xptr vmm_cur_xptr;

extern int vmm_data_block_count;
#define VMM_INC_DATA_BLOCK_COUNT                ++vmm_data_block_count;
int vmm_data_blocks_allocated();


#ifdef VMM_GATHER_STATISTICS

#include <map>

typedef std::map<xptr, int> t_block_usage;

extern t_block_usage _vmm_block_usage;
extern t_block_usage _vmm_block_modif;
extern int _vmm_tmp_block_count;
extern int _vmm_number_of_checkp_calls;
extern int _vmm_number_of_sm_callbacks;

#define VMM_INC_NUMBER_OF_CHECKP_CALLS          ++_vmm_number_of_checkp_calls;
#define VMM_INC_NUMBER_OF_SM_CALLBACKS          ++_vmm_number_of_sm_callbacks;
#define VMM_INC_NUMBER_OF_MODIFICATIONS(p)      _vmm_inc_number_of_modifications(p);
#define VMM_INC_TMP_BLOCK_COUNT                 ++_vmm_tmp_block_count;

void _vmm_inc_number_of_modifications(xptr p);

int vmm_different_blocks_touched();
int vmm_blocks_touched();
int vmm_different_blocks_modified();
int vmm_blocks_modified();
int vmm_tmp_blocks_allocated();
int vmm_number_of_checkp_calls();
int vmm_number_of_sm_callbacks();

#else

#define VMM_INC_NUMBER_OF_CHECKP_CALLS
#define VMM_INC_NUMBER_OF_SM_CALLBACKS
#define VMM_INC_NUMBER_OF_MODIFICATIONS(p)
#define VMM_INC_TMP_BLOCK_COUNT

#endif /*VMM_GATHER_STATISTICS*/



#ifdef VMM_TRACE

#define TRACE_FILE                             "vmm_trace.txt"

void vmm_trace_CHECKP(const xptr& p);
void vmm_trace_signal_modification(const xptr& p);
void vmm_trace_alloc_data_block();
void vmm_trace_alloc_tmp_block();
void vmm_trace_unswap(const xptr& p);
void vmm_trace_delete_block(const xptr& p);


#define VMM_TRACE_CHECKP(p)                    vmm_trace_CHECKP(p);
#define VMM_TRACE_SIGNAL_MODIFICATION(p)       vmm_trace_signal_modification(p);
#define VMM_TRACE_ALLOC_DATA_BLOCK             vmm_trace_alloc_data_block();
#define VMM_TRACE_ALLOC_TMP_BLOCK              vmm_trace_alloc_tmp_block();
#define VMM_TRACE_UNSWAP(p)                    vmm_trace_unswap(p);
#define VMM_TRACE_DELETE_BLOCK(p)              vmm_trace_delete_block(p);

#else

#define VMM_TRACE_CHECKP(p)
#define VMM_TRACE_SIGNAL_MODIFICATION(p)
#define VMM_TRACE_ALLOC_DATA_BLOCK
#define VMM_TRACE_ALLOC_TMP_BLOCK
#define VMM_TRACE_UNSWAP(p)
#define VMM_TRACE_DELETE_BLOCK(p)

#endif /*VMM_TRACE*/



#ifdef LRU
#define REFRESH_LRU_STAMP(p)                   ((vmm_sm_blk_hdr*)((pint)((p).addr) & PAGE_BIT_MASK))->lru = ++(*lru_global_stamp_data);

extern LRU_stamp *lru_global_stamp_data;
#else
#define REFRESH_LRU_STAMP(p)
#endif /*LRU*/


#ifdef VMM_DEBUG_CHECKP
extern xptr vmm_checkp_xptr;
#endif /*VMM_DEBUG_CHECKP*/

inline void check_if_null_xptr(const xptr& p)
{
    if (p == XNULL)
    {
        throw USER_EXCEPTION2(SE1003, "Wrong CHECKP argument");
    }
}

#endif /* _VMM_H */
