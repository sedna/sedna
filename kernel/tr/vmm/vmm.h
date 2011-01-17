/*
 * File:  vmm.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 *
 * Virtual Memory Manager.
 * Provides interface for working with persistent memory - data in blocks.
 */

#ifndef _VMM_H
#define _VMM_H

#include "common/sedna.h"

#include "common/xptr.h"
#include "common/wutypes.h"
#include "common/SSMMsg.h"
#include "common/u/uthread.h"
#include "common/u/uatomic.h"

#include "common/sm_vmm_data.h"

#include "tr/vmm/os_exceptions.h"
#include "tr/rcv/rcv_test_tr.h"
#include "tr/vmm/vmmtrace.h"

// #define VMM_LINUX_DEBUG_CHECKP
// #define VMM_DEBUG_CHECKP

namespace tr_globals {
    extern session_id sid;
}

extern xptr vmm_cur_xptr;
extern volatile lsize_t vmm_cur_offs;

void vmm_init_block_counter();
uint64_t vmm_get_block_counter();

void vmm_determine_region(bool log = false);

void vmm_preliminary_call(lsize_t layer_size);

void vmm_on_session_begin(SSMMsg *_ssmmsg_, bool is_rcv_mode);
void vmm_on_session_end();
void vmm_on_transaction_begin(bool is_query, TIMESTAMP &ts);
void vmm_on_transaction_end();

void vmm_alloc_data_block(xptr /*out*/ *p);
void vmm_alloc_tmp_block(xptr /*out*/ *p);
void vmm_delete_block(xptr p);
void vmm_delete_tmp_blocks();

void vmm_unswap_block(xptr p);
void vmm_unswap_block_write(xptr p);

// This function unmaps all blocks mapped on this transaction
void vmm_unmap_all_blocks();

/* Microtransaction is the mechanism with isolated block readings
 * On the end of microtransaction all blocks read during its work are unmapped
 */

class VMMMicrotransaction {
  private:
    bool mStarted;
    static bool singletonSentinell;
  public:
    void begin();
    void end();

    VMMMicrotransaction() : mStarted(false) {};
    ~VMMMicrotransaction();
};

#ifdef VMM_LINUX_DEBUG_CHECKP

void __vmmdcp_checkp(xptr p);
void __vmmdcp_vmm_signal_modification(xptr p);

#define CHECKP(p) { __vmmdcp_checkp(p); }
#define VMM_SIGNAL_MODIFICATION(p) { __vmmdcp_vmm_signal_modification(p); }

#endif

inline void check_if_null_xptr(const xptr& p)
{
    if (p == XNULL) {
#ifdef VMM_LINUX_DEBUG_CHECKP
        throw SYSTEM_EXCEPTION("Wrong CHECKP argument");
#else /* ! VMM_LINUX_DEBUG_CHECKP */
        throw USER_EXCEPTION2(SE1003, "Wrong CHECKP argument");
#endif /* VMM_LINUX_DEBUG_CHECKP */
    }
}

#ifdef VMM_DEBUG_CHECKP

void vmm_unmap(void *addr);
extern xptr vmm_checkp_xptr;
#define CHECKP(p)    {                                                                           \
                         vmm_checkp_xptr = (p);                                                  \
                         VMM_TRACE_CHECKP(vmm_checkp_xptr);                                      \
                         check_if_null_xptr(vmm_checkp_xptr);                                    \
                         if (!same_block(vmm_checkp_xptr, vmm_cur_xptr)) {                       \
                             if (vmm_cur_offs) vmm_unmap(ALIGN_ADDR(XADDR(vmm_cur_xptr)));       \
                             vmm_cur_offs = vmm_checkp_xptr.getOffs();                           \
                             vmm_cur_xptr = vmm_checkp_xptr;                                     \
                             if (!TEST_XPTR(vmm_checkp_xptr)) vmm_unswap_block(vmm_checkp_xptr); \
                         }                                                                       \
                     }

#else /* ! VMM_DEBUG_CHECKP */

#ifndef VMM_LINUX_DEBUG_CHECKP

#define CHECKP(p)    {                                                                           \
                         VMM_TRACE_CHECKP(p);                                                    \
                         vmm_cur_offs = (p).getOffs();                                           \
                         vmm_cur_xptr = (p);                                                     \
                         /* Make sure block won't be unswapped before cur_offs assignment. */    \
                         AO_compiler_barrier();                                                  \
                         if (!TEST_XPTR(p)) vmm_unswap_block(p);                                 \
                     }

#endif /* VMM_LINUX_DEBUG_CHECKP */

#endif /* VMM_DEBUG_CHECKP */

#ifndef VMM_LINUX_DEBUG_CHECKP

#define VMM_SIGNAL_MODIFICATION(p)    {                                                          \
                                          VMM_TRACE_SIGNAL_MODIFICATION(p)                       \
                                          if (((vmm_sm_blk_hdr*)((uintptr_t)(XADDR(p)) & PAGE_BIT_MASK))->trid_wr_access != tr_globals::sid) \
                                          vmm_unswap_block_write(p);                             \
                                          ((vmm_sm_blk_hdr*)((uintptr_t)(XADDR(p)) & PAGE_BIT_MASK))->is_changed = true;                     \
                                          RECOVERY_CRASH;                                        \
                                      }

#endif /* VMM_LINUX_DEBUG_CHECKP */

#define WRITEP(x) CHECKP(x); VMM_SIGNAL_MODIFICATION(x);

inline
xptr checkp(const xptr a) { CHECKP(a); return a; }

inline
void * checkpxaddr(const xptr a) { CHECKP(a); return XADDR(a); }

void vmm_storage_block_statistics(sm_blk_stat /*out*/ *stat);


#endif /* _VMM_H */
