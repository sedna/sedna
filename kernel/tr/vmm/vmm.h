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
#include "tr/models/SednaModule.h"

#include "common/xptr/xptr.h"
#include "common/xptr/wutypes.h"
#include "common/ssmmsg/SSMMsg.h"
#include "u/uthread.h"
#include "u/uatomic.h"

#include "common/xptr/sm_vmm_data.h"

#include "tr/vmm/os_exceptions.h"
#include "tr/vmm/vmmtrace.h"

// #define VMM_LINUX_DEBUG_CHECKP
// #define VMM_DEBUG_CHECKP
extern void  *LAYER_ADDRESS_SPACE_START_ADDR;
extern void  *LAYER_ADDRESS_SPACE_BOUNDARY;
extern uintptr_t LAYER_ADDRESS_SPACE_START_ADDR_INT;
extern uintptr_t LAYER_ADDRESS_SPACE_BOUNDARY_INT;
extern lsize_t LAYER_ADDRESS_SPACE_SIZE;


/* use this macros when you want obtain pointer in terms of process' virtual address space */
#define XADDR(p)                ((void *)(LAYER_ADDRESS_SPACE_START_ADDR_INT + (p).getOffs()))
#define XADDR_INT(p)            (LAYER_ADDRESS_SPACE_START_ADDR_INT + (p).getOffs())
#define BLOCKXPTR(a)            cxptr(a.layer,((a).getOffs() & PAGE_BIT_MASK))

#define ADDR2XPTR(a)            cxptr(*(t_layer*)(((uintptr_t)(a)) & PAGE_BIT_MASK),   \
                                      *(lsize_t *)((((uintptr_t)(a)) & PAGE_BIT_MASK) + sizeof(t_layer)) + \
                                      (lsize_t)(((uintptr_t)(a)) & PAGE_REVERSE_BIT_MASK))
#define TEST_XPTR(p)            (*(t_layer*)((LAYER_ADDRESS_SPACE_START_ADDR_INT + (p).getOffs()) & PAGE_BIT_MASK) == (p).layer)
#define ALIGN_ADDR(a)           ((void*)((uintptr_t)(a) & PAGE_BIT_MASK))
#define ALIGN_OFFS(a)           ((lsize_t)((lsize_t)(a) & PAGE_BIT_MASK))

#define XOFFS2ADDR(p)           ((void *)(LAYER_ADDRESS_SPACE_START_ADDR_INT + (p)))
#define LAYERS_EQUAL(a, p)      (*(t_layer*)((uintptr_t)(a) & PAGE_BIT_MASK) == ((p).layer))


inline
void * xaddr(const xptr p) {
    return XADDR(p);
}

inline xptr addr2xptr(const void * p)
{
    U_ASSERT(LAYER_ADDRESS_SPACE_START_ADDR_INT + ((xptr *) (((uintptr_t)p) & PAGE_BIT_MASK))->offs ==
             (((uintptr_t) p ) & PAGE_BIT_MASK));
    return cxptr(* (t_layer*) (((uintptr_t) p) & PAGE_BIT_MASK), (lsize_t)(((uintptr_t)p) - LAYER_ADDRESS_SPACE_START_ADDR_INT));
}

inline bool same_block(const xptr& a, const xptr& b) {
    return (BLOCKXPTR(a) == BLOCKXPTR(b));
}

inline bool isTmpBlock(const xptr &p) { return p.layer >= TMP_LAYER_STARTS_WITH; };

namespace tr_globals {
    extern session_id sid;
}

extern xptr vmm_cur_xptr;
extern volatile lsize_t vmm_cur_offs;

class VirtualMemoryManager : public SednaModule{
    bool isSessionInitialized;
    bool isTransactionInitialized;
    bool isReadOnlyQuery;
    bool isRunRecovery;
    TIMESTAMP timeStamp;
    SSMMsg * ssmmsgClient;
    
    transaction_id obtainTransactionId();
    void releaseTransactionId();
    void preliminaryCall(lsize_t layer_size);
    void readWriteLayerSizeOnCreateDatabase(lsize_t *data, bool write);
public:
    VirtualMemoryManager(bool _isRunRecovery): 
                                isSessionInitialized(false), 
                                isTransactionInitialized(false),
                                isRunRecovery(false) {};
    
    void determineRegion();
    virtual void onSessionBegin();
    virtual void onSessionEnd();

    virtual void onTransactionBegin();
    virtual void onTransactionEnd();
    
    void setQueryMode (bool mode) { isReadOnlyQuery = mode; };
    TIMESTAMP getTimestamp () { return timeStamp; };
};

void vmm_init_block_counter();
uint64_t vmm_get_block_counter();

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
