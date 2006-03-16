/*
 * File:  sm_plmgr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SM_PLMGR_H
#define _SM_PLMGR_H

#include "plmgr_core.h"
#include "bm_core.h"

inline xptr sm_addr2xptr(const void *addr) 
{ 
    char *hdr = ((char*)buf_mem_addr + ((uint32)addr - (uint32)buf_mem_addr) / PAGE_SIZE * PAGE_SIZE);
    return CONSTRUCT_XPTR(
            *(t_layer*)hdr,
            (void*)(*(uint32*)(hdr + sizeof(t_layer)) + 
                    ((uint32)addr - (uint32)buf_mem_addr) % PAGE_SIZE));
}

inline vmm_sm_blk_hdr* sm_get_block_hdr(const void *addr) 
{ 
    return (vmm_sm_blk_hdr*)((char*)buf_mem_addr + 
                             ((uint32)addr - (uint32)buf_mem_addr) / PAGE_SIZE * PAGE_SIZE);
}


class sm_plmgr : public plmgr_core
{
public:
  LONG_LSN recoverDataBase();

protected:
  xptr addr2xptr(const void *addr) { return sm_addr2xptr(addr); }
  vmm_sm_blk_hdr* get_block_hdr(const void *addr) { return sm_get_block_hdr(addr); }
};

#endif

