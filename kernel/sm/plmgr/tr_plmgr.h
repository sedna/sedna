/*
 * File:  tr_plmgr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TR_PLMGR_H
#define _TR_PLMGR_H

#include "plmgr_core.h"

class tr_plmgr : public plmgr_core
{
protected:
  xptr addr2xptr(const void *addr) { return ADDR2XPTR(addr); }
  vmm_sm_blk_hdr* get_block_hdr(const void *addr) { return (vmm_sm_blk_hdr*)(ALIGN_ADDR(addr)); }
};

#endif
