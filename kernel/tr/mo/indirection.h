/*
 * File:  indirection.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _INDIRECTION_H
#define _INDIRECTION_H

#include "common/sedna.h"
#include "common/xptr/xptr.h"

#include "tr/vmm/vmm.h"

enum rollback_mode_t {
    rbm_normal, rbm_undo, rbm_redo
};

/* Delete block from indirection chain if needed */
void indirectionChainDeleteBlock(xptr block_xptr);

/* Add block to indirection chain if needed */
void indirectionChainAddBlock(xptr block_xptr);

xptr indirectionTableAddRecord(xptr target);
void indirectionTableDeleteRecord(xptr target_indirection);

void indirectionSetRollbackMode(enum rollback_mode_t mode);
void indirectionSetRollbackRecord(xptr p);
xptr indirectionGetLastRecord();
xptr indirectionGetRollbackRecord();
enum rollback_mode_t indirectionGetRollbackMode();

inline void indirectionInitialize() {
    indirectionSetRollbackMode(rbm_normal);
}

inline xptr indirectionDereferenceCP(xptr indirection)
{
    if (indirection != XNULL) {
        CHECKP(indirection);
        return *((xptr*) XADDR(indirection));
    }
    return XNULL;
}

#endif /* _MICROSURGERY_H */
