/*
 * File:  indirection.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/indirection.h"
#include "tr/crmutils/node_utils.h"
#include "tr/structures/nodes.h"
#include "tr/mo/microoperations.h"
#include "tr/mo/blocks.h"
#include "tr/mo/modebug.h"

static enum rollback_mode_t rollback_mode = rbm_normal;
static xptr rollback_record = XNULL;
static xptr last_indir = XNULL;

void indirectionChainDeleteBlock(xptr block_xptr)
{
    node_blk_hdr * block = getBlockHeaderCP(block_xptr);
    schema_node_cptr block_snode = block->snode;

    if (block->indir_count >= block->count &&
           (block->pblk_indir != XNULL ||
            block->nblk_indir != XNULL ||
            block_snode->bblk_indir == block_xptr))
    {
        xptr pblk = block->pblk_indir;
        xptr nblk = block->nblk_indir;

        molog(("MOLOG (chain:0x%llx, block:0x%llx)", block_snode.ptr().to_logical_int(),  block_xptr.to_logical_int()));

        if (block_snode->bblk_indir == block_xptr)
            block_snode.modify()->bblk_indir = nblk;

        if (pblk != XNULL) {
            WRITEP(pblk);
            getBlockHeader(pblk)->nblk_indir = nblk;
        }

        if (nblk != XNULL) {
            WRITEP(nblk);
            getBlockHeader(nblk)->pblk_indir = pblk;
        }

        WRITEP(block_xptr);
        block->pblk_indir = XNULL;
        block->nblk_indir = XNULL;
    }
}


void indirectionChainAddBlock(xptr block_xptr)
{
    node_blk_hdr * block = getBlockHeaderCP(block_xptr);
    schema_node_cptr block_snode = block->snode;

    if (block->indir_count < block->count &&
           (block->pblk_indir == XNULL &&
            block->nblk_indir == XNULL &&
            block_snode->bblk_indir != block_xptr))
    {
        xptr nblk = block_snode->bblk_indir;
        block_snode.modify()->bblk_indir=block_xptr;

        molog(("MOLOG (chain:0x%llx, block:0x%llx)", block_snode.ptr().to_logical_int(),  block_xptr.to_logical_int()));

        if (nblk!=XNULL) {
            WRITEP(nblk);
            getBlockHeader(nblk)->pblk_indir = block_xptr;
        }

        WRITEP(block_xptr);

        block->nblk_indir = nblk;
    }
}

xptr indirectionTableAddRecord(xptr target)
{
    xptr irecord;
    node_blk_hdr * indirection_block;
    node_blk_hdr * target_block = getBlockHeaderCP(target);
    shft * precord;

    if (rollback_mode == rbm_undo) {
        irecord = rollback_record;

        if (undeleteBlock(block_xptr(irecord))) {
            createBlock(XNULL, block_xptr(target), block_xptr(irecord));
        }

        indirection_block = getBlockHeaderCP(irecord);

        /* As we insert our record to the arbitrary place of indirection list,
         * we now need to fix the whole list!
         * TODO: make this list double-linked!
         */

        CHECKP(irecord);
        shft p = 0, i = indirection_block->free_first_indir, ir = calcShift(XADDR(irecord));
        while (i != ir) {
            p = i;
            i = * (shft*) getBlockPointer(irecord, i);
            U_ASSERT(i != 0);
        }
        if (p == 0) {
            precord = &(indirection_block->free_first_indir);
        } else {
            precord = (shft *) getBlockPointer(irecord, p);
        }
    } else {
        CHECKP(target);
        if (target_block->free_first_indir != 0) {
            indirection_block = target_block;
        } else {
            indirection_block = getBlockHeaderCP(target_block->snode->bblk_indir);
        }
        U_ASSERT(indirection_block->free_first_indir != 0);
        irecord = addr2xptr(GET_DSC(indirection_block, indirection_block->free_first_indir));

        CHECKP(irecord);
        precord = &(indirection_block->free_first_indir);
    }

    WRITEP(irecord);
    * precord = * (shft *) XADDR(irecord);
    * (xptr*) (XADDR(irecord)) = target;
    indirection_block->indir_count++;

    last_indir = irecord; // we need this hint to apply dynamic xptr remapping during redo

    return irecord;
}

void indirectionTableDeleteRecord(xptr target_indirection)
{
    xptr object = indirectionDereferenceCP(target_indirection);
    node_blk_hdr * indirection_block = getBlockHeader(target_indirection);

    WRITEP(target_indirection);

    indirection_block->indir_count--;
    * (shft*) (XADDR(target_indirection)) = indirection_block->free_first_indir;
    indirection_block->free_first_indir=calcShift(XADDR(target_indirection));
}


void indirectionSetRollbackMode(enum rollback_mode_t mode)
{
    rollback_mode = mode;
}

enum rollback_mode_t indirectionGetRollbackMode()
{
    return rollback_mode;
}

void indirectionSetRollbackRecord(xptr p)
{
    rollback_record = p;
}

xptr indirectionGetRollbackRecord()
{
    if (rollback_mode == rbm_undo) {
        return rollback_record;
    } else {
        return XNULL;
    }
}

xptr indirectionGetLastRecord()
{
    return last_indir;
}
