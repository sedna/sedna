/*
 * File:  text.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <set>

#include "tr/structures/nodeblocks.h"
#include "tr/structures/nodeoperations.h"

#include "tr/mo/blocks.h"
#include "tr/mo/boundaries.h"
#include "tr/mo/indirection.h"
#include "tr/mo/nodemoutils.h"
#include "tr/mo/modebug.h"

using namespace internal;

typedef std::set<xptr> xptrset;
static xptrset * blocks_to_delete;

void storage_on_transaction_begin()
{
    blocks_to_delete = new xptrset;
    indirectionInitialize();
}

void storage_on_transaction_end()
{
    deleteDeadBlocks();
    delete blocks_to_delete;
}


void deleteDeadBlocks()
{
    for (xptrset::iterator i = blocks_to_delete->begin(); i != blocks_to_delete->end(); i++) {
        vmm_delete_block(*i);
    }
}

bool undeleteBlock(xptr block_xptr)
{
    if (blocks_to_delete->find(block_xptr) != blocks_to_delete->end()) {
        blocks_to_delete->erase(block_xptr);
        return true;
    }
    return false;
}

xptr createBlock(schema_node_cptr schema_node, xptr prev_block_xptr, const xptr &undo_hint, int child_count_hint)
{
    xptr block_xptr, next_block_xptr;
    node_blk_hdr * block;
    int desc_size;

    if (!schema_node.found()) {
        U_ASSERT(prev_block_xptr != XNULL);
        CHECKP(prev_block_xptr);
        schema_node = getBlockHeader(prev_block_xptr)->snode;
    }

    if (undo_hint != XNULL) {
        block_xptr = undo_hint;
    } else if (schema_node->persistent) {
        vmm_alloc_data_block(&block_xptr);
    } else {
        vmm_alloc_tmp_block(&block_xptr);
    }

#ifdef DEBUG_MO_LOG
    elog(EL_DBG, ("MOLOG: createBlock(0x%llx, 0x%llx, 0x%llx, %d) = 0x%llx", schema_node.ptr().to_logical_int(),
            prev_block_xptr.to_logical_int(), undo_hint.to_logical_int(), child_count_hint, block_xptr.to_logical_int()));
#endif

    /* Update siblings' pointers */
    if (prev_block_xptr == XNULL) {
        next_block_xptr = schema_node->bblk;
        schema_node->bblk = block_xptr;
    } else {
        node_blk_hdr * prev_block = getBlockHeader(prev_block_xptr);

        WRITEP(prev_block_xptr);

        U_ASSERT(prev_block->snode == schema_node.ptr());

        next_block_xptr = prev_block->nblk;
        prev_block->nblk = block_xptr;
    }

    if (next_block_xptr != XNULL) {
        WRITEP(next_block_xptr);
        getBlockHeader(next_block_xptr)->pblk = block_xptr;
    }

    /* Initialize block */

    WRITEP(block_xptr);
    desc_size = size_of_node(schema_node->type);

    if (schema_node->has_children()) {
        U_ASSERT(child_count_hint <= schema_node->get_child_count());
        child_count_hint = (child_count_hint == 0 ? schema_node->get_child_count() : child_count_hint);
        desc_size += child_count_hint * sizeof(xptr);
    }

    initNodeBlock(block_xptr, desc_size, schema_node);

    block = getBlockHeader(block_xptr);
    block->pblk = prev_block_xptr;
    block->nblk = next_block_xptr;

    schema_node.modify();
    schema_node->blockcnt++;

    return block_xptr;
}

void deleteBlockVirtually(xptr block_xptr)
{
#ifdef DEBUG_MO_LOG
    elog(EL_DBG, ("MOLOG: deleteBlock(0x%llx)", block_xptr.to_logical_int()));
#endif

    if (IS_DATA_BLOCK(block_xptr)) {
        CHECKP(block_xptr);
        node_blk_hdr * block = (node_blk_hdr *) XADDR(block_xptr);

        U_ASSERT(blocks_to_delete->find(block_xptr) == blocks_to_delete->end());
        U_ASSERT((block->count + block->indir_count) == 0);

        xptr pblk_indir = block->pblk_indir;
        xptr nblk_indir = block->nblk_indir;
        xptr pblk = block->pblk;
        xptr nblk = block->nblk;
        schema_node_cptr snode = block->snode;
        snode.modify();

        if (snode->bblk_indir == block_xptr)
            snode->bblk_indir = nblk_indir;

        if (snode->bblk == block_xptr)
            snode->bblk = nblk;

        snode->blockcnt--;

        col_schema_node_object* snode_as_collection = dynamic_cast<col_schema_node_object*>(&*schema_node_cptr(snode));

        if ((snode_as_collection != NULL) && (snode_as_collection->eblk == block_xptr)) {
            snode_as_collection->eblk = (nblk == XNULL) ? pblk : nblk;
        }

        VMM_SIGNAL_MODIFICATION(block_xptr);

        block->pblk_indir = XNULL;
        block->nblk_indir = XNULL;
        block->pblk = XNULL;
        block->nblk = XNULL;

        if (pblk_indir!=XNULL)
        {
            CHECKP(pblk_indir);
            VMM_SIGNAL_MODIFICATION(pblk_indir);
            getBlockHeader(pblk_indir)->nblk_indir = nblk_indir;
        }

        if (pblk!=XNULL)
        {
            CHECKP(pblk);
            VMM_SIGNAL_MODIFICATION(pblk);
            getBlockHeader(pblk)->nblk = nblk;
        }

        if (nblk_indir!=XNULL)
        {
            CHECKP(nblk_indir);
            VMM_SIGNAL_MODIFICATION(nblk_indir);
            getBlockHeader(nblk_indir)->pblk_indir = pblk_indir;
        }

        if (nblk!=XNULL)
        {
            CHECKP(nblk);
            VMM_SIGNAL_MODIFICATION(nblk);
            getBlockHeader(nblk)->pblk = pblk;
        }

        blocks_to_delete->insert(block_xptr);
    }
}

void updateBlockChains(xptr node_src, xptr node_sink, enum update_hint_t update_hint)
{
    node_blk_hdr * blk;

    node_src = block_xptr(node_src);
    node_sink = block_xptr(node_sink);

    if (!same_block(node_src, node_sink)) {
        indirectionChainDeleteBlock(node_src);
        indirectionChainAddBlock(node_sink);
    }

    if (update_hint == up_insert) { return; }

    blk = getBlockHeader(checkp(node_src));
    if (blk->count == 0 && blk->indir_count == 0) {
        deleteBlockVirtually(node_src);
    }

    if (update_hint == up_move) { return; }
    if (same_block(node_src, node_sink)) { return; }

    blk = getBlockHeader(checkp(node_sink));
    if (blk->count == 0 && blk->indir_count == 0) {
        deleteBlockVirtually(node_sink);
    }
}


void deleteBlock(xptr block_xptr) // depricated
{
    node_blk_hdr * block = getBlockHeader(block_xptr);
    xptr nblk;
    xptr pblk;
    schema_node_cptr snode = XNULL;

    U_ASSERT(false);

    CHECKP(block_xptr);

    nblk = block->nblk;
    pblk = block->pblk;
    snode = block->snode;
    snode.modify();

    if (nblk!=XNULL)
    {
        CHECKP(nblk);
        VMM_SIGNAL_MODIFICATION(nblk);
        getBlockHeader(nblk)->pblk=nblk;
    }

    if (pblk!=XNULL)
    {
        CHECKP(pblk);
        VMM_SIGNAL_MODIFICATION(pblk);
        getBlockHeader(pblk)->nblk=pblk;
    } else {
        snode->bblk = nblk;
    }

    vmm_delete_block(block_xptr) ;
}
