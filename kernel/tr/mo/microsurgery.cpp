/*
 * File:  microsurgery.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string.h>

#include "tr/mo/microsurgery.h"
#include "tr/structures/nodes.h"

void nodeUpdateLinks(xptr node_xptr, xptr child_in_parent)
{
    xptr ldsc, rdsc, indir;

    CHECKP(node_xptr);
    ldsc = ((n_dsc *) XADDR(node_xptr))->ldsc;
    rdsc = ((n_dsc *) XADDR(node_xptr))->rdsc;
    indir = ((n_dsc *) XADDR(node_xptr))->indir;

    if (ldsc != XNULL) {
        CHECKP(ldsc);
        VMM_SIGNAL_MODIFICATION(ldsc);
        ((n_dsc *) XADDR(ldsc))->rdsc = node_xptr;
    }

    if (rdsc != XNULL) {
        CHECKP(rdsc);
        VMM_SIGNAL_MODIFICATION(rdsc);
        ((n_dsc *) XADDR(rdsc))->ldsc = node_xptr;
    }

/* Update child pointer in parent */
    if (child_in_parent != XNULL) {
        CHECKP(child_in_parent);
        VMM_SIGNAL_MODIFICATION(child_in_parent);
        * (xptr *) XADDR(child_in_parent) = node_xptr;
    }

    CHECKP(indir);
    VMM_SIGNAL_MODIFICATION(indir);
    * (xptr*) XADDR(indir) = node_xptr;
}


void fixPointersOnDelete(xptr block_xptr, n_dsc * node)
{
    xptr node_xptr = addr2xptr(node);
    xptr ldsc = node->ldsc;
    xptr rdsc = node->rdsc;
    xptr pdsc = node->pdsc;
    xptr child_in_parent;

    if (ldsc != XNULL) {
        CHECKP(ldsc);
        VMM_SIGNAL_MODIFICATION(ldsc);
        ((n_dsc *) XADDR(ldsc))->rdsc = rdsc;
    }

    if (rdsc != XNULL) {
        CHECKP(rdsc);
        VMM_SIGNAL_MODIFICATION(rdsc);
        ((n_dsc *) XADDR(rdsc))->ldsc = ldsc;
    }

    child_in_parent = findNodeInParentCP(node_xptr);

    if (child_in_parent != XNULL) {
        xptr next_child = getNextDescriptorOfSameSortXptr(node_xptr);

        if (next_child != XNULL) {
            CHECKP(next_child);
            if (getParentIndirection(next_child) != pdsc) {
                next_child = XNULL;
            }
        }

        CHECKP(child_in_parent);
        VMM_SIGNAL_MODIFICATION(child_in_parent);
        memcpy(XADDR(child_in_parent), &next_child, sizeof(xptr));
    }
}

static bool no_block_chain_update_flag = false;

void setNoBlockChainUpdateFlag() {
    no_block_chain_update_flag = true;
    elog(EL_DBG, ("MOLOG: no block chain update"));
}

void unsetNoBlockChainUpdateFlag()
{
    no_block_chain_update_flag = false;
    elog(EL_DBG, ("MOLOG: normal block chain update"));
}

void nodeListDeleteCP(xptr block_xptr, n_dsc * node)
{
    node_blk_hdr * block = getBlockHeaderCP(block_xptr);
    VMM_SIGNAL_MODIFICATION(block_xptr);

    if (node->desc_prev != 0) {
        (GET_DSC(block, node->desc_prev))->desc_next = node->desc_next;
    } else {
        block->desc_first = node->desc_next;
    }

    if (node->desc_next != 0) {
        (GET_DSC(block, node->desc_next))->desc_prev = node->desc_prev;
    } else {
        block->desc_last = node->desc_prev;
    }

#ifdef EL_DEBUG
    memset(node, 0xdeadbeef, sizeof(n_dsc));
#endif /* EL_DEBUG */

    * (shft*) node = block->free_first;
    block->free_first = calcShift(node);
    block->count--;
}


n_dsc * nodeListInsertCP(xptr block_xptr, shft left_node, node_buffer * src, int node_buffer_pos)
{
    node_blk_hdr * block = getBlockHeader(block_xptr);
    n_dsc * node;
    shft node_shft;
    shft right_node;

    CHECKP(block_xptr);
    VMM_SIGNAL_MODIFICATION(block_xptr);

    U_ASSERT(block->free_first != 0);

    if (left_node == LAST_NODE) { left_node = block->desc_last; };
    right_node = (left_node == 0) ? block->desc_first : (GET_DSC(block, left_node))->desc_next;
    node_shft = block->free_first;
    node = GET_DSC(block, node_shft);
    block->free_first = * (shft*) node;

    if (src != NULL) {
        memcpy(node, nodeBufferGetNode(src, node_buffer_pos), src->size);
        U_ASSERT(block->dsc_size >= src->size);
        memset((char *) node + src->size, 0, block->dsc_size - src->size);
    } else {
        memset(node, 0, block->dsc_size);
    }

    node->desc_prev = left_node;
    node->desc_next = right_node;

    if (left_node != 0) {
        (GET_DSC(block, left_node))->desc_next = node_shft;
    } else {
        block->desc_first = node_shft;
    }

    if (right_node != 0) {
        (GET_DSC(block, right_node))->desc_prev = node_shft;
    } else {
        block->desc_last = node_shft;
    }

    block->count++;

    return node;
}


