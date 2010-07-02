/*
 * File:  microsurgery.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string.h>

#include "tr/mo/microsurgery.h"
#include "tr/structures/descriptor.h"

#include "tr/mo/nodemoutils.h"

using namespace internal;

void nodeUpdateLinks(xptr node_xptr, xptr child_in_parent)
{
    xptr ldsc, rdsc, indir;
    node_base_t * node = ((node_base_t *) XADDR(node_xptr));

    CHECKP(node_xptr);
    ldsc = node->ldsc;
    rdsc = node->rdsc;
    indir = node->indir;

    if (ldsc != XNULL) {
        CHECKP(ldsc);
        VMM_SIGNAL_MODIFICATION(ldsc);
        ((node_base_t *) XADDR(ldsc))->rdsc = node_xptr;
    }

    if (rdsc != XNULL) {
        CHECKP(rdsc);
        VMM_SIGNAL_MODIFICATION(rdsc);
        ((node_base_t *) XADDR(rdsc))->ldsc = node_xptr;
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


void fixPointersOnDelete(xptr block_xptr, node_base_t * node)
{
    xptr node_xptr = addr2xptr(node);
    xptr ldsc = node->ldsc;
    xptr rdsc = node->rdsc;
    xptr pdsc = node->pdsc;
    xptr child_in_parent;

    if (ldsc != XNULL) {
        WRITEP(ldsc);
        ((node_base_t *) XADDR(ldsc))->rdsc = rdsc;
    }

    if (rdsc != XNULL) {
        WRITEP(rdsc);
        ((node_base_t *) XADDR(rdsc))->ldsc = ldsc;
    }

    child_in_parent = findNodeInParentCP(node_xptr);

    if (child_in_parent != XNULL) {
        xptr next_child = getNextDescriptorOfSameSort(node_xptr);

        if (next_child != XNULL) {
            CHECKP(next_child);
            if (nodeGetParentIndirection(next_child) != pdsc) {
                next_child = XNULL;
            }
        }

        CHECKP(child_in_parent);
        VMM_SIGNAL_MODIFICATION(child_in_parent);
        memcpy(XADDR(child_in_parent), &next_child, sizeof(xptr));
    }
}

void nodeListDeleteCP(xptr block_xptr, node_base_t * node)
{
    node_blk_hdr * block = getBlockHeaderCP(block_xptr);
    VMM_SIGNAL_MODIFICATION(block_xptr);

    if (node->desc_prev != 0) {
        getPrevDsc((char *) block, node)->desc_next = node->desc_next;
    } else {
        block->desc_first = node->desc_next;
    }

    if (node->desc_next != 0) {
        getNextDsc((char *) block, node)->desc_prev = node->desc_prev;
    } else {
        block->desc_last = node->desc_prev;
    }

#ifdef EL_DEBUG
    memset(node, 0xdeadbeef, sizeof(node_base_t));
#endif /* EL_DEBUG */

    * (shft*) node = block->free_first;
    block->free_first = calcShift(node);
    block->count--;
}


node_base_t * nodeListInsertCP(xptr block_xptr, shft left_node, node_buffer * src, int node_buffer_pos)
{
    node_blk_hdr * block = getBlockHeader(block_xptr);
    node_base_t * node;
    shft node_shft;
    shft right_node;

    WRITEP(block_xptr);

    U_ASSERT(block->free_first != 0);

    if (left_node == LAST_NODE) { left_node = block->desc_last; };
    right_node = (left_node == 0) ? block->desc_first : (getDsc((char *) block, left_node))->desc_next;
    node_shft = block->free_first;
    node = getDsc((char*)block, node_shft);
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
        getDsc((char *) block, left_node)->desc_next = node_shft;
    } else {
        block->desc_first = node_shft;
    }

    if (right_node != 0) {
        getDsc((char *) block, right_node)->desc_prev = node_shft;
    } else {
        block->desc_last = node_shft;
    }

    block->count++;

    return node;
}

xptr findNodeInParentCP(const xptr &node_xptr, int hint_child)
{
    node_base_t * node = (node_base_t *) XADDR(node_xptr);

    CHECKP(node_xptr);

    if (node->pdsc == XNULL) {
        return XNULL;
    } else if ((node->desc_prev != 0) && (getBase(nodeGetPrev(node_xptr))->pdsc == node->pdsc)) {
        return XNULL;
    } else {
        xptr parent = indirectionDereferenceCP(node->pdsc);
        xptr * childx;
        int childcount;

        CHECKP(parent);
        childcount = getChildList(parent, childx);

        if (hint_child == -1) {
            while ((childcount > 0) && (*childx != node_xptr)) {
                childcount--;
                childx++;
            }

            return (childcount == 0) ? XNULL : ADDR2XPTR(childx);
        } else {
            return ((childcount > hint_child) && (childx[hint_child] == node_xptr)) ? addr2xptr(childx + hint_child) : XNULL;
        }
    }
}

