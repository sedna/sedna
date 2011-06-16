/*
 * File:  shifts.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "tr/mo/microoperations.h"
#include "tr/mo/microsurgery.h"
#include "tr/mo/modebug.h"

#include "tr/mo/nodemoutils.h"

inline xptr shiftNodeToBlockInt(xptr source_block_xptr, xptr dest_block_xptr, xptr source_node_xptr, shft dest_left_node)
{
    xptr child_in_parent_xptr;
    node_base_t * dest_node;
    node_buffer * tmp_node;
    xptr dest_node_xptr;

/* Find a pointer to source node xptr position in parent child list if exists */
    child_in_parent_xptr = findNodeInParentCP(source_node_xptr);

/* Delete descriptor from source block */
    tmp_node = nodeBufferCopyCP(source_node_xptr);
    nodeListDeleteCP(source_block_xptr, (node_base_t *) XADDR(source_node_xptr));

/* Add descriptor to destination block */
    CHECKP(dest_block_xptr);
    if (dest_left_node == LAST_NODE)   { dest_left_node =  getBlockHeader(dest_block_xptr)->desc_last; }
    dest_node = nodeListInsertCP(dest_block_xptr, dest_left_node, tmp_node);
    dest_node_xptr = ADDR2XPTR(dest_node);
    nodeBufferFree(tmp_node);

    updateBlockChains(source_node_xptr, dest_node_xptr, up_move);

    molog(("MOLOG: Shift node 0x%llx -> 0x%llx", source_node_xptr.to_logical_int(), dest_node_xptr.to_logical_int()));

/* Update external pointers */
    nodeUpdateLinks(dest_node_xptr, child_in_parent_xptr);

    return dest_node_xptr;
}

struct xptr_mapping {
    xptr ptr, to;
    size_t field;
    enum mapping_flags {
        mf_none,
        mf_indirection,
        mf_child_in_parent,
        mf_indirection_pointer
    } flags;
};

enum shift_direction_t
{
    shd_begin, shd_end
};

struct shift_node_structure_t
{
    int node_count;
    node_buffer * buffer;
    xptr_mapping * pfl;
};

#define FIX_OFFSET_RDSC 0
#define FIX_OFFSET_LDSC 1
#define FIX_OFFSET_INDIR 2
#define FIX_OFFSET_PDSC 3

inline void setPointerMappingRecord(xptr_mapping * mr, xptr ptr, xptr from, size_t field, xptr_mapping::mapping_flags flags)
{
    if (ptr != XNULL) {
        mr->ptr = ptr;
        mr->field = field;
        mr->flags = flags;
    } else {
        mr->ptr = XNULL;
    }
}

xptr_mapping * addNodeToPFL(xptr_mapping * pointer_to_fix, xptr node_xptr, int child_pos)
{
    CHECKP(node_xptr);

    node_base_t * node = (node_base_t *) XADDR(node_xptr);
    xptr rdsc = node->rdsc;
    xptr ldsc = node->ldsc;

    setPointerMappingRecord(pointer_to_fix + FIX_OFFSET_INDIR, node->indir, node_xptr, 0, xptr_mapping::mf_indirection_pointer);

    if (same_block(rdsc, node_xptr)) {
        setPointerMappingRecord(pointer_to_fix + FIX_OFFSET_RDSC, getIndirectionSafeCP(rdsc), node_xptr, offsetof(node_base_t, ldsc), xptr_mapping::mf_indirection);
    } else {
        setPointerMappingRecord(pointer_to_fix + FIX_OFFSET_RDSC, rdsc, node_xptr, offsetof(node_base_t, ldsc), xptr_mapping::mf_none);
    }

    if (same_block(ldsc, node_xptr)) {
        setPointerMappingRecord(pointer_to_fix + FIX_OFFSET_LDSC, getIndirectionSafeCP(ldsc), node_xptr, offsetof(node_base_t, rdsc), xptr_mapping::mf_indirection);
    } else {
        setPointerMappingRecord(pointer_to_fix + FIX_OFFSET_LDSC, ldsc, node_xptr, offsetof(node_base_t, rdsc), xptr_mapping::mf_none);
    }

    setPointerMappingRecord(pointer_to_fix + FIX_OFFSET_PDSC, findNodeInParentCP(node_xptr, child_pos), node_xptr, 0, xptr_mapping::mf_child_in_parent);

    return pointer_to_fix + 4;
}

int xptr_mapping_compare(const void * a, const void * b)
{
    const xptr_mapping * xa = (const xptr_mapping *) a;
    const xptr_mapping * xb = (const xptr_mapping *) b;

    if (!((xa->flags == xptr_mapping::mf_indirection_pointer) ^ (xb->flags == xptr_mapping::mf_indirection_pointer))) {
        return (xa->ptr.layer == xb->ptr.layer) ? ((int) (xa->ptr.getOffs() - xb->ptr.getOffs())) : ((int) (xa->ptr.layer - xb->ptr.layer));
    } else if (xa->flags == xptr_mapping::mf_indirection_pointer) {
        return -1;
    } else {
        return 1;
    }
}

inline void shiftManyNodesPaste(struct shift_node_structure_t * shift_info, xptr dest_xptr, int pos)
{
    U_ASSERT(dest_xptr != XNULL);
    CHECKP(dest_xptr);

    for (int i = 0; i < shift_info->node_count; i++) {
        xptr node_xptr = addr2xptr(nodeListInsertCP(dest_xptr, pos, shift_info->buffer, i));

        shift_info->pfl[i*4 + FIX_OFFSET_RDSC].to = node_xptr;
        shift_info->pfl[i*4 + FIX_OFFSET_LDSC].to = node_xptr;
        shift_info->pfl[i*4 + FIX_OFFSET_INDIR].to = node_xptr;
        shift_info->pfl[i*4 + FIX_OFFSET_PDSC].to = node_xptr;
    }
}

inline void shiftManyNodesFixPointers(struct shift_node_structure_t * shift_info)
{
    int count = shift_info->node_count * 4;
    xptr_mapping * pfl = shift_info->pfl;

    qsort(pfl, count, sizeof(xptr_mapping), xptr_mapping_compare);

    for (int i = 0; i < count; i++) {
        if (pfl[i].ptr != XNULL) {
            xptr p = pfl[i].ptr;
            if (pfl[i].flags == xptr_mapping::mf_indirection) {
                 p = indirectionDereferenceCP(p);
            }
#ifdef EL_DEBUG
            if (pfl[i].flags == xptr_mapping::mf_indirection_pointer) {
                CHECKP(p);
                molog(("MOLOG: Shift node 0x%llx -> 0x%llx", ((xptr *) XADDR(p))->to_logical_int(), pfl[i].to.to_logical_int()));
            }
#endif /* EL_DEBUG */
            p += pfl[i].field;
            CHECKP(p);
            VMM_SIGNAL_MODIFICATION(p);
            memcpy(XADDR(p), &(pfl[i].to), sizeof(xptr));
        }
    }
}

inline void shiftManyNodesFree(struct shift_node_structure_t * shift_info)
{
    free(shift_info->pfl);
    nodeBufferFree(shift_info->buffer);
}

#define getfieldat(type, base, offset) (* (type *) ((char *) (base) + (offset)))

void shiftManyNodesCopy(struct shift_node_structure_t * shift_info, xptr source_block_xptr, enum shift_direction_t direction)
{
    node_blk_hdr * source_block;
    xptr_mapping * pfl_it; /* pointer fix list */
    node_base_t * node;
    int pos_in_parent;
    int node_count = shift_info->node_count;

    off_t it_field = (direction == shd_end) ? offsetof(node_base_t, desc_prev) : offsetof(node_base_t, desc_next);
    off_t init_field = (direction == shd_end) ? offsetof(node_blk_hdr, desc_last) : offsetof(node_blk_hdr, desc_first);

    CHECKP(source_block_xptr);
    VMM_SIGNAL_MODIFICATION(source_block_xptr);

    source_block = getBlockHeader(source_block_xptr);
    shift_info->buffer = nodeBufferAlloc(source_block->dsc_size, node_count);
    pfl_it = shift_info->pfl = (xptr_mapping *) malloc(node_count * 4 * sizeof(xptr_mapping));
    pos_in_parent = schema_node_cptr(source_block->snode)->getIndex();

    /* Fill fix pointer list. We MUST do it before any nodes are deleted from the block
        (because left or right siblings may lay in this block). */

    node = getDsc((char *) source_block, getfieldat(shft, source_block, init_field));
    for (int i = 0; i < node_count; i++) {
        U_ASSERT(node != NULL);
        pfl_it = addNodeToPFL(pfl_it, ADDR2XPTR(node), pos_in_parent);
        CHECKP(source_block_xptr);
        node = getDsc((char*) source_block, getfieldat(shft, node, it_field));
    }

    /* Cut nodes to buffer */
    CHECKP(source_block_xptr);
    node = getDsc((char*) source_block, getfieldat(shft, source_block, init_field));
    for (int i = 0; i < node_count; i++) {
        node_base_t * prev_node = getDsc((char *) source_block, getfieldat(shft, node, it_field));
        nodeBufferAddCP(shift_info->buffer, ADDR2XPTR(node));
        nodeListDeleteCP(source_block_xptr, node);
        node = prev_node;
    }
}

void widenBlockDescriptor(xptr block_ptr, shft new_dsc_size, int irecord_count)
{
    node_blk_hdr * source_block = getBlockHeaderCP(block_ptr);
    shift_node_structure_t shift_info = {source_block->count};
    size_t indirection_tail_size = irecord_count * sizeof(xptr);
    void * indirection_tail_point = XADDR(block_ptr + PAGE_SIZE - indirection_tail_size);
    void * indirection_tail;

    if (irecord_count > 0) {
        indirection_tail = malloc(indirection_tail_size);
        memcpy(indirection_tail, indirection_tail_point, indirection_tail_size);
    }

    shiftManyNodesCopy(&shift_info, block_ptr, shd_end);

    CHECKP(block_ptr);
    source_block->dsc_size = new_dsc_size;
    clearNodeBlock(block_ptr);

    shiftManyNodesPaste(&shift_info, block_ptr, 0);

    if (irecord_count > 0) {
        memcpy(indirection_tail_point, indirection_tail, indirection_tail_size);
        free(indirection_tail);
    }

    shiftManyNodesFixPointers(&shift_info);
    shiftManyNodesFree(&shift_info);
}


void shiftManyNodesToNextBlock(xptr source_block_xptr, int node_count)
{
    shift_node_structure_t shift_info = {node_count};
    xptr dest_block_xptr = getBlockHeaderCP(source_block_xptr)->nblk;

    shiftManyNodesCopy(&shift_info, source_block_xptr, shd_end);
    shiftManyNodesPaste(&shift_info, dest_block_xptr, 0);
    shiftManyNodesFixPointers(&shift_info);
    shiftManyNodesFree(&shift_info);
    updateBlockChains(source_block_xptr, dest_block_xptr, up_move);

    MOCHECK(checkBlock(source_block_xptr));
    MOCHECK(checkBlock(dest_block_xptr));
}


void shiftManyNodesToPreviousBlock(xptr source_block_xptr, int node_count)
{
    shift_node_structure_t shift_info = {node_count};
    xptr dest_block_xptr = getBlockHeaderCP(source_block_xptr)->pblk;

    shiftManyNodesCopy(&shift_info, source_block_xptr, shd_begin);
    shiftManyNodesPaste(&shift_info, dest_block_xptr, LAST_NODE);
    shiftManyNodesFixPointers(&shift_info);
    shiftManyNodesFree(&shift_info);
    updateBlockChains(source_block_xptr, dest_block_xptr, up_move);

    MOCHECK(checkBlock(source_block_xptr));
    MOCHECK(checkBlock(dest_block_xptr));
}


xptr shiftOneNodeToNextBlock(xptr source_block_xptr)
{
    xptr dest_block_xptr;
    node_blk_hdr * source_block = getBlockHeader(source_block_xptr);
    xptr source_node_xptr;

/* Get next block */
    CHECKP(source_block_xptr);
    dest_block_xptr = source_block->nblk;
    U_ASSERT(dest_block_xptr != XNULL);
    source_node_xptr = getLastBlockNode(source_block_xptr);

/* Shift node */
    return shiftNodeToBlockInt(source_block_xptr, dest_block_xptr, source_node_xptr, 0);
}



xptr shiftOneNodeToPreviousBlock(xptr source_block_xptr)
{
    xptr dest_block_xptr;
    node_blk_hdr * source_block = getBlockHeader(source_block_xptr);
    xptr source_node_xptr;

/* Get next block */
    CHECKP(source_block_xptr);
    dest_block_xptr = source_block->pblk;
    U_ASSERT(dest_block_xptr != XNULL);
    source_node_xptr = getFirstBlockNode(source_block_xptr);

/* Shift node */
    return shiftNodeToBlockInt(source_block_xptr, dest_block_xptr, source_node_xptr, LAST_NODE);
}
