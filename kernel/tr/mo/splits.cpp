/*
 * File:  splits.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/microoperations.h"
#include "tr/mo/microsurgery.h"
#include "tr/mo/blocks.h"
#include "tr/mo/nodemoutils.h"

#include "auxiliary/bit_set.h"

inline int canAcceptNodes(xptr block_xptr, int desc_size)
{
    if (block_xptr != XNULL) {
        CHECKP(block_xptr);
        node_blk_hdr * block = getBlockHeader(block_xptr);

        return (block->dsc_size >= desc_size) ? (getPageDescriptorCapacitySP(block) - block->count) : 0;
    }
    return 0;
}


inline bool tryWidenSingleBlock(xptr block_ptr, node_blk_hdr * block, int required_dsc_size)
{
    CHECKP(block_ptr);
    xptr * endpoint = (xptr *) ((char *) XADDR(block_ptr) + PAGE_SIZE) - 1;
    int total_cells = getPageDescriptorCapacitySP(block_ptr);
    int new_total_cells;
    sedna::Bitset free_cells(total_cells);
    void * indir;
    int save_indir_count;
    int i;

    free_cells.clear();

    indir = getBlockPointer(block_ptr, getBlockHeaderCP(block_ptr)->free_first_indir);

    try {
        while (indir != NULL) {
            i = endpoint - ((xptr *) indir);
            free_cells.setAt(i);

            indir = getBlockPointer(block_ptr, * (shft *) indir);
        };
    } catch (ANY_SE_EXCEPTION) {
        throw SYSTEM_EXCEPTION("Exception at tryWidenSingleBlock");
    }

    try {
        i = total_cells - 1;
        while (i >= 0 && free_cells.testAt(i)) { i--; }
    } catch (ANY_SE_EXCEPTION) {
        throw SYSTEM_EXCEPTION("Exception at tryWidenSingleBlock");
    }

    save_indir_count = i + 1;

    if (save_indir_count * (required_dsc_size + sizeof(xptr)) <= (PAGE_SIZE - sizeof(node_blk_hdr))) {
        shft prev;
        shft indir_count = block->indir_count;

        widenBlockDescriptor(block_ptr, required_dsc_size, save_indir_count);

        WRITEP(block_ptr);
        block->indir_count = indir_count;
        new_total_cells = getPageDescriptorCapacitySP(block_ptr);
        U_ASSERT(new_total_cells <= total_cells);
        total_cells = new_total_cells;
        U_ASSERT(total_cells >= save_indir_count);

        try {
            prev = 0;
            for (i = total_cells - 1; i >=0 ; i--) {
                if (free_cells.testAt(i)) {
                    void * iptr = endpoint - i;
                    * (shft *) iptr = prev;
                    prev = calcShift(iptr);
                }
            }
        } catch (ANY_SE_EXCEPTION) {
            throw SYSTEM_EXCEPTION("Exception at tryWidenSingleBlock");
        }

        getBlockHeader(block_ptr)->free_first_indir = prev;

        MOCHECK(checkBlock(block_ptr));

        return true;
    }

    return false;
}

xptr widenDescriptor(xptr node_xptr, int pos, xptr set_value)
{
    xptr node_indir = getIndirectionSafeCP(node_xptr);
    node_blk_hdr * block = getBlockHeaderCP(node_xptr);
    int required_dsc_size = size_of_node(block->node_type) + (pos + 1) * sizeof(xptr);
    int node_position = 0;
    int nodes_to_move;
    xptr dest_block;
    bool widened = false;

    molog(("MOLOG (0x%llx, %d, 0x%llx)", node_xptr.to_logical_int(), pos, set_value.to_logical_int()));

    if (MAX(block->count, block->indir_count) * (required_dsc_size + sizeof(xptr)) <= (PAGE_SIZE - sizeof(node_blk_hdr))) {
        widened = tryWidenSingleBlock(block_xptr(node_xptr), block, required_dsc_size);
    }

    if (!widened) {
        for (node_base_t * node_it = getDsc((char *) xaddr(block_xptr(node_xptr)), block->desc_first);
          (void *) node_it != XADDR(node_xptr);
          node_it = getDsc((char *) xaddr(block_xptr(node_xptr)), node_it->desc_next)) {
            U_ASSERT(node_it != NULL);
            node_position++;
        }

        if (block->count - node_position < node_position + 1) {
            /* Move last nodes to the next block */
            nodes_to_move = block->count - node_position;

            dest_block = block->nblk;
            while (nodes_to_move > 0) {
                int n;

                if ((n = canAcceptNodes(dest_block, required_dsc_size)) > 0) {
                    n = MIN(n, nodes_to_move);
                    shiftManyNodesToNextBlock(block_xptr(node_xptr), n);
                    nodes_to_move -= n;
                }

                if (nodes_to_move == 0) { break; }
                CHECKP(node_xptr);
                dest_block = createBlock(XNULL, block_xptr(node_xptr));
            }
        } else {
            /* Move first nodes to the previous block */
            nodes_to_move = node_position + 1;

            dest_block = block->pblk;
            while (nodes_to_move > 0) {
                int n;

                if ((n = canAcceptNodes(dest_block, required_dsc_size)) > 0) {
                    n = MIN(n, nodes_to_move);
                    shiftManyNodesToPreviousBlock(block_xptr(node_xptr), n);
                    nodes_to_move -= n;
                }

                if (nodes_to_move == 0) { break; }
                CHECKP(node_xptr);
                dest_block = createBlock(block->snode, block->pblk);
            }
        }
    }

    node_xptr = indirectionDereferenceCP(node_indir);
    setNodeChild(node_xptr, pos, set_value);

    return node_xptr;
}

xptr splitBlock(xptr node_xptr)
{
    xptr node_indir;
    node_blk_hdr block;

    CHECKP(node_xptr);
    node_indir = getIndirectionSafeCP(node_xptr);
    memcpy(&block, getBlockHeader(node_xptr), getHeaderSize(node_xptr));

    if (canAcceptNodes(block.pblk, block.dsc_size) > 1) {
        shiftOneNodeToPreviousBlock(block_xptr(node_xptr));
    } else if (canAcceptNodes(block.nblk, block.dsc_size) > 1) {
        shiftOneNodeToNextBlock(block_xptr(node_xptr));
    } else {
        U_ASSERT(block.count > 3);

        xptr new_block = createBlock(XNULL, block_xptr(node_xptr));
        int new_block_capacity = canAcceptNodes(new_block, block.dsc_size) - 1;
        int to_move = MIN(block.count / 2, new_block_capacity);

        U_ASSERT(to_move > 0);
        shiftManyNodesToNextBlock(block_xptr(node_xptr), to_move);
    }

    return indirectionDereferenceCP(node_indir);
}
