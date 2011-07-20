/*
 * File:  nodeblocks.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef NODEBLOCKS_H_
#define NODEBLOCKS_H_

#include "common/xptr.h"
#include "tr/vmm/vmm.h"
#include "tr/structures/nodetypes.h"

namespace internal {

    struct node_blk_hdr {
        vmm_sm_blk_hdr __vmm_data;  /* sm/vmm parameters */

        shft sz;

        xptr pblk;              /* previoius block */
        xptr nblk;              /* next block */

        xptr snode;             /* pointer to the scm node; 0 for text blocks */
        shft dsc_size;          /* size of the descriptor in bytes */
        shft desc_first;        /* shift to the first descriptor in block */
        shft desc_last;         /* shift to the last descriptor in block */
        shft count;             /* total number of descriptors in block */
        shft free_first;        /* shift to the first empty space in block */

        shft indir_count;       /* total number of indirection records in block*/
        shft free_first_indir;  /* shift to the first free indirection pointer*/
        xptr pblk_indir;        /* previous block with free indirection space*/
        xptr nblk_indir;        /* next block with free indirection space*/

        t_item node_type;       /* duplicates type of node declaration from schema node */
    };

    inline static shft getBlockCapacity(node_blk_hdr * hdr) {
        return ((PAGE_SIZE - hdr->sz) / (hdr->dsc_size + sizeof(xptr)));
    }

    inline static
    node_blk_hdr * getBlockHeader(xptr p) { return (node_blk_hdr *) XADDR(block_xptr(p)); };

    inline static shft getHeaderSize(xptr p) { return getBlockHeader(p)->sz; };

    inline static
    node_blk_hdr * getBlockHeaderCP(xptr p) { CHECKP(p); return (node_blk_hdr *) XADDR(block_xptr(p)); };

    inline static shft getBlockCapacity(xptr p) {
        return getBlockCapacity(getBlockHeaderCP(p));
    };

    inline static
    void * getBlockPointer(xptr p, shft i) { if (i == 0) { return NULL; } else { CHECKP(p); return xaddr(block_xptr(p) + i); } }

    inline static
    xptr blockGetFreeIndirection(xptr p) { return block_xptr(p) + ((node_blk_hdr *) XADDR(block_xptr(p)))->free_first_indir; }
};

xptr getNonemptyBlockLookFore(xptr block);
xptr getNonemptyBlockLookBack(xptr block);

xptr getNextNonemptyBlock(xptr block);
xptr getPrevNonemptyBlock(xptr block);

static inline
xptr getFirstBlockNode(xptr block) {
    internal::node_blk_hdr * bh = (internal::node_blk_hdr *) XADDR(block);
    return bh->desc_first == 0 ? XNULL : block_offset(block, bh->desc_first);
}

static inline
xptr getLastBlockNode(xptr block) {
    internal::node_blk_hdr * bh = (internal::node_blk_hdr *) XADDR(block);
    return bh->desc_last == 0 ? XNULL : block_offset(block, bh->desc_last);
}

#endif /* NODEBLOCKS_H_ */
