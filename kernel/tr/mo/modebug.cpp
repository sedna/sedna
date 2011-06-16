/*
 * File:  modebug.cpp
 *
 * Several consistency check functions
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/microsurgery.h"
#include "tr/mo/modebug.h"

#include "tr/tr_base.h"
#include "tr/structures/schema.h"
#include "tr/vmm/vmm.h"
#include "tr/strings/strings_base.h"
#include "tr/pstr/pstr.h"

#include <streambuf>
#include <sstream>

#include "tr/structures/nodeoperations.h"
#include "tr/structures/nodeutils.h"
#include "tr/structures/nodeinterface.h"

using namespace internal;

enum consistency_error_t consistency_error = ce_none;

#define CHECK_INVARIANT(predicate, error) if (!(predicate)) { consistency_error = error; U_ASSERT(false); return false; }

inline bool checkNodeOuterPointers(xptr node_ptr) {
    node_base_t * node = (node_base_t *) XADDR(node_ptr);
    xptr test_ptr;

    CHECKP(node_ptr);
    test_ptr = node->indir;
    CHECKP(test_ptr);
    CHECK_INVARIANT(*(xptr*) XADDR(test_ptr) == node_ptr, ce_indirection);

    if (getNodeType(checkp(node_ptr)) == text) {
        CHECK_INVARIANT(!TextNode(node_ptr).isEmpty(), ce_snode);
    }

    CHECKP(node_ptr);
    test_ptr = node->ldsc;
    if (test_ptr != XNULL) {
        CHECKP(test_ptr);
        CHECK_INVARIANT(((node_base_t*) XADDR(test_ptr))->rdsc == node_ptr, ce_left_pointer);
        CHECK_INVARIANT(nid_cmp_effective(test_ptr, node_ptr) == -1, ce_nid);
    }

    CHECKP(node_ptr);
    test_ptr = node->rdsc;
    if (test_ptr != XNULL) {
        CHECKP(test_ptr);
        CHECK_INVARIANT(((node_base_t*) XADDR(test_ptr))->ldsc == node_ptr, ce_right_pointer);
        CHECK_INVARIANT(nid_cmp_effective(test_ptr, node_ptr) == 1, ce_nid);
    }

    CHECKP(node_ptr);
    test_ptr = nodeGetParent(node_ptr);
    if (test_ptr != XNULL) {
        xptr lptr = getPreviousDescriptorOfSameSort(node_ptr);
        xptr lparent = XNULL;
        if (lptr != XNULL) {
            lparent = nodeGetParent(checkp(lptr));
        }
        CHECK_INVARIANT(nid_cmp_effective(test_ptr, node_ptr) == -2, ce_nid);

        if (test_ptr != lparent) {
            int child_no = getSchemaNode(node_ptr)->getIndex();
            xptr * list;
            int child_num;
            CHECKP(test_ptr);
            child_num = getChildList(test_ptr, list);
            CHECK_INVARIANT(child_num > child_no && list[child_no] == node_ptr, ce_child_in_parent);
        }
    }

    return true;
}

#ifdef DEBUG_MO_NID

void logNID(const char * type, xptr l, xptr r, xptr n)
{
    std::stringstream a, b, c;

    nid_print(l, a);
    nid_print(r, b);
    nid_print(n, c);

    monidlog(("MOLOG NID %s : %s x %s => %s", type, a.str().c_str(), b.str().c_str(), c.str().c_str()));
}

#endif /* DEBUG_MO_NID */

bool checkBlockPointers(xptr block_ptr)
{
    node_blk_hdr * block = getBlockHeaderCP(block_ptr);
    xptr pblk = block->pblk;
    xptr nblk = block->nblk;

    CHECK_INVARIANT((pblk == XNULL && getSchemaNode(block_ptr)->bblk == block_ptr) || getBlockHeaderCP(pblk)->nblk == block_ptr, ce_block_chain);
    CHECK_INVARIANT(nblk == XNULL || getBlockHeaderCP(nblk)->pblk == block_ptr, ce_block_chain);

    return true;
}

#ifdef DEBUG_MO_CHECK

bool checkBlock(xptr block_ptr)
{
    node_base_t * lnode, * rnode;
    node_blk_hdr block;
    xptr * indir, * endpoint;

    copyBlockHeaderCP(&block, block_ptr);

    if (block.count + block.indir_count == 0) { return true; }

    lnode = NULL;
    rnode = getDsc(block_ptr, block.desc_first);

    while (rnode != NULL) {
        CHECKP(block_ptr);
        xptr rnode_xptr = addr2xptr(rnode);

        CHECK_INVARIANT(getDsc(block_ptr, rnode->desc_prev) == lnode, ce_inblock_pointer);

        if (lnode != NULL) {
            CHECK_INVARIANT(nid_cmp_effective(addr2xptr(lnode), rnode_xptr) == -1, ce_nid);
        }

        if (!checkNodeOuterPointers(rnode_xptr)) { return false; }

        CHECKP(block_ptr);
        lnode = rnode;
        rnode = getDsc(block_ptr, lnode->desc_next);
    }

    indir = (xptr *) getBlockPointer(block_ptr, getBlockHeaderCP(block_ptr)->free_first_indir);

    while (indir != NULL) {
        U_ASSERT((uint32_t) indir > 0x8888);
        U_ASSERT(((shft *) indir)[1] == 0);
        indir = (xptr *) getBlockPointer(block_ptr, * (shft *) indir);
    }

    if (!checkBlockPointers(block_ptr)) { return false; }

    return true;
}

#endif /* DEBUG_MO_CHECK */
