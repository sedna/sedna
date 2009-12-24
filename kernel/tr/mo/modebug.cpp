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
#include "tr/crmutils/node_utils.h"
#include "tr/pstr/pstr.h"

#include <streambuf>
#include <sstream>

enum consistency_error_t consistency_error = ce_none;

#define CHECK_INVARIANT(predicate, error) if (!(predicate)) { consistency_error = error; U_ASSERT(false); return false; }

inline bool checkNodeOuterPointers(xptr node_ptr) {
    n_dsc * node = (n_dsc *) XADDR(node_ptr);
    xptr test_ptr;

    CHECKP(node_ptr);
    test_ptr = node->indir;
    CHECKP(test_ptr);
    CHECK_INVARIANT(*(xptr*) XADDR(test_ptr) == node_ptr, ce_indirection);

    if (getNodeTypeCP(node_ptr) == text) {
        t_dsc * t = (t_dsc *) XADDR(node_ptr);
        CHECK_INVARIANT(t->size <= PSTRMAXSIZE || t->data == block_xptr(t->data), ce_snode);
    }

    CHECKP(node_ptr);
    test_ptr = node->ldsc;
    if (test_ptr != XNULL) {
        CHECKP(test_ptr);
        CHECK_INVARIANT(((n_dsc*) XADDR(test_ptr))->rdsc == node_ptr, ce_left_pointer);
        CHECK_INVARIANT(nid_cmp_effective(test_ptr, node_ptr) == -1, ce_nid);
    }

    CHECKP(node_ptr);
    test_ptr = node->rdsc;
    if (test_ptr != XNULL) {
        CHECKP(test_ptr);
        CHECK_INVARIANT(((n_dsc*) XADDR(test_ptr))->ldsc == node_ptr, ce_right_pointer);
        CHECK_INVARIANT(nid_cmp_effective(test_ptr, node_ptr) == 1, ce_nid);
    }

    CHECKP(node_ptr);
    test_ptr = getParent(node_ptr);
    if (test_ptr != XNULL) {
        xptr lptr = getPreviousDescriptorOfSameSortXptr(node_ptr);
        xptr lparent = XNULL;
        if (lptr != XNULL) {
            lparent = getParentCP(lptr);
        }
        CHECK_INVARIANT(nid_cmp_effective(test_ptr, node_ptr) == -2, ce_nid);

        if (test_ptr != lparent) {
            int child_no = getBlockHeaderCP(node_ptr)->snode->get_node_position_in_parent();
            xptr * list;
            int child_num;
            CHECKP(test_ptr);
            getChildList(test_ptr, list, child_num);
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

    CHECK_INVARIANT((pblk == XNULL && block->snode->bblk == block_ptr) || getBlockHeaderCP(pblk)->nblk == block_ptr, ce_block_chain);
    CHECK_INVARIANT(nblk == XNULL || getBlockHeaderCP(nblk)->pblk == block_ptr, ce_block_chain);

    return true;
}

#ifdef DEBUG_MO

bool checkBlock(xptr block_ptr)
{
    n_dsc * lnode, * rnode;
    node_blk_hdr block;

    copyBlockHeaderCP(&block, block_ptr);

    if (block.count + block.indir_count == 0) { return true; }

    lnode = NULL;
    rnode = getDescriptor(block_ptr, block.desc_first);

    while (rnode != NULL) {
        CHECKP(block_ptr);
        xptr rnode_xptr = addr2xptr(rnode);

        CHECK_INVARIANT(getDescriptor(block_ptr, rnode->desc_prev) == lnode, ce_inblock_pointer);

        if (lnode != NULL) {
            CHECK_INVARIANT(nid_cmp_effective(addr2xptr(lnode), rnode_xptr) == -1, ce_nid);
        }

        if (!checkNodeOuterPointers(rnode_xptr)) { return false; }

        CHECKP(block_ptr);
        lnode = rnode;
        rnode = getDescriptor(block_ptr, lnode->desc_next);
    }

    if (!checkBlockPointers(block_ptr)) { return false; }

    return true;
}

#endif /* DEBUG_MO */
