/*
 * File:  nodeinsert.cpp
 *
 * This module contatins low level node insertion routines, which are indifferent to node type
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/microoperations.h"
#include "tr/mo/microsurgery.h"
#include "tr/mo/indirection.h"
#include "tr/mo/blocks.h"
#include "tr/mo/nodemoutils.h"

inline static void createNID(xptr left_sibling, xptr right_sibling, xptr parent, xptr node)
{
    if (left_sibling != XNULL) {
        if (right_sibling != XNULL) {
            nid_create_between(left_sibling, right_sibling, node);
            logNID("LR", left_sibling, right_sibling, node);
        } else {
            nid_create_right(left_sibling, parent, node);
            logNID("LP", left_sibling, parent, node);
        }
    } else if (right_sibling != XNULL) {
        nid_create_left(right_sibling, parent, node);
        logNID("RP", right_sibling, parent, node);
    } else {
        nid_create_child(parent, node);
        logNID("P", parent, parent, node);
    }
}


inline static
void init_node(node_base_t* node, t_item ntype, xmlscm_type type) {
    switch (ntype) {
    case(element) :
        ((element_node *) node)->type = type;
        break;
    case(attribute) :
        ((attribute_node *) node)->type = type;
        break;
    case(text):
        ((text_node *) node)->flags = 0;
    default:
        break;
    }
}

template <typename Iterator> static
xptr scanForChild(xptr p, int child_pos) {
    xptr child;

    while (p != XNULL) {
        child = getChildAt(p, child_pos);
        if (child != XNULL) { return child; }
        p = Iterator::nextNode(p);
    }

    return XNULL;
}

static void findNodeBrother(const node_info_t* node_info, /*out*/ xptr &left_brother, /*out*/ xptr &right_brother)
{
    schema_node_cptr parent_snode = getBlockHeaderCP(node_info->parent)->snode;
    int child_pos = parent_snode->find_first_child(node_info->ns, node_info->name, node_info->node_type);
    xptr child;

    left_brother = XNULL;
    right_brother = XNULL;

    U_ASSERT(child_pos != -1);
    child = getChildAt(node_info->parent, child_pos);

    if (child != XNULL) {
/* Brother is in the given parent. Now we have to find it */
        xptr prev_child = XNULL;
        if (node_info->left_sibling != XNULL) {
            do {
                if (nid_cmp_effective(node_info->left_sibling, child) < 0) { break; }
                prev_child = child;
                child = getNextDescriptorOfSameSort(child);
            } while (child != XNULL && nodeGetParentIndirection(child) == node_info->parent_indir);

            if (prev_child == XNULL) {
                right_brother = child;
                molog(("MOLOG right brother is the first child : 0x%llx", child.to_logical_int()));
            } else {
                left_brother = prev_child;
                molog(("MOLOG left brother found easily : 0x%llx", child.to_logical_int()));
            }
        } else {
            U_ASSERT(node_info->right_sibling != XNULL);
            bool right_condition_flag = false;
            do {
                right_condition_flag = nid_cmp_effective(node_info->right_sibling, child) < 0;
                if (right_condition_flag) { break; }
                prev_child = child;
                child = getNextDescriptorOfSameSort(child);
            } while (child != XNULL && nodeGetParentIndirection(child) == node_info->parent_indir);

            if (right_condition_flag) {
                right_brother = child;
                molog(("MOLOG right brother found easily : 0x%llx", child.to_logical_int()));
            } else {
                left_brother = prev_child;
                molog(("MOLOG left brother is the last child : 0x%llx", child.to_logical_int()));
            }
        }
    } else {
        xptr child;

        child = scanForChild<NodeIteratorForeward>(node_info->parent, child_pos);
        if (child != XNULL) {
            right_brother = child;
            molog(("MOLOG right brother found hardly : 0x%llx", child.to_logical_int()));
            return ;
        }

        child = scanForChild<NodeIteratorBackward>(node_info->parent, child_pos);
        if (child != XNULL) {
            /* Child found to the left of the given parent node, so we need to find the rightmost node of this kind that belong to this parent */
            CHECKP(child);
            xptr left_parent = nodeGetParentIndirection(child);
            do {
                left_brother = child;
                child = getNextDescriptorOfSameSort(child);
            } while (nullsafe_CHECKP(child) && (nodeGetParentIndirection(child) == left_parent));
            molog(("MOLOG left brother found hardly : 0x%llx", left_brother.to_logical_int()));
        }
    }

    U_ASSERT((right_brother != XNULL) ^ (left_brother != XNULL));
}


inline xptr doInsertNodeCP(xptr block, shft left, node_info_t* node_info)
{
    node_base_t * new_node;

    WRITEP(block);

    node_info->snode = getBlockHeader(block)->snode;
    new_node = nodeListInsertCP(block, left);
    init_node(new_node, node_info->node_type, node_info->scm_type);

    new_node->pdsc = node_info->parent_indir;
    new_node->ldsc = node_info->left_sibling;
    new_node->rdsc = node_info->right_sibling;
    node_info->node_xptr = ADDR2XPTR(new_node);
    node_info->indirection = createIndirectionForNewNodeCP(node_info->node_xptr);

    nodeUpdateLinks(node_info->node_xptr, node_info->child_in_parent_xptr);
    if ((node_info->node_type == virtual_root) || (node_info->node_type == document)) {
        nid_create_root(node_info->node_xptr, IS_DATA_BLOCK(node_info->node_xptr));
    } else {
        createNID(node_info->left_sibling, node_info->right_sibling, node_info->parent, node_info->node_xptr);
    }

    updateBlockChains(node_info->indirection, node_info->node_xptr, up_insert);

    return node_info->node_xptr;
}

xptr insertNodeFirst(xptr block, node_info_t* node_info)
{
    molog(("MOLOG"));
    U_ASSERT(block != XNULL);
    return doInsertNodeCP(block, 0, node_info);
}

inline void fixSiblingPointers(node_info_t* node_info, const xptr &evil_block)
{
    if (node_info->left_sibling != XNULL && same_block(evil_block, node_info->left_sibling)) {
        node_info->left_sibling = indirectionDereferenceCP(node_info->left_sibling_indir);
    }
    if (node_info->right_sibling != XNULL && same_block(evil_block, node_info->right_sibling)) {
        node_info->right_sibling = indirectionDereferenceCP(node_info->right_sibling_indir);
    }
}

xptr insertNodeWithLeftBrother(xptr left_brother, node_info_t* node_info)
{
    xptr block;
    node_blk_hdr block_header;
    shft left_brother_shift = calcShift(XADDR(left_brother));

    molog(("MOLOG (0x%llx)", left_brother.to_logical_int()));

    U_ASSERT(left_brother != XNULL);

    block = block_xptr(left_brother);
    CHECKP(block);
    memcpy(&block_header, XADDR(block), sizeof(node_blk_hdr));
//    U_ASSERT(nodeGetParentIndirection(left_sibling) == node_info->parent_indir);

    /* The case, when block is full */
    if (block_header.free_first == 0) {
        /* If left brother is the last descriptor in the block, we may want to add new node to the next block */
        if (left_brother_shift == block_header.desc_last) {
            node_blk_hdr * next_block_header = block_header.nblk == XNULL ? NULL : getBlockHeaderCP(block_header.nblk);

            /* Check if the next block accepts new node */
            if (nullsafe_CHECKP(block_header.nblk) && (next_block_header->dsc_size >= block_header.dsc_size) && (next_block_header->free_first != 0)) {
                block = block_header.nblk;
            } else {
            /* If not, create new block next to current */
                block = createBlock(block_header.snode, block);
            }
            left_brother_shift = 0;
        } else {
        /* If left brother is not in the end of the block the block, we should split the block */
            xptr evil_block = left_brother;
            left_brother = splitBlock(left_brother);
            fixSiblingPointers(node_info, evil_block);
            block = block_xptr(left_brother);
            left_brother_shift = calcShift(XADDR(left_brother));
        }
    }

    doInsertNodeCP(block, left_brother_shift, node_info);

    return node_info->node_xptr;
}


xptr insertNodeWithRightBrother(xptr right_brother, node_info_t* node_info)
{
    xptr block;
    node_blk_hdr block_header;
    shft left_brother;
    xptr left_brother_xptr;

    molog(("MOLOG (0x%llx)", right_brother.to_logical_int()));

    U_ASSERT(right_brother != XNULL);

    block = block_xptr(right_brother);
    CHECKP(block);
    memcpy(&block_header, XADDR(block), sizeof(node_blk_hdr));
//    U_ASSERT(nodeGetParentIndirection(right_sibling) == node_info->parent_indir);

    /* The case, when block is full */
    if (block_header.free_first == 0) {
        /* If right brother is the first descriptor in the block, we may want to add new node to the previous block */
        if (calcShift(XADDR(right_brother)) == block_header.desc_first) {
            node_blk_hdr * prev_block_header = block_header.pblk == XNULL ? NULL : getBlockHeaderCP(block_header.pblk);

            /* Check if the next block accepts new node */
            if (nullsafe_CHECKP(block_header.pblk) && (prev_block_header->dsc_size >= block_header.dsc_size) && (prev_block_header->free_first != 0)) {
                block = block_header.pblk;
                left_brother = prev_block_header->desc_last;
            } else {
            /* If not, create new block next to current */
                block = createBlock(block_header.snode, block_header.pblk);
                left_brother = 0;
            }
        } else {
        /* If right brother is not in the beginning of the block the block, we should split the block */
            xptr evil_block = right_brother;
            right_brother = splitBlock(right_brother);
            fixSiblingPointers(node_info, evil_block);
            block = block_xptr(right_brother);
            CHECKP(block);
            left_brother = ((node_base_t*) XADDR(right_brother))->desc_prev;
        }
    } else {
        CHECKP(block);
        left_brother = ((node_base_t*) XADDR(right_brother))->desc_prev;
    }

    CHECKP(right_brother);
    if (nodeGetParentIndirection(right_brother) == node_info->parent_indir) {
        left_brother_xptr = getPreviousDescriptorOfSameSort(right_brother);
        if (!nullsafe_CHECKP(left_brother_xptr) || (nodeGetParentIndirection(left_brother_xptr) != node_info->parent_indir)) {
            node_info->child_in_parent_xptr = findNodeInParentCP(right_brother);
        }
    }

    doInsertNodeCP(block, left_brother, node_info);

    return node_info->node_xptr;
}

xptr insertNodeGeneral(node_info_t * node_info)
{
    schema_node_cptr parent_snode;
    schema_node_cptr snode;
    xptr node_xptr;
    xptr left_brother = XNULL;

    parent_snode = getBlockHeaderCP(node_info->parent)->snode;
    snode = parent_snode->get_first_child(node_info->ns, node_info->name, node_info->node_type);

    if (!snode.found()) {
        snode = parent_snode->add_child(node_info->ns, node_info->name, node_info->node_type);
    }

    if (snode->lastnode_ind != XNULL) {
    /* If the place to insert is hinted, trust it */
        U_ASSERT(snode->nodecnt != 0);
        left_brother = indirectionDereferenceCP(snode->lastnode_ind);
        node_xptr = insertNodeWithLeftBrother(left_brother, node_info);
    } else if (snode->nodecnt == 0) {
    /* If there are no nodes in this sequence */
        if (snode->bblk == XNULL) {
            xptr rbblk = block_xptr(indirectionGetRollbackRecord());
            if (rbblk != XNULL && undeleteBlock(rbblk)) {
                snode->bblk = createBlock(snode, XNULL, rbblk);
            } else {
                snode->bblk = createBlock(snode);
            }
        }
        node_xptr = insertNodeFirst(snode->bblk, node_info);
    } else {
    /* Common case : find brothers if nothing is known */
        xptr right_brother;
        findNodeBrother(node_info, left_brother, right_brother);

        if (left_brother != XNULL) {
            node_xptr = insertNodeWithLeftBrother(left_brother, node_info);
        } else {
            U_ASSERT(right_brother != XNULL);
            node_xptr = insertNodeWithRightBrother(right_brother, node_info);
        }
        left_brother = getPreviousDescriptorOfSameSort(node_xptr);
    }

    /* Update child pointer in parent */
    CHECKP(node_info->parent);
    int pos = parent_snode->find_first_child(node_info->ns, node_info->name, node_info->node_type);
    if (pos >= getChildCount(node_info->parent)) {
        widenDescriptor(node_info->parent, pos, node_xptr);
        node_info->parent = indirectionDereferenceCP(node_info->parent_indir);
    } else if (!nullsafe_CHECKP(left_brother) || nodeGetParentIndirection(left_brother) != node_info->parent_indir) {
        setNodeChild(node_info->parent, pos, node_xptr);
    }

    return node_info->node_xptr;
}


