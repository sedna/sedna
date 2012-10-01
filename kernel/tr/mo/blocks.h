/*
 * File:  blocks.h
 *
 * This file provides interfaces for block management operations (create, destroy, etc.)
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BLOCKS_H
#define _BLOCKS_H

#include <stddef.h>
#include <stdint.h>

#include "common/sedna.h"
#include "common/xptr/xptr.h"

#include "tr/vmm/vmm.h"
#include "tr/tr_base.h"
#include "tr/structures/schema.h"
#include "tr/strings/strings_base.h"

/** Transaction finalization routine, that actually deletes all virtually deleted blocks.
 */
void deleteDeadBlocks();

/** Delete block from block deletion chain.
 * \warning undeleteBlock Does neither re-markup the block, nor insert it to the block chain.
 */
bool undeleteBlock(xptr block_xptr);

/** Delete block.
 * Function does not delete block physically, but just adds it to special list.
 * Blocks are deleted from this list on the end of transaction.
 * This is made in order to have the original blocks for indirection in case of transaction rollback.
 */
void deleteBlockVirtually(xptr block_xptr);

/** Create and mark up block for the node chain.
 *  Either \param schema_node or \param pre_block_xptr must be set.
 *  Anyway block is created to the right of \param prev_block_xptr. If it is XNULL, the block is inserted to the first place.
 *  \param schema_node If not set (XNULL), schema node is taken from \param prev_block_xptr (->snode)
 *  \param undo_hint Tells function not to create a new block, but to mark up the given one.
 *  \param child_count_hint The child cells to create in case of element or document node. If 0,
 *   maximum number of child cells are created (i.e. number of children by schema).
 */
xptr createBlock(schema_node_cptr schema_node, xptr prev_block_xptr = XNULL, const xptr& undo_hint = XNULL, int child_count_hint = 0);

enum update_hint_t {
    up_delete, up_insert, up_move
};

void updateBlockChains(xptr node_src, xptr node_sink, enum update_hint_t update_hint);

#endif /* _BLOCKS_H */
