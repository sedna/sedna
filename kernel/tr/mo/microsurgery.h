/*
 * File:  microsurgery.h
 *
 * This file provides low level node and block operations for Sedna internal data representation, which includes:
 *  - some operations for updating block header
 *  - operations for maintaining node list (the ONLY right way to add or delete nodes)
 *  - node buffer implementation
 *  - some miscellaneous low level routines needed for microoperations
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _MICROSURGERY_H
#define _MICROSURGERY_H

#include "common/sedna.h"
#include "common/xptr.h"

#include "tr/structures/schema.h"
#include "tr/mo/indirection.h"
#include "tr/mo/blocks.h"

using namespace internal;

/* Methods, that does not change current pointer are suffixed with SP */
/* Methods, that call CHECKP ONCE (!) with their only xptr parameter are suffixed with CP */

#define ASSERTP(p) U_ASSERT(TEST_XPTR(p))

struct node_buffer;


void fixPointersOnDelete(xptr block_xptr, node_base_t * node);

/** Delete node from block node list. */
void nodeListDeleteCP(xptr block_xptr, node_base_t * node);

/*
inline void nodeListDeleteCP(xptr node_xptr)
{
    nodeListDeleteCP(block_xptr(node_xptr), (node_base_t *) XADDR(node_xptr));
}
*/

inline void nodeDeleteCP(xptr node_ptr)
{
    CHECKP(node_ptr);

    xptr block_ptr = block_xptr(node_ptr);
    node_base_t * node = (node_base_t *) XADDR(node_ptr);
    xptr indir = node->indir;

    fixPointersOnDelete(block_ptr, node);
    indirectionTableDeleteRecord(indir);
    nodeListDeleteCP(block_ptr, node);
}



/** Used as preudo-shift to the last node in \def nodeListInsertCP function. */
#define LAST_NODE  0xffff

/** Insert node to block list.
  * New node is inserted to block \param block_xptr after the node, located at \param left_node shift
  * \param left_node can be 0 or \def LAST_NODE. If 0, the first node is inserted, if \def LAST_NODE, the last node is inserted.
  * \param src is the node buffer with the node to insert. If NULL, the node is filled with zeroes.
  * \note If new descripor is larger, then source descriptor, the rest is filled with zeros
  * \warning Function does not check for the opposite situation. New block descriptor size MUST be greater or equal to the source desriptor size.
  * \param node_buffer_pos Tells the position of source node in the \param src buffer.
  */
node_base_t * nodeListInsertCP(xptr block_xptr, shft left_node, node_buffer * src = NULL, int node_buffer_pos = 0);

/** Updates left sibling, right sibling and indirection links.
  * Also update the child link to then node in node's parent, if given.
  * \warning No check for correctness is made.
  */
void nodeUpdateLinks(xptr node_xptr, xptr child_in_parent_xptr);

/** Searches for a link to the current node among the children of the parent node.
  * \param hint_child If this parameter is not -1, checks only the hint_child-th child pointer of te parent
  */
xptr findNodeInParentCP(const xptr& node_xptr, int hint_child = -1);

#ifdef _MSC_VER
#pragma warning( disable : 4200 )
#endif /* _MSC_VER */

struct node_buffer
{
    shft size;       /// Size of one node
    shft count;      /// Number of nodes in buffer
    char content[0]; /// The content of the buffer
};

#ifdef _MSC_VER
#pragma warning( default : 4200 )
#endif /* _MSC_VER */

char * nodeBufferGetNode(node_buffer * b, int pos);
node_buffer * nodeBufferAlloc(shft size, shft count);
void nodeBufferFree(node_buffer * b);

/* Create node buffer and copy node to that buffer */
node_buffer * nodeBufferCopyCP(xptr source);

/* Add node to that created buffer */
node_buffer * nodeBufferAddCP(node_buffer * b, xptr source);


inline bool nullsafe_CHECKP(xptr p)
{
    if (p != XNULL) {
        CHECKP(p);
        return true;
    };

    return false;
}

/* Aliases */

inline int getPageDescriptorCapacitySP(xptr block_xptr) { return internal::getBlockCapacity(block_xptr); }
inline int getPageDescriptorCapacitySP(node_blk_hdr * block) { return internal::getBlockCapacity(block); }

//#define GET_EFFECTIVE_PAGE_SIZE(p)

inline char * nodeBufferGetNode(node_buffer * b, int pos)
{
    U_ASSERT(b->count > pos);
    return b->content + b->size * pos;
}

inline xptr createIndirectionForNewNodeCP(xptr node)
{
    xptr node_indirection;
    node_indirection = indirectionTableAddRecord(node);
    WRITEP(node);
    ((node_base_t *) XADDR(node))->indir = node_indirection;
    return node_indirection;
}


inline node_buffer * nodeBufferAlloc(shft size, shft count)
{
    node_buffer * nb = (node_buffer *) malloc(sizeof(node_buffer) + size + size*count);
    nb->size = size;
    nb->count = 0;
    return nb;
}

inline void nodeBufferFree(node_buffer * b)
{
    free(b);
}

inline node_buffer * nodeBufferAddCP(node_buffer * b, xptr source)
{
    CHECKP(source);

    U_ASSERT(getBlockHeader(source)->dsc_size == b->size);
    memcpy(b->content + b->count * b->size, XADDR(source), b->size);
    b->count++;

    return b;
}

inline node_buffer * nodeBufferCopyCP(xptr source)
{
    node_buffer * result;
    CHECKP(source);

    shft size = getBlockHeader(source)->dsc_size;
    result = nodeBufferAlloc(size, 1);
    result->count++;
    memcpy(result->content, XADDR(source), size);

    return result;
}

xptr findNodeInParentCP(const xptr &node_xptr, int hint_child);

#endif /* _MICROSURGERY_H */
