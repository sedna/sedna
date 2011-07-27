/*
 * File:  microoperations.h
 *
 * This file provides interfaces for a low level internal data operations:
 *  - block management operations (create, destroy)
 *  - block splitting and descriptor size increasing
 *  - node shifting between blocks
 *  - insertion of text values
 *  - node insertions
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _MICROOPERATIONS_H
#define _MICROOPERATIONS_H

#include <stddef.h>
#include <stdint.h>

#include "common/sedna.h"
#include "common/xptr.h"

#include "tr/tr_base.h"
#include "tr/structures/schema.h"
#include "tr/vmm/vmm.h"
#include "tr/strings/strings_base.h"

#include "tr/mo/modebug.h"

#include "tr/mo/nodemoutils.h"

struct node_info_t
{
    xptr left_sibling;
    xptr right_sibling;
    xptr parent;

    t_item node_type;
    xmlscm_type scm_type;
    xmlns_ptr ns;
    char* name;

    xptr left_sibling_indir;
    xptr right_sibling_indir;
    xptr parent_indir;

    xptr child_in_parent_xptr;

    xptr node_xptr;
    xptr indirection;

    schema_node_cptr snode;

    strsize_t text_size;
    xptr text_data;
    size_t pi_target_size;

    int cdataflag;
};

xptr splitBlock(xptr node_xptr);

xptr widenDescriptor(xptr node, int pos, xptr set_value);

void widenBlockDescriptor(xptr block_ptr, shft new_dsc_size, int irecord_count);

xptr shiftOneNodeToNextBlock(xptr source_block_xptr);
void shiftManyNodesToNextBlock(xptr source_block_xptr, int node_count);

xptr shiftOneNodeToPreviousBlock(xptr source_block_xptr);
void shiftManyNodesToPreviousBlock(xptr source_block_xptr, int node_count);

/* Hinted node insertion functions */
xptr insertNodeFirst(xptr block, node_info_t * node_info);
xptr insertNodeWithLeftBrother(xptr left_sibling, node_info_t * node_info);
xptr insertNodeWithRightBrother(xptr right_sibling, node_info_t * node_info);

/** Unhinted node insertion function.
  The main differance between this function and three previous ones is that previous
  functions have hints where to insert node, thus they are much faster than the following one.
*/
xptr insertNodeGeneral(node_info_t * node_info);

enum insert_position_t
{
    ip_head, ip_tail
};

void insertTextValue(xptr node, const text_source_t source);
void insertTextValue(enum insert_position_t position, xptr node, const text_source_t source);

void deleteTextValue(xptr node_xptr);
void deleteTextValue(enum insert_position_t position, xptr node, strsize_t size);

void moSetUserException(int a_mo_exception);

/*****************************************
           Inline implementations
 *****************************************/

extern int mo_exception;

inline void microoperation_begin(xptr block) {
    mo_exception = 0;
};

inline void microoperation_end(xptr block) {
    if (mo_exception != 0) {
        throw USER_EXCEPTION(mo_exception);
    }
};


#endif /* _MICROOPERATIONS_H */
