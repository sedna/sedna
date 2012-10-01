/*
 * File:  modebug.h
 *
 * Several consistency check functions
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _MODEBUG_H
#define _MODEBUG_H

#include <stddef.h>
#include <stdint.h>

#include "common/sedna.h"
#include "common/xptr/xptr.h"

//#define DEBUG_MO_CHECK
//#define DEBUG_MO_LOG
//#define DEBUG_MO_NID

extern enum consistency_error_t {
    ce_none,
    ce_inblock_pointer,
    ce_left_pointer,
    ce_right_pointer,
    ce_indirection,
    ce_parent_pointer,
    ce_child_in_parent,
    ce_logical_consistency,
    ce_snode,
    ce_nid,
    ce_block_chain,
    ce_indirection_chain
} consistency_error;

#ifdef DEBUG_MO_NID

#define monidlog(message) elog(EL_DBG, message)
void logNID(const char * type, xptr l, xptr r, xptr n);

#else  /* DEBUG_MO_NID */

#define monidlog(message)
inline void logNID(const char * type, xptr l, xptr r, xptr n) {};

#endif /* DEBUG_MO_NID */


#ifdef DEBUG_MO_CHECK

#define MOCHECK(test) U_ASSERT(test)
bool checkBlock(xptr block_ptr);

#else

#define MOCHECK(test)
inline bool checkBlock(xptr block_ptr) { return true; };

#endif /* DEBUG_MO_CHECK */

#ifdef DEBUG_MO_LOG

#define molog(message) elog(EL_DBG, message)

#else

#define molog(message)

#endif /* DEBUG_MO_LOG */

#endif /* _MODEBUG_H */
