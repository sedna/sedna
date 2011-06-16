/*
 * File:  nodemoutils.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef NODEMOUTILS_H_
#define NODEMOUTILS_H_

#include "tr/structures/nodeblocks.h"
#include "tr/structures/nodeoperations.h"
#include "tr/structures/nodeutils.h"

using namespace internal;

/* If any of these is needed elsewhere besides microoperations,
 * move one to tr/structures/nodeoperations.h */

void initNodeBlock(xptr p, shft dsc_size, schema_node_cptr schema_node);

void clearNodeBlock(xptr p);

inline
shft calcShift(void * p)
{
    return ((shft)((char*)(p) - (char *) ((ptrdiff_t)(p) & PAGE_BIT_MASK)));
}

inline
void setNodeChild(const xptr node_xptr, int child_index, xptr child)
{
    int n;
    xptr * child_list;

    CHECKP(node_xptr);
    n = getChildList(node_xptr, child_list);
    U_ASSERT(n > child_index);
    VMM_SIGNAL_MODIFICATION(node_xptr);
    child_list[child_index] = child;
}

inline static
void * getBlockPtr(char * base, shft offset) {
    return (void *) (base + offset);
}

inline static
node_base_t * getDsc(char * base, shft offset) {
    return (node_base_t *) (base + offset);
}

inline static
node_base_t * getPrevDsc(char * base, node_base_t * node) {
    return (node_base_t *) (base + node->desc_prev);
}

inline static
node_base_t * getNextDsc(char * base, node_base_t * node) {
    return (node_base_t *) (base + node->desc_next);
}


inline static
xptr getAnyDmChildrenChild(const xptr source)
{
    return getAnyChildByTypeMask(source, ti_dmchildren);
}

inline static
xptr getAnyAttributeChild(const xptr source)
{
    return getAnyChildByTypeMask(source, attribute);
}

inline static
xptr getLastNonDmChildrenChild(const xptr source)
{
    return getLastChildByTypeMask(source, ti_first_children);
}

inline static
xptr getLastNamespaceChild(const xptr source)
{
    return getLastChildByType(source, xml_namespace);
}


#endif /* NODEMOUTILS_H_ */
