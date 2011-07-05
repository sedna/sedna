/*
 * File:  nodeoperations.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef NODEOPERATORS_H_
#define NODEOPERATORS_H_

#include "tr/mo/indirection.h"
#include "tr/structures/descriptor.h"
#include "tr/structures/nodeblocks.h"
#include "tr/structures/schema.h"
#include "tr/structures/nodetypes.h"

#include "tr/pstr/pstr.h"
#include "tr/pstr/pstr_long.h"

/* The following functions work until node base is in the
 * beginning of every node structure. If someone changes
 * the node structure, these methods must be changed.
 */

/* Casting options */

inline
internal::node_base_t * getBaseFromAnyNode(const xptr node) { return (internal::node_base_t *) XADDR(node); }

inline
internal::node_text_t * getTextFromAnyNode(const xptr node) { return (internal::node_text_t *) (((char *) XADDR(node)) + sizeof(internal::node_base_t)); };

inline
internal::namespace_node * getNamespaceNode(const xptr node) { return (internal::namespace_node *) XADDR(node); }

inline
xmlns_ptr_pers getNamespaceFromNSNode(const xptr node) { return getNamespaceNode(node)->ns; }

inline
internal::pi_node * getPINode(const xptr node) { return (internal::pi_node *) XADDR(node); }

/* Node basic fields accessors */

inline
xptr nodeGetLeftSibling(const xptr node) { return getBaseFromAnyNode(node)->ldsc; }

inline
xptr nodeGetRightSibling(const xptr node) { return getBaseFromAnyNode(node)->rdsc; }

inline
xptr nodeGetNext(const xptr node) {
    shft offs = getBaseFromAnyNode(node)->desc_next;
    return offs == 0 ? XNULL : block_offset(node, offs);
}

inline
xptr nodeGetPrev(const xptr node) {
    shft offs = getBaseFromAnyNode(node)->desc_prev;
    return offs == 0 ? XNULL : block_offset(node, offs);
}

inline
xptr nodeGetParentIndirection(const xptr node) { return getBaseFromAnyNode(node)->pdsc; }

inline
xptr nodeGetParent(const xptr node) { return indirectionDereferenceCP(getBaseFromAnyNode(node)->pdsc); }

inline
xptr nodeGetIndirection(const xptr node) { return getBaseFromAnyNode(node)->indir; }

inline
xptr getIndirectionSafeCP(xptr node)
{
    if (node == XNULL) return XNULL;
    CHECKP(node);
    return nodeGetIndirection(node);
}

inline
xptr nodeiGetRightSiblingIndirection(const xptr nodei) {
    const xptr node = indirectionDereferenceCP(nodei);
    CHECKP(node);
    return getIndirectionSafeCP(nodeGetRightSibling(node));
}

/* Very careful with this one! */
inline
xptr nodeGetNIDPtr(const xptr node) {
    return node + offsetof(internal::node_base_t, nid);
}

/* Very careful with this one! */
inline
t_nid * nodeGetNIDP(const xptr node) {
    return (t_nid*) ((char *) XADDR(node) + offsetof(internal::node_base_t, nid));
}

inline
xptr nodeGetNIDPtrCP(const xptr node) {
    CHECKP(node)
    return node + offsetof(internal::node_base_t, nid);
}

/* Text basic accessors */

// Returns dereferenced text pointer if available, or PSTRLONG pointer
inline
xptr nodeGetTextPointer(const internal::node_text_t * text) {
    const uint16_t size = text->size;
    xptr result;
    if (size == internal::textInPstrLong) {
        memcpy(&result, text->data, sizeof(xptr));
        return result;
    } else if (size > internal::maxDescriptorTextSize) {
        memcpy(&result, text->data, sizeof(xptr));
        CHECKP(result);
        return pstrderef(result);
    } else {
        return addr2xptr(&(text->data));
    }
}

inline
strsize_t nodeGetTextSize(const internal::node_text_t * text) {
    const uint16_t size = text->size;
    if (size == internal::textInPstrLong) {
        xptr ptr;
        memcpy(&ptr, text->data, sizeof(xptr));
        return pstr_long_bytelength2(ptr);
    } else {
        return size;
    }
}

inline
bool isPstrLong(const internal::node_text_t * text) {
    return (text->size == internal::textInPstrLong);
}

/* Node utility functions */

int getChildList(const xptr node_xptr, xptr * &child_list);
xptr getChildAt(const xptr node_xptr, int child_index);

inline
int getChildCount(const xptr node_xptr) { return internal::getNodeChildCount(internal::getBlockHeader(node_xptr)); }

inline
schema_node_cptr getSchemaNode(const xptr node) {
    CHECKP(node);
    return internal::getBlockHeader(node)->snode;
}

inline
schema_node_xptr getSchemaPointer(const xptr node) {
    return internal::getBlockHeader(node)->snode;
}

inline
t_item getNodeType(xptr node) {
    return internal::getBlockHeader(node)->node_type;
}

/* returns the xptr to the first child of the node identified by name and type*/
xptr getNodeChild(xptr node, schema_node_xptr scn);
xptr getNodeChildByType(xptr node, t_item type);
xptr getNodeAttribute(xptr node, const char* name, xmlns_ptr ns);

/* Node iterators */

enum {
    child_count_unknown = -1
};

xptr getRootNode(const xptr node);

xmlscm_type getScmType(xptr node);

/* Legacy */

inline
bool is_element(const xptr node) {
    CHECKP(node);
    return getNodeType(node) == element;
}

inline
bool is_attribute(const xptr node) {
    CHECKP(node);
    return getNodeType(node) == attribute;
}

#endif /* NODEOPERATORS_H_ */
