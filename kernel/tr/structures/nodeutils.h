/*
 * File:  nodeutils.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef NODEUTILS_H_
#define NODEUTILS_H_

#include <set>

#include "tr/structures/nodeoperations.h"
#include "tr/executor/base/xsd.h"

xptr getFirstChild(const xptr node);
xptr getFirstChildByType(const xptr node, t_item t);
xptr getFirstChildByTypeMask(const xptr node, typemask_t tmask);
xptr getFirstChildByTypeScan(const xptr node, t_item t);
xptr getFirstChildByTypeMaskScan(const xptr node, typemask_t tmask);
xptr getFirstChildBySchema(const xptr node, schema_node_cptr scn);

xptr getLastChild(const xptr node);
xptr getLastChildByType(const xptr node, t_item t);
xptr getLastChildByTypeMask(const xptr node, typemask_t tmask);
xptr getLastChildBySchema(const xptr node, schema_node_cptr scn);

xptr getAnyChild(xptr node);
xptr getAnyChildByTypeMask(xptr node, typemask_t tmask);
xptr getAnyChildBySchema(const xptr node, schema_node_cptr scn);

xptr getRightSiblingByType(const xptr node, t_item t);
xptr getRightSiblingByTypeMask(const xptr node, typemask_t tmask);
xptr getRightSiblingBySchema(const xptr node, schema_node_cptr scn);
xptr getLeftSiblingBySchema(const xptr node, schema_node_cptr scn);

xptr getMostLeftSiblingByType(const xptr node, t_item t);
xptr getLeftSiblingByType(const xptr node, t_item t);

xptr __getNextBlockDescriptorOfSameSort(xptr nodex);

static inline
xptr getNextDescriptorOfSameSort(xptr nodex)
{
    CHECKP(nodex);

    internal::node_base_t * nodebase = getBaseFromAnyNode(nodex);

    if (nodebase->desc_next != 0) {
        return block_offset(nodex, nodebase->desc_next);
    } else {
        return __getNextBlockDescriptorOfSameSort(nodex);
    }
}

xptr __getPreviousBlockDescriptorOfSameSort(xptr nodex);

static inline
xptr getPreviousDescriptorOfSameSort(xptr nodex)
{
    CHECKP(nodex);

    internal::node_base_t * nodebase = getBaseFromAnyNode(nodex);

    if (nodebase->desc_prev != 0) {
        return block_offset(nodex, nodebase->desc_prev);
    } else {
        return __getPreviousBlockDescriptorOfSameSort(nodex);
    }
}

xptr getRightSiblingOfSameSort(xptr nodex);
xptr getLeftSiblingOfSameSort(xptr nodex);

xptr findAttribute(xptr node, const char* name, const char* uri);

/* Aliases */

inline static
xptr getNextByType(const xptr node, t_item t) { return getRightSiblingByType(node, t); };
inline static
xptr getNextByTypeMask(const xptr node, typemask_t tmask) { return getRightSiblingByTypeMask(node, tmask); };
inline static
xptr getNextBySchema(const xptr node, schema_node_cptr scn) { return getRightSiblingBySchema(node, scn); };
inline static
xptr getNextAttribute(const xptr node) { return getRightSiblingByType(node, attribute); };
inline static
xptr getNextElement(const xptr node) { return getRightSiblingByType(node, element); };
inline static
xptr getNextNode(const xptr node) { return nodeGetRightSibling(checkp(node)); };

inline static
xptr getFirstAttributeChild(const xptr node) { return getFirstChildByType(node, attribute); };
inline static
xptr getFirstElementChild(const xptr node) { return getFirstChildByType(node, element); };
inline static
xptr getFirstChildNode(const xptr node) { return getFirstChildByTypeMask(node, ti_content); };

inline static
xptr getNextSiblingOfSameSort(xptr nodex) { return getRightSiblingOfSameSort(nodex); }
inline static
xptr getPrevSiblingOfSameSort(xptr nodex) { return getLeftSiblingOfSameSort(nodex); }



inline static
xptr findAttribute(xptr node, const char* name, xmlns_ptr ns) {
    return findAttribute(node, name, (ns == NULL) ? NULL : ns->uri);
}

inline static
bool sameQName(xsd::QName qname, xptr p) {
    schema_node_cptr scn = getSchemaNode(p);
    return same_xmlns_uri(scn->get_xmlns(), qname.getXmlNs()) && (strcmpex(scn->get_name(), qname.getLocalName()) == 0);
}

//xptr getFirstDescandantBySchema(xptr ancestor,schema_node_cptr scn);

/** Returns the first  child  in dm:children accessor + attributes*/
xptr  getFirstNNSChildNode(const xptr source);

/** Returns the first non-attribute child by document order*/
static inline
xptr getFirstNonAttributeChild(const xptr source) {
    return getFirstChildByTypeMask(source, ti_dmchildren);
}

/** Returns the next non-attribute sibling by document order*/
static inline
xptr getNextNonAttribute(const xptr source) {
    return getNextByTypeMask(source, ti_dmchildren);
}

/* Alias */

/*returns the next non-descendant node in document*/
xptr getNextNDNode(const xptr node);

/*returns the next node in document*/
xptr getNextDONode(const xptr node);

/* Returns the parent node if it is not a virtual root, otherwise returns XNULL */
inline static
xptr getActualParentNode(const xptr node) {
    CHECKP(node);
    const xptr parent = nodeGetParent(node);
    return (parent == XNULL || getNodeType(parent) == virtual_root) ? XNULL : parent;
}

/*returns the previous node in document*/
xptr getPreviousDONode(xptr node);

/*returns the next non-descendant node in document that fits input schema_node */
xptr getNextNDNode(xptr node, schema_node_cptr scn);

/*returns the previous non-ancestor node in document that fits input schema_node */
xptr getPreviousNANode(xptr node, schema_node_cptr scn);

xptr getFirstAttributeDescendantAndFillPath(std::vector<xptr> &descstack);

/* returns the inderection pointer to the ancestor of the descriptor that corresponds
to the selected ancestor by scheme */
xptr getAncestorIndirectionByScheme (xptr node, const schema_node_cptr scm_node, const schema_node_cptr scm_anc);

inline static
xptr getNodeAncestorIndirectionByScheme (xptr node, const schema_node_cptr scm_anc)
{
    CHECKP(node);
    return getAncestorIndirectionByScheme(node, getSchemaNode(node), scm_anc);
}

xptr getAncestorBySchemeCP(xptr node, schema_node_xptr scm_anc);

/* returns the first descandant or self of the current node that corresponds to the stated schema_node */
xptr getFirstDescandantBySchema(xptr ancestor,schema_node_cptr scm);

xptr getLeftmostDescriptorWithPstrInThisBlock(xptr blk, xptr node);
xptr getRightmostDescriptorWithPstrInThisBlock(xptr blk, xptr node);

xptr getNextNonemptyBlock(xptr block);
xptr getPrevNonemptyBlock(xptr block);

struct NodeIteratorForeward {
    static inline xptr nextNode(xptr t) { return getNextDescriptorOfSameSort(checkp(t)); }
    static inline xptr nextNodeBlock(xptr t) { return getNextNonemptyBlock(t); }
};

struct NodeIteratorBackward {
    static inline xptr nextNode(xptr t) { return getPreviousDescriptorOfSameSort(checkp(t)); }
    static inline xptr nextNodeBlock(xptr t) { return getPrevNonemptyBlock(t); }
};

#endif /* NODEUTILS_H_ */
