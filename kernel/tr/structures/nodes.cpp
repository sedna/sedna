/*
 * File:  nodes.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "common/xptr.h"
#include "tr/structures/xmlns.h"
#include "tr/structures/schema.h"

#include "tr/strings/e_string.h"
#include "tr/pstr/pstr.h"

#include "tr/structures/nodeoperations.h"
#include "tr/structures/nodeinterface.h"
#include "tr/structures/nodeutils.h"
#include "tr/structures/textnodes.h"

#include <vector>

using namespace internal;
using namespace std;

static
xptr tmp_childlist[internal::maxElementChildCount];

#define CONSISTENCY_PROBLEM "Consistency problem found while traversing"

xptr getNodeAttribute(xptr node, const char* name, xmlns_ptr ns)
{
    return getChildAt(node, getSchemaNode(node)->find_first_child(ns, name, attribute));
}

xptr getChildAt(const xptr node_xptr, int child_index)
{
    int n;
    xptr * child_list;

    if (child_index < 0) { return XNULL; };

    CHECKP(node_xptr);
    n = getChildList(node_xptr, child_list);
    if (n > child_index) {
        return child_list[child_index];
    } else {
        return XNULL;
    }
}

int getChildList(const xptr node_xptr, xptr * &child_list)
{
    CHECKP(node_xptr);

    node_blk_hdr * block = getBlockHeader(node_xptr);
    int node_size = size_of_node(block->node_type);

    child_list = (xptr*) ((char *) XADDR(node_xptr) + node_size);
    return (int) (block->dsc_size - node_size) / sizeof(xptr);
}

int getChildListPersistent(const xptr node_xptr, xptr * child_list)
{
    CHECKP(node_xptr);

    node_blk_hdr * block = getBlockHeader(node_xptr);
    int node_size = size_of_node(block->node_type);

    int child_count = (int) (block->dsc_size - node_size) / sizeof(xptr);
    memcpy(child_list, ((char *) XADDR(node_xptr) + node_size), sizeof(xptr) * child_count);

    return child_count;
}

xmlscm_type getScmType(xptr node)
{
    CHECKP(node);
    t_item t = getNodeType(node);

    if (t == element) {
        return ElementNode(node).getType();
    } else if (t == attribute) {
        return AttributeNode(node).getType();
    } else {
        return 0;
    }
}

char * CommonTextNode::copyToBuffer(char * buffer, strsize_t position, size_t size) const
{
    if (position != 0) {
        throw USER_EXCEPTION2(SE1002, "Text random access");
    }

    const xptr textptr = this->getTextPointerCP();

    switch(this->getStorageType()) {
        case internal::innode :
            U_ASSERT(position <= internal::maxDescriptorTextSize);
        case internal::pstr :
            U_ASSERT(position < internal::textInPstrLong);
            CHECKP(textptr);
            memcpy(buffer, (char*) xaddr(textptr) + (shft) position, size);
            break;
        case internal::pstrlong :
            pstr_long_copy_to_buffer2(buffer, textptr, size);
            break;
        default :
            U_ASSERT(false);
    }

    return buffer;
}


int xmlscm_type_size(xmlscm_type xtype)
{
    switch(xtype)
    {
        case xs_float  : return sizeof(float);
        case xs_double : return sizeof(double);
        case xs_decimal: return sizeof(xs_decimal_t);
        case xs_integer: return sizeof(int64_t);
        case xs_boolean: return sizeof(bool);
        default        :
            if (!is_fixed_size_type(xtype))
                return 0;
            else if (is_temporal_type(xtype))
            {
                if (xtype == xs_duration || xtype == xs_dayTimeDuration || xtype == xs_yearMonthDuration)
                    return sizeof(xs_packed_duration);
                else
                    return sizeof(xs_packed_datetime);
            }
            else if (is_derived_from_xs_integer(xtype))
                return sizeof(uint64_t);
            else
                throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type in xmlscm_type_size.");
    }
}

/* Leon special stuff AS IS */

/*
    Function scans the block chain foreward starting from the given one, skipping all empty ones.
    Side effect: blocks are mapped to memory (CHECKP).
    If there are no unempty blocks, function returns XNULL.
*/

xptr getNonemptyBlockLookFore(xptr p)
{
    node_blk_hdr* x;

    while (p != XNULL) {
        CHECKP(p);
        x = (node_blk_hdr*) XADDR(p);

        if (x->count > 0) return p;
        p = x->nblk;
    }

    return p;
}

/*
    Function scans the block chain backward starting from the given one, skipping all empty ones.
    Side effect: blocks are mapped to memory (CHECKP).
    If there are no unempty blocks, function returns XNULL.
*/

xptr getNonemptyBlockLookBack(xptr p)
{
    node_blk_hdr* x;

    while (p != XNULL) {
        CHECKP(p);
        x = (node_blk_hdr*) XADDR(p);

        if (x->count > 0) return p;
        p = x->pblk;
    }

    return p;
}

xptr getNextNonemptyBlock(xptr block)
{
    if (block == XNULL) { return XNULL; }
    return getNonemptyBlockLookFore(getBlockHeaderCP(block)->nblk);
}

xptr getPrevNonemptyBlock(xptr block)
{
    if (block == XNULL) { return XNULL; }
    return getNonemptyBlockLookBack(getBlockHeaderCP(block)->pblk);
}


xptr getFirstAttributeDescendantAndFillPath(std::vector<xptr> &descstack)
{
    xptr node=descstack[descstack.size()-1];
    CHECKP(node);
    xptr child=getFirstElementChild(node);
    xptr attr=XNULL;
    while (child!=XNULL)
    {
        descstack.push_back(child);
        attr=getFirstAttributeChild(child);
        if (attr!=XNULL) return attr;
        attr=getFirstAttributeDescendantAndFillPath(descstack);
        if (attr!=XNULL) return attr;
        descstack.pop_back();
        child=getNextElement(child);
    }
    return XNULL;
}

xptr __getNextBlockDescriptorOfSameSort(xptr nodex)
{
    xptr blk = getNonemptyBlockLookFore(getBlockHeader(nodex)->nblk);
    if (blk == XNULL) return XNULL;
    return getFirstBlockNode(blk);
}

xptr __getPreviousBlockDescriptorOfSameSort(xptr nodex)
{
    xptr blk = getNonemptyBlockLookBack(getBlockHeader(nodex)->pblk);
    if (blk == XNULL) return XNULL;
    return getLastBlockNode(blk);
}


xptr getRightSiblingOfSameSort(xptr nodex)
{
    CHECKP(nodex);
    xptr par_i = nodeGetParentIndirection(nodex);
    xptr next = getNextDescriptorOfSameSort(nodex);

    if (next != XNULL && nodeGetParentIndirection(next) == par_i) {
        return next;
    }

    return XNULL;
}

xptr getLeftSiblingOfSameSort(xptr nodex)
{
    CHECKP(nodex);
    xptr par_i = nodeGetParentIndirection(nodex);
    xptr prev = getPreviousDescriptorOfSameSort(nodex);

    if (prev != XNULL && nodeGetParentIndirection(prev) == par_i) {
        return prev;
    }

    return XNULL;
}

xptr getRightSiblingByType(const xptr node, t_item t)
{
    return getRightSiblingByTypeMask(node, t);
}

#ifdef BITWISE_NODE_TYPES

xptr getRightSiblingByTypeMask(const xptr node, typemask_t tmask)
{
    CHECKP(node);
    xptr i = node;

    /* Optimization : attribute or namespace nodes are always in the first nodes of the element.
     *  If we are looking for a attribute (or ns) node and current node is not attribute (or ns)
     *  then there will be no attribute or ns nodes further
     */
    t_item nodetype = getNodeType(i);
    if (((tmask & ~ti_first_children) == 0) && (nodetype & ti_first_children) == 0) { return XNULL; }

    i = nodeGetRightSibling(i);

    while (i != XNULL) {
        CHECKP(i);
        nodetype = getNodeType(i);

        if (((tmask & ~ti_first_children) == 0) && (nodetype & ti_first_children) == 0) { break; }

        if ((nodetype & tmask) > 0) {
            return i;
        }

        i = nodeGetRightSibling(i);
    }

    return XNULL;
}

#endif /* BITWISE_NODE_TYPES */


xptr getMostLeftSiblingByType(const xptr node, t_item t)
{
    xptr result = XNULL;

    for (xptr i = node; i != XNULL; i = nodeGetLeftSibling(checkp(i))) {
        if (getNodeType(checkp(i)) == t) {
            result = i;
        }
    }

    return result;
}

xptr getLeftSiblingByType(const xptr node, t_item t)
{
    for (xptr i = nodeGetLeftSibling(checkp(node)); i != XNULL; i = nodeGetLeftSibling(checkp(i))) {
        if ((getNodeType(checkp(i)) & t) > 0) {
            return i;
        }
    }

    return XNULL;
}

#define FOR_EACH_SCHEMA_CHILD(i, snode) for (cat_list<sc_ref>::item * i = snode->children->first; i != NULL; i = i->next)

xptr getFirstChildByType(const xptr node, t_item t)
{
    return getFirstChildByTypeMask(node, t);
}

xptr getFirstChildByTypeMask(const xptr node, typemask_t tmask)
{
    /* If there can be only one schema child of given type, just return it.
      Valid only until getAnyChildByTypeMask returns first child by document order */
    if ((tmask & (ti_singleton_element)) == tmask) {
        return getAnyChildByTypeMask(node, tmask);
    }

    CHECKP(node);
    xptr * children = tmp_childlist;
    shft child_count = getChildListPersistent(node, children);
    schema_node_cptr scn = getSchemaNode(node);
    xptr minchild = XNULL;

    int i = 0;
    FOR_EACH_SCHEMA_CHILD(schild, scn) {
        if (i >= child_count) {
            break;
        }

        const xptr child = children[i];
        if (child != XNULL && (schild->object.type & tmask) > 0) {
            if (minchild == XNULL || nid_cmp_effective(child, minchild) < 0) {
                minchild = child;
            }
        }

        ++i;
    }

    return minchild;
}

xptr getFirstChildBySchema(const xptr node, schema_node_cptr childscn)
{
    CHECKP(node);
    return getChildAt(node, childscn->getIndex());
}

xptr getFirstChild(const xptr node)
{
    CHECKP(node);
    xptr * children = tmp_childlist;
    shft child_count = getChildListPersistent(node, children);

    for (int i = 0; i < child_count; i++) {
        const xptr child = children[i];
        if (child != XNULL && nodeGetLeftSibling(checkp(child)) == XNULL) {
            return child;
        }
    }

    return XNULL;
}

xptr getFirstChildByTypeScan(const xptr node, t_item t)
{
    xptr first_child = getFirstChild(node);
    return first_child == XNULL ? XNULL : getRightSiblingByType(first_child, t);
}

xptr getFirstChildByTypeMaskScan(const xptr node, typemask_t tmask)
{
    xptr first_child = getFirstChild(node);
    return first_child == XNULL ? XNULL : getRightSiblingByTypeMask(first_child, tmask);
}


xptr getAnyChild(xptr node)
{
    CHECKP(node);
    xptr * children;
    int child_count = getChildList(node, children);

    for (int i = 0; i < child_count; i++) {
        const xptr child = children[i];
        if (child != XNULL) {
            return child;
        }
    }

    return XNULL;
}

xptr getAnyChildByTypeMask(xptr node, typemask_t tmask)
{
    CHECKP(node);
    xptr * children = tmp_childlist;
    shft child_count = getChildListPersistent(node, children);
    schema_node_cptr scn = getSchemaNode(node);

    int i = 0;
    FOR_EACH_SCHEMA_CHILD(schild, scn) {
        if (i >= child_count) {
            break;
        }

        const xptr child = children[i];
        if ((child != XNULL) && (schild->object.type & tmask) > 0) {
            return child;
        }

        ++i;
    }

    return XNULL;
}


xptr getAnyChildBySchema(const xptr node, schema_node_cptr scn)
{
    return getFirstChildBySchema(node, scn);
}

/*
xptr getRightSiblingBySchema(const xptr node, schema_node_cptr scn)
{
    for (xptr i = nodeGetRightSibling(checkp(node)); i != XNULL; i = nodeGetRightSibling(checkp(i))) {
        if (getSchemaPointer(checkp(i)) == scn.ptr()) {
            return i;
        }
    }

    return XNULL;
}
*/

xptr getRightSiblingBySchema(const xptr node, schema_node_cptr scn)
{
    CHECKP(node);
    xptr parent = nodeGetParent(node);

    if (parent == XNULL) {
        return XNULL;
    }

    for (xptr i = getFirstChildBySchema(parent, scn); i != XNULL; i = getNextSiblingOfSameSort(checkp(i))) {
        if (nid_cmp_effective(i, node) > 0) {
            return i;
        }
    }

    return XNULL;
}

xptr getLeftSiblingBySchema(const xptr node, schema_node_cptr scn)
{
    CHECKP(node);
    xptr parent = nodeGetParent(node);

    if (parent == XNULL) {
        return XNULL;
    }

    xptr child = getFirstChildBySchema(parent, scn);

    if (nid_cmp_effective(child,node) >= 0) {
        return XNULL;
    }

    while (child != XNULL) {
        xptr nextchild = getNextSiblingOfSameSort(checkp(child));
        if (nextchild == XNULL ||
                nid_cmp_effective(nextchild, node) >= 0) {
            return child;
        }
        child = nextchild;
    }

    return XNULL;
}

xptr getLastFirstChildByTypeMask(const xptr node, typemask_t tmask)
{
    CHECKP(node);
    xptr * children = tmp_childlist;
    shft child_count = getChildListPersistent(node, children);
    schema_node_cptr scn = getSchemaNode(node);
    xptr maxchild = XNULL;

    int i = 0;
    FOR_EACH_SCHEMA_CHILD(schild, scn) {
        if (i >= child_count) {
            break;
        }

        const xptr child = children[i];
        if (child != XNULL && (schild->object.type & tmask) > 0) {
            if (maxchild == XNULL || nid_cmp_effective(child, maxchild) > 0) {
                maxchild = child;
            }
        }

        ++i;
    }

    return maxchild;
}


xptr getLastChild(const xptr pnode)
{
    xptr node = getLastFirstChildByTypeMask(pnode, ti_all);
    xptr next = node;

    while (next != XNULL) {
        node = next;
        next = nodeGetRightSibling(checkp(node));
    }

    return node;
}

xptr getLastChildByType(const xptr node, t_item t)
{
    return getLastChildByTypeMask(node, t);
}

xptr getLastChildByTypeMask(const xptr pnode, typemask_t tmask)
{
    xptr node = getLastFirstChildByTypeMask(pnode, tmask);
    xptr next = node;

    while (next != XNULL) {
        node = next;
        next = getRightSiblingByTypeMask(checkp(node), tmask);
    }

    return node;
}

xptr getLastChildBySchema(const xptr pnode, schema_node_cptr scn)
{
    xptr node = getFirstChildBySchema(pnode, scn);
    xptr next = node;

    while (next != XNULL) {
        node = next;
        next = getRightSiblingOfSameSort(checkp(node));
    }

    return node;
}

xptr findAttribute(xptr node, const char* name, const char* uri)
{
    return getChildAt(node, getSchemaNode(node)->find_child_fair(uri, name, attribute));
}

/******* LEON Special *******/

xptr getFirstDescandantBySchema(xptr ancestor, schema_node_cptr scm)
{
    CHECKP(ancestor);
    schema_node_cptr sc_anc = getSchemaNode(ancestor);

    if (sc_anc == scm) {
        return ancestor;
    }

    vector<schema_node_xptr> path;
    schema_node_cptr tmp = scm;

    /* Build schema path from given schema node to ancestor schema node */
    while (tmp.ptr() != sc_anc.ptr())
    {
        path.push_back(tmp.ptr());
        tmp = tmp->parent;
        if (!tmp.found()) return XNULL;
    }

    vector<schema_node_xptr>::reverse_iterator it = path.rbegin();
    xptr child = getFirstChildBySchema(ancestor, *it);

    /* Is there is no child nodes on this path, exit */
    if (child == XNULL) {
        return XNULL;
    }

    ++it;

    while (it != path.rend()) {
        xptr grchild = XNULL;

        /* If we found the first child node on the step of path traversing,
         * we should scan through all children of this type to find one, that have
         * requested grandchild for the next step */
        while (grchild == XNULL) {
            grchild = getFirstChildBySchema(child, *it);

            if (grchild == XNULL) {
                child = getNextDescriptorOfSameSort(child);
                if (child == XNULL || nid_cmp_effective(child, ancestor) != 2) {
                    return XNULL;
                }
            }
        }

        child = grchild;
        ++it;
    }

    U_ASSERT(nid_cmp_effective(child, ancestor) == 2);

    return child;
}


xptr getRootNode(const xptr node)
{
    xptr cur = node;
    xptr p;

    CHECKP(cur);

    while ((p = nodeGetParent(cur)) != XNULL) {
        CHECKP(p);
        if (getNodeType(p) == virtual_root) {
            return cur;
        }
        cur = p;
    }

    return cur;
}


int getMedianDescriptor(int s, int r, xptr end, xptr * res)
{
    int cnt = (r-s) / 2;
    *res = end;
    for (int i = 0; i < cnt; i++) {
        *res = nodeGetPrev(*res);
    }
    return r - cnt;
}

int getMedianDescriptor2(int s, int r, xptr start, xptr * res)
{
    int cnt = (r-s) / 2;
    *res = start;
    for (int i = 0; i < cnt; i++) {
        *res = nodeGetNext(*res);
    }
    return s+cnt;
}


/*returns the next non-descendant node in document that fits input schema_node */

xptr getNextNDNode(xptr node, schema_node_cptr scn)
{
    xptr blk = scn->bblk;

    // 1. finding block
    while (blk != XNULL)
    {
        xptr ni = getLastBlockNode(blk);
        if (ni != XNULL && nid_cmp_effective(ni, node) == 1) {
            break;
        }
        blk = getNextNonemptyBlock(blk);
    }

    if (blk == XNULL) {
        return XNULL;
    }

    // 2. finding node in block
    if (nid_cmp_effective(getFirstBlockNode(checkp(blk)), node) == 1) {
        return getFirstBlockNode(checkp(blk));
    }

    CHECKP(blk);

    int s = 0;
    int r = getBlockHeader(blk)->count;
    xptr right = getLastBlockNode(blk);
    xptr med;
    while (s < r-1)
    {
        int i = getMedianDescriptor(s, r, right, &med);
        if (nid_cmp_effective(med, node) == 1) {
            r=i;
            right=med;
        } else {
            s=i;
        }
        CHECKP(blk);
    }
    return right;
}

xptr getPreviousNANode(xptr node, schema_node_cptr scn)
{
    xptr blk = scn->bblk;
    xptr lastn = XNULL;

    // 1. finding block
    while (blk != XNULL)
    {
        xptr ni = getLastBlockNode(blk);
        if (ni != XNULL) {
            lastn = ni;
            if (nid_cmp_effective(ni, node) != -1) {
                break;
            }
        }
        blk = getNextNonemptyBlock(blk);
    }

    if (blk == XNULL) {
        return lastn;
    }

    //2. finding node in block
    CHECKP(blk);
    xptr left = getFirstBlockNode(blk);

    if (nid_cmp_effective(left, node) != -1) {
        return getPreviousDescriptorOfSameSort(left);
    }

    int s = 0;
    int r = getBlockHeader(checkp(blk))->count;

    xptr med;
    while (s < r-1) {
        int i = getMedianDescriptor2(s, r, left, &med);
        if (nid_cmp_effective(med,node) != -1) {
            r=i;
        } else {
            s=i;
            left=med;
        }
        CHECKP(blk);
    }
    return left;
}


/*returns the next non-descendant node in document*/
xptr getNextNDNode(const xptr node)
{
    xptr tmp = node;

    while (tmp != XNULL) {
        CHECKP(tmp);
        xptr rdsc = nodeGetRightSibling(tmp);
        if (rdsc != XNULL) {
            if (getSchemaNode(tmp)->parent->type == virtual_root) {
                return XNULL;
            } else {
                return rdsc;
            }
        }

        tmp = nodeGetParent(tmp);
    }

    return XNULL;
}

xptr getNextDONode(const xptr node)
{
    xptr tmp = getFirstChildNode(node);

    if (tmp != XNULL) {
        return tmp;
    } else {
        return getNextNDNode(node);
    }
}

bool hasLeftSiblingDM(xptr node)
{
    xptr tmp = nodeGetLeftSibling(node);
    if (tmp == XNULL)return false;

    CHECKP(tmp);
    bool res = dm_children_accessor_filter(getNodeType(tmp));

    CHECKP(node);
    return res;
}


/*returns the previous node in document*/
xptr getPreviousDONode(xptr node)
{
    xptr tmp = node;

    while (true) {
        CHECKP(tmp);
        if (hasLeftSiblingDM(tmp)) {
            if (getSchemaNode(tmp)->parent->type == virtual_root) {
                return XNULL;
            } else {
                break;
            }
        } else {
            return nodeGetParent(tmp);
        }
    }

    tmp = nodeGetLeftSibling(tmp);
    xptr tmp2;
    while (true) {
        CHECKP(tmp);
        tmp2 = getLastChild(tmp);
        if (tmp2 == XNULL || !dm_children_accessor_filter(getNodeType(tmp2))) {
            CHECKP(tmp);
            return tmp;
        } else {
            tmp=tmp2;
        }
    }
}

/* returns the inderection pointer to the ancestor of the descriptor that corresponds
to the selected ancestor by scheme */
xptr getAncestorIndirectionByScheme (xptr node, const schema_node_cptr scm_node, const schema_node_cptr scm_anc)
{
    if (scm_node == scm_anc) {
        return nodeGetIndirection(checkp(node));
    }

    schema_node_cptr tmp = scm_node;
    xptr tmp_node = node;
    while (tmp->parent != scm_anc.ptr()) {
        tmp_node = nodeGetParent(tmp_node);
        CHECKP(tmp_node);
        if (tmp->parent==XNULL) {
            return XNULL;
        }
        tmp = tmp->parent;
    }
    return nodeGetParentIndirection(tmp_node);
}

xptr getAncestorBySchemeCP(xptr node, schema_node_xptr scm_anc)
{
    CHECKP(node);
    schema_node_xptr scm_node = getSchemaPointer(node);
    while (scm_node != scm_anc) {
        node = nodeGetParent(node);
        if (node == XNULL) { return XNULL; }
        CHECKP(node);
        scm_node = getSchemaPointer(node);
    }
    return node;
}

template < typename Iterator > static inline
xptr getDescriptorWithPstrInThisBlock(xptr blk, xptr node) {
    CHECKP(node);

    if (!getSchemaNode(node)->has_text()) {
        throw SYSTEM_EXCEPTION("Wrong type of node: should be text node descendant");
    }

    xptr p = node;
    xptr lastp = p;

    while (p != XNULL) {
        p = Iterator::nextNode(p);

        if (p == XNULL) { break; }

        CommonTextNode tn(p);

        if (tn.isPstr()) {
            if (!same_block(tn.getPstrPtr(), blk)) { break; };
            lastp = p;
        }
    }

    return lastp;
}

xptr getLeftmostDescriptorWithPstrInThisBlock(xptr blk, xptr node) {
    return getDescriptorWithPstrInThisBlock<NodeIteratorBackward>(blk, node);
};

xptr getRightmostDescriptorWithPstrInThisBlock(xptr blk,xptr node) {
    return getDescriptorWithPstrInThisBlock<NodeIteratorForeward>(blk, node);
};
