/*
 * File:  nodeinterface.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef NODEINTERFACE_H_
#define NODEINTERFACE_H_

#include "tr/structures/nodeblocks.h"
#include "tr/structures/nodeoperations.h"
#include "tr/structures/xmlns.h"
#include "tr/structures/nodeutils.h"

#include "tr/strings/strings_base.h"
#include "tr/strings/text_data.h"

class Node {
protected:
    mutable xptr node;
public:
    inline Node() : node(XNULL) {};
    inline Node(const xptr p) : node(p) {};
    inline Node(const Node & anode) : node(anode.node) {};

    inline
    xptr getPtr() const { return node; };

    inline const Node & checkp() const { if (node != XNULL) { CHECKP(node); } return *this; }
    inline Node & checkp() { if (node != XNULL) { CHECKP(node); } return *this; }

    inline
    bool isNull() const { return node == XNULL; };

    inline
    Node getLeft() const { return Node(getBaseFromAnyNode(node)->ldsc); };
    inline
    Node getRight() const { return Node(getBaseFromAnyNode(node)->rdsc); };

    inline
    Node getNext() const { return Node(nodeGetNext(node)); };
    inline
    Node getPrev() const { return Node(nodeGetPrev(node)); };

    inline
    Node getNextSameSort() const { return Node(getNextSiblingOfSameSort(node)); };

    inline
    bool hasParent() const { return getBaseFromAnyNode(node)->pdsc != XNULL; };
    inline
    Node getParent() const { return Node(indirectionDereferenceCP(getBaseFromAnyNode(node)->pdsc)); };

    inline
    Node getActualParent() const {
        Node parent = getParent();
        return parent.isNull() || parent.checkp().getNodeType() == virtual_root ? XNULL : parent;
    };

    inline
    t_nid getNID() const { return getBaseFromAnyNode(node)->nid; };

    inline
    xptr getParentIndirection() const { return getBaseFromAnyNode(node)->pdsc; };
    inline
    xptr getIndirection() const { return getBaseFromAnyNode(node)->indir; };

    inline
    t_item getNodeType() const { return internal::getBlockHeader(node)->node_type; };

    inline
    schema_node_cptr getSchemaNode() const { return internal::getBlockHeader(node)->snode; };

    inline
    bool isTmpNode() const { return isTmpBlock(node); }

    /* Legacy: has a bad name */
    inline
    bool isNodeChild() const { CHECKP(node); t_item t = getNodeType(); return (t & ti_first_children) == 0; }

    inline
    bool isNodeInCollection() const { CHECKP(node); return getSchemaNode()->root->nodecnt > 1; }

    inline
    bool isTextBasedNode() const { return internal::isTextType(internal::getBlockHeader(node)->node_type); };

    inline
    bool operator==(const Node & other) const {
        return node == other.node;
    }

    inline
    bool operator<(const Node & other) const {
        // The idea is all XNULL nodes are greater then non-null

        if (node == XNULL && other.node == XNULL) return false;
        if (node == XNULL) return false;
        if (other.node == XNULL) return true;

        return nid_cmp(node, other.node) < 0;
    }
};

/*
class MaterializedNode {
private:
    t_item item_type;
    xptr ptr;

    struct dsc::base_t base;
    struct dsc::text_t text;
    xmlscm_type scm_type;
    shft pi_target_separator;
    xmlns_ptr_pers ns;
public :
    MaterializedNode(const xptr p);
    MaterializedNode(const Node node);

    inline t_item getNodeType() { return item_type; };
    inline xptr getNodeXptr() { return ptr; };
    inline xmlscm_type getType() { return scm_type; };
    inline xptr getLeftSibling() { return base.ldsc; };
    inline xptr getRightSibling() { return base.rdsc; };
    inline xptr getParentIndirection() { return base.pdsc; };
    inline xptr getNID() { return base.nid; };
    inline xptr getIndirection() { return base.indir; };
    inline xptr getNext() { return base.desc_next; };
    inline xmlns_ptr getNamespace() { return ns; };
};
*/

namespace internal {
    enum text_type_t { innode, pstr, pstrlong };
}

class CommonTextNode : public Node {
public:
    inline CommonTextNode(const xptr & p) : Node(p) {};
    inline CommonTextNode(const Node & anode) : Node(anode) {};

    inline CommonTextNode & checkp() { if (node != XNULL) { CHECKP(node); } return *this; }

    inline
    xptr getTextPointer() const {
        CHECKP(node);
        return nodeGetTextPointer(getTextFromAnyNode(node));
    };

    inline
    xptr getTextPointerCP() const {
        xptr text = getTextPointer();
        CHECKP(text);
        return text;
    };

    inline
    strsize_t getTextSize() const { CHECKP(node); return nodeGetTextSize(getTextFromAnyNode(node)); };

    inline
    bool isEmpty() const { CHECKP(node); return getTextFromAnyNode(node)->size == 0; };

    inline
    bool isPstrLong() const { CHECKP(node); return getTextFromAnyNode(node)->size == internal::textInPstrLong; }

    inline
    bool isPstr() const {
        return getStorageType() == internal::pstr;
    }

    inline
    xptr getPstrPtr() const {
        U_ASSERT(isPstr());
        xptr result;
        memcpy(&result, getTextFromAnyNode(node)->data, sizeof(xptr));
        return result;
    }

    inline
    xptr getPstrLongPtr() const {
        U_ASSERT(isPstrLong());
        xptr result;
        memcpy(&result, getTextFromAnyNode(node)->data, sizeof(xptr));
        return result;
    }

    char * copyToBuffer(char * buffer, strsize_t position, size_t size) const;

    inline
    struct text_source_t getTextSource() {
        if (isPstrLong()) {
	    return text_source_pstrlong(getTextPointerCP());
        } else {
	    return text_source_pstr(getTextPointerCP(), getTextSize());
        }
    };

    inline
    enum internal::text_type_t getStorageType() const {
        CHECKP(node);
        const uint16_t size = getTextFromAnyNode(node)->size;
        if (size == internal::textInPstrLong) {
            return internal::pstrlong;
        } else if (size > internal::maxDescriptorTextSize) {
            return internal::pstr;
        } else {
            return internal::innode;
        }
    };
};

class TextNode : public CommonTextNode {
public:
    inline TextNode(const xptr & p) : CommonTextNode(p) {};
    inline TextNode(const Node & anode) : CommonTextNode(anode) {};
    inline TextNode & checkp() { if (node != XNULL) { CHECKP(node); } return *this; }
};

class ElementNode : public Node {
public:
    inline ElementNode(const xptr & p) : Node(p) {};
    inline ElementNode(const Node & anode) : Node(anode) {};
    inline ElementNode & checkp() { if (node != XNULL) { CHECKP(node); } return *this; }

    inline xmlscm_type getType() const { CHECKP(node); return ((internal::element_node *) XADDR(node))->type; };
};

class AttributeNode : public CommonTextNode {
public:
    inline AttributeNode(const xptr & p) : CommonTextNode(p) {};
    inline AttributeNode(const Node & anode) : CommonTextNode(anode) {};
    inline AttributeNode & checkp() { if (node != XNULL) { CHECKP(node); } return *this; }

    inline xmlscm_type getType() const { CHECKP(node); return ((internal::attribute_node *) XADDR(node))->type; };
};

class NSNode : public Node {
public:
    inline NSNode(const xptr & p) : Node(p) {};
    inline NSNode(const Node & anode) : Node(anode) {};
    inline NSNode & checkp() { if (node != XNULL) { CHECKP(node); } return *this; }

    inline bool isNullPrefix() const {
        const xmlns_ptr x = getNamespaceLocal();
        return (x->prefix == NULL) || (*(x->prefix) == '\0');
    }

    inline xmlns_ptr_pers getNamespace() const { return ((internal::namespace_node *) XADDR(node))->ns; }
    inline xmlns_ptr getNamespaceLocal() const { return xmlns_touch(((internal::namespace_node *) XADDR(node))->ns); }
};


class PINode : public CommonTextNode {
public:
    inline PINode(const xptr & p) : CommonTextNode(p) {};
    inline PINode(const Node & anode) : CommonTextNode(anode) {};
    inline PINode & checkp() { if (node != XNULL) { CHECKP(node); } return *this; }

    inline
    int compareTarget(const char * against) const {
        U_ASSERT(!isPstrLong());
        return strncmp(against, (char *) XADDR(getTextPointerCP()), getPITargetSize());
    }

    inline
    size_t getPITargetSize() const {
        return ((internal::pi_node *) XADDR(node))->separator;
    }

    inline
    strsize_t getPIDataSize() const {
        internal::pi_node * pi = (internal::pi_node *) XADDR(node);
        U_ASSERT(!isPstrLong());

        /* Zero-length PI has size = target, while non-zero
         * length PI has target size = target + data + 1
         */

        if (pi->text.size < PSTRMAXSIZE && pi->separator == pi->text.size) {
            return 0;
        } else {
            return getTextSize() - pi->separator - 1;
        }
    }

    inline
    xptr getPIData() const {
        internal::pi_node * pi = (internal::pi_node *) XADDR(node);
        U_ASSERT(!isPstrLong());

        if (isEmptyPI()) {
            return XNULL;
        } else {
            return getTextPointerCP() + pi->separator + 1;
        }
    }

    bool isEmptyPI() const {
        return getPIDataSize() == 0;
    }
};

class NodeBlockHeader {
private:
    xptr header;

    inline
    internal::node_blk_hdr * getNodeHeader() { return (internal::node_blk_hdr *) XADDR(header); };
public:
    NodeBlockHeader(const Node &node);
    NodeBlockHeader(const xptr p) : header(block_xptr(p)) {};

    inline
    NodeBlockHeader & checkp() { CHECKP(header); return *this; }

    inline
    Node getLastDescriptor() { return Node(getLastBlockNode(header)); };

    inline
    Node getFirstDescriptor() { return Node(getFirstBlockNode(header)); };

    inline
    t_item getNodeType() { return getNodeHeader()->node_type; };

    inline
    int getNodeCount() { return getNodeHeader()->count; };

    inline
    schema_node_cptr getSchemaNode() { return schema_node_cptr(getNodeHeader()->snode); };

    inline
    int getNodeChildCount() { return internal::getNodeChildCount(getNodeHeader()); };
};


#endif /* NODEINTERFACE_H_ */
