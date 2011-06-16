#include "tr/crmutils/xdm.h"
#include "tr/structures/nodeutils.h"
#include "tr/strings/strings.h"

xptr SednaNode::getXptr() const { return node; };

void SednaNode::setNode(xptr a_node) {
    node = a_node;
    snode = XNULL;
}

SednaNode::SednaNode(xptr a_node) : node(a_node), snode(XNULL), childList(NULL) { }

SednaNode::~SednaNode() {
    if (childList != NULL) { delete childList; }
}

t_item SednaNode::getNodeKind() const {
    if (!snode.found()) { snode = getSchemaNode(checkp(node)); }
    return snode->type;
}

const char * SednaNode::getLocalName() const {
    if (!snode.found()) { snode = getSchemaNode(checkp(node)); }
    return snode->get_name();
}

xmlns_ptr SednaNode::getNamespace() const {
    if (!snode.found()) { snode = getSchemaNode(checkp(node)); }
    if (snode->type == xml_namespace) {
        return NSNode(node).getNamespaceLocal();
    } else {
        return snode->get_xmlns();
    }
}

xsd::QName SednaNode::getQName() const {
    if (!snode.found()) { snode = getSchemaNode(checkp(node)); }
    return xsd::QName::createNsN(snode->get_xmlns(), snode->get_name(), true);
}

const text_source_t SednaNode::getValue() const {
    if (getNodeKind() == xml_namespace) {
        return text_source_cstr(NSNode(checkp(node)).getNamespaceLocal()->get_uri());
    } else {
        return text_source_node(node);
    }
}

void SednaNode::printNodeName(se_ostream & out) const {
    if (!snode.found()) { snode = getSchemaNode(checkp(node)); }
    if (snode->type == xml_namespace) {
        const xmlns_ptr ns = NSNode(checkp(node)).getNamespaceLocal();
        U_ASSERT(ns != NULL_XMLNS);
        out << "xmlns";
        if (ns->has_prefix()) {
          out << ":" << ns->get_prefix();
        }
    } else {
        const xmlns_ptr ns = snode->get_xmlns();
        if (ns != NULL_XMLNS && ns->has_prefix()) {
            out << ns->get_prefix() << ":";
        }
        out << snode->get_name();
    }
}

IXDMNodeList * SednaNode::getAllChildren() {
    if (childList != NULL) { delete childList; }
    childList = new SednaNodeList(getFirstChild(checkp(node)));
    return childList;
}

SednaNodeList::SednaNodeList(xptr firstChild) : child(firstChild), node(NULL) { }

SednaNodeList::~SednaNodeList() {
    if (node != NULL) { delete node; }
}

bool SednaNodeList::end() {
    return child == XNULL;
};

bool SednaNodeList::next() {
    if (child != XNULL) { child = getNextNode(child); }
    return child != XNULL;
};

IXDMNode * SednaNodeList::getNode() {
    if (child == XNULL) { return NULL; }
    if (node == NULL) { node = new SednaNode(child); } else { node->setNode(child); }
    return node;
}
