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

xmlns_ptr SednaNode::getNamespaceValue() const {
    if (!snode.found()) { snode = getSchemaNode(checkp(node)); }
    if (snode->type == xml_namespace) {
        return NSNode(node).getNamespaceLocal();
    } else {
        return snode->get_xmlns();
    }
}

xsd::QName SednaNode::getQName() const {
    if (!snode.found()) { snode = getSchemaNode(checkp(node)); }

    if (snode->type == xml_namespace) {
        xmlns_ptr ns = NSNode(node).getNamespaceLocal();
        if (ns->has_prefix()) {
            return xsd::QName::createUnchecked(xmlns_touch("xmlns", "http://www.w3.org/2000/xmlns/"), ns->get_prefix());
        } else {
            return xsd::QName::createUnchecked(NULL_XMLNS, "xmlns");
        }
    };

    return snode->get_qname();
}

const text_source_t SednaNode::getValue() const {
    if (getNodeKind() == xml_namespace) {
        return text_source_cstr(NSNode(checkp(node)).getNamespaceLocal()->get_uri());
    } else {
        return text_source_node(node);
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
