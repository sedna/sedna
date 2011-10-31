/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "SCElementProducer.h"

#include "tr/mo/mo.h"
#include "tr/updates/updates.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/mo/microoperations.h"
#include "tr/mo/blocks.h"
#include "tr/structures/metadata.h"

tuple_cell SCElementProducer::addAttribute(const xsd::QName& qname, const text_source_t value, xmlscm_type type)
{
    processAtomics();

    if (node_kind == document) {
        throw XQUERY_EXCEPTION2(XPTY0004, "Attribute node in the document constructor content sequence");
    }

    if (noMoreAttributes && node_kind != virtual_root) {
        throw XQUERY_EXCEPTION(XQTY0024);
    }

    if (left != XNULL) {
        insert_attribute(indirectionDereferenceCP(left), XNULL, XNULL, qname, type, value);
    } else {
        insert_attribute(XNULL, XNULL, indirectionDereferenceCP(self), qname, type, value);
    }

    left = get_last_mo_inderection();

    return tuple_cell::node_indir(left);
}

tuple_cell SCElementProducer::addNS(const xmlns_ptr ns)
{
    processAtomics();

    if (node_kind == document) {
        throw XQUERY_EXCEPTION2(XPTY0004, "Namespace node in the document constructor content sequence");
    }

    if (noMoreAttributes && node_kind != virtual_root) {
        throw XQUERY_EXCEPTION(XQTY0024);
    }

    if (left != XNULL) {
        insert_namespace(indirectionDereferenceCP(left), XNULL, XNULL, ns);
    } else {
        insert_namespace(XNULL, XNULL, indirectionDereferenceCP(self), ns);
    }

    left = get_last_mo_inderection();

    return tuple_cell::node_indir(left);
}

IElementProducer* SCElementProducer::addElement(const xsd::QName& qname, xmlscm_type type)
{
    processAtomics();

    if (left != XNULL) {
        insert_element(indirectionDereferenceCP(left), XNULL, XNULL, qname, type);
    } else {
        insert_element(XNULL, XNULL, indirectionDereferenceCP(self), qname, type);
    }

    left = get_last_mo_inderection();
    noMoreAttributes = true;

    return new SCElementProducer(this, left);
}

bool SCElementProducer::hasNode(const tuple_cell& node)
{
    return node.is_node() && node.get_node_inderection() == left;
}

tuple_cell SCElementProducer::addNode(const tuple_cell& tc, bool preserveType)
{
    processAtomics();

    U_ASSERT(tc.is_node());

    Node node = Node(tc.get_node()).checkp();
    t_item nodeType = node.getNodeType();

    if (nodeType == virtual_root) {
        throw XQUERY_EXCEPTION2(SE1003, "Virtual root node type in the document constructor content sequence");
    }

    if (node_kind == document && ((nodeType & (attribute | xml_namespace)) > 0)) {
        if (nodeType == attribute) {
            throw XQUERY_EXCEPTION2(XPTY0004, "Attribute node in the document constructor content sequence");
        } else if (nodeType == xml_namespace) {
            throw XQUERY_EXCEPTION2(XPTY0004, "Namespace node in the document constructor content sequence");
        }
    }

    if ((nodeType & ti_first_children) > 0 && noMoreAttributes && nodeType != virtual_root) {
        throw XQUERY_EXCEPTION(XQTY0024);
    }

    xptr newNode = XNULL;

    if (nodeType == text && CommonTextNode(node).isEmpty()) {
        return tuple_cell::eos();
    }

    if (nodeType == document) {
        newNode = copy_node_content(self, node.getPtr(), left, NULL, preserveType);
    } else {
        /* depth 1 means, that this is not an update operation copying */
        newNode = deep_copy_node_ii(left, XNULL, self, node.getPtr(), NULL, preserveType, 1);
    }

    if (newNode == XNULL) {
        return tuple_cell::eos();
    }

    left = newNode;

    if ((nodeType & ti_first_children) == 0) {
        noMoreAttributes = true;
    }

    return tuple_cell::node_indir(newNode);
}

tuple_cell SCElementProducer::addAtomic(const tuple_cell& atomic)
{
    U_ASSERT(atomic.is_atomic());

    if (node_kind == virtual_root) {
        throw USER_EXCEPTION(SE2020);
    }

    textAccum->add(tuple(atomic));

    return tuple_cell::eos();
}


tuple_cell SCElementProducer::addText(const text_source_t value)
{
    processAtomics();

    strsize_t size = get_text_size(value);

    /* from  http://www.w3.org/TR/xquery/#id-textConstructors:

        It is possible for a text node constructor to construct a text node containing a zero-length string.
        However, if used in the content of a constructed element or document node, such a text node will be deleted or merged with another text node.
    */

    if (node_kind != virtual_root && size == 0) {
        return tuple_cell::eos();
    }

    if (left != XNULL) {
        insert_text(indirectionDereferenceCP(left), XNULL, XNULL, value);
    } else {
        insert_text(XNULL, XNULL, indirectionDereferenceCP(self), value);
    }

    left = get_last_mo_inderection();
    noMoreAttributes = true;

    return tuple_cell::node_indir(left);
}

tuple_cell SCElementProducer::addComment(const text_source_t value)
{
    processAtomics();

    if (left != XNULL) {
        insert_comment(indirectionDereferenceCP(left), XNULL, XNULL, value);
    } else {
        insert_comment(XNULL, XNULL, indirectionDereferenceCP(self), value);
    }

    left = get_last_mo_inderection();
    noMoreAttributes = true;

    return tuple_cell::node_indir(left);

}

tuple_cell SCElementProducer::addPI(const xsd::NCName& name, const text_source_t value)
{
    processAtomics();

    if (left != XNULL) {
        insert_pi(indirectionDereferenceCP(left), XNULL, XNULL, name, value);
    } else {
        insert_pi(XNULL, XNULL, indirectionDereferenceCP(self), name, value);
    }

    left = get_last_mo_inderection();
    noMoreAttributes = true;

    return tuple_cell::node_indir(left);
}


tuple_cell SCElementProducer::close()
{
    processAtomics();

    return tuple_cell::node_indir(self);
}

text_source_t concatTextSequence(sequence* textSequence)
{
    if (textSequence->size() == 0) {
        return NULL_TEXT;
    }

    executor_globals::tmp_op_str_buf.clear();

    sequence::iterator it = textSequence->begin();

    while (it != textSequence->end()) {
        executor_globals::tmp_op_str_buf.append(cast(tuple_cell::make_sure_light_atomic((*it).cells[0]), xs_string));
        it++;
    };

    if (executor_globals::tmp_op_str_buf.get_size() > 0) {
        return text_source_strbuf(&(executor_globals::tmp_op_str_buf));
    } else {
        return NULL_TEXT;
    }
}


tuple_cell SCElementProducer::processAtomics()
{
    if (textAccum->size() > 0) { // This check is just for optimization purposes
        text_source_t ts = concatTextSequence(textAccum);
        textAccum->clear();

        if (!text_is_null(ts)) {
            if (left != XNULL) {
                insert_text(indirectionDereferenceCP(left), XNULL, XNULL, ts);
            } else {
                insert_text(XNULL, XNULL, indirectionDereferenceCP(self), ts);
            }

            left = get_last_mo_inderection();
            noMoreAttributes = true;
            return tuple_cell::node_indir(left);
        }
    }

    return tuple_cell::eos();
}

Node SCElementProducer::getNode()
{
    return Node(indirectionDereferenceCP(self));
}

SCElementProducer::~SCElementProducer()
{
    delete textAccum;
}

SCElementProducer* SCElementProducer::createTemporaryDocument(const xsd::AnyURI& name, dynamic_context* cxt)
{
    xptr documentNode = insert_document(name.getValue(), false);
    SCElementProducer* result = NULL;

    if (cxt != NULL) {
        cxt->add_temporary_doc_node(documentNode);
    }

    result = new SCElementProducer(getIndirectionSafeCP(documentNode));
    result->node_kind = document;
    result->noMoreAttributes = true;

    return result;
}

xptr SCElementProducer::virtualRoot = XNULL;
SCElementProducer * SCElementProducer::virtualRootProducer = NULL;

void SCElementProducer::deleteVirtualRoot()
{
    U_ASSERT(virtualRoot != XNULL);

    schema_node_cptr scnVirtualRoot = getSchemaNode(virtualRoot);
    nid_delete(virtualRoot);
    vmm_delete_block(virtualRoot);
    scnVirtualRoot->drop();
    virtualRoot = XNULL;

    delete virtualRootProducer;
    virtualRootProducer = NULL;
}

SCElementProducer* SCElementProducer::getVirtualRoot(xptr vr)
{
    if (virtualRootProducer != NULL) {
        return virtualRootProducer;
    };

    U_ASSERT(vr == XNULL);

    node_info_t node_info = {XNULL, XNULL, XNULL, virtual_root};
    schema_node_cptr scnVirtualRoot = doc_schema_node_object::create_virtual_root()->p;
    xptr blk = createBlock(scnVirtualRoot, XNULL);
    virtualRoot = insertNodeFirst(blk, &node_info);

    virtualRootProducer = new SCElementProducer(getIndirectionSafeCP(virtualRoot));
    virtualRootProducer->node_kind = virtual_root;
    return virtualRootProducer;
}

SCElementProducer::SCElementProducer(SCElementProducer* _parent, xptr node)
  : parent(_parent), node_kind(element), parentNode(_parent->self), self(node), left(XNULL), noMoreAttributes(false), textAccum(new sequence(1))
{
}

SCElementProducer::SCElementProducer(xptr existent)
  : parent(NULL), node_kind(element), parentNode(XNULL), self(existent), left(XNULL), noMoreAttributes(false), textAccum(new sequence(1))
{
}
