/*
 * File: serialization.cpp
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "common/sedna.h"
#include "common/base.h"

#include "tr/crmutils/serializer.h"

#include "tr/strings/strings.h"
#include "tr/structures/nodeutils.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/xsd.h"
#include "tr/tr_globals.h"
#include "tr/structures/portal.h"

#include <string.h>







inline static
bool isNamespaceOrAttribute(const xptr node) {
    CHECKP(node)
    return (getNodeType(node) & (attribute | xml_namespace)) > 0;
}

inline static
bool isNamespace(const xptr node) {
    CHECKP(node)
    return (getNodeType(node) & xml_namespace) > 0;
}

inline static
bool isAttribute(const xptr node) {
    CHECKP(node)
    return (getNodeType(node) & attribute) > 0;
}

//static const XMLSerializationOptions defaultOptions = { true, " ", true };

XMLSerializer::XMLSerializer(dynamic_context * a_cxt, se_ostream &a_crmout)
    : cxt(a_cxt), crmout(a_crmout), options(&defaultOptions), elementContext(NULL) {
}


void XMLSerializer::printDocument(xptr node) {
    indentLevel = 0;
    indentNext = true;

    crmout << XML_docTagStart;

    xptr child=getFirstChild(node);
    while (child != XNULL && isNamespaceOrAttribute(child)) {
        this->serialize(tuple(tuple_cell::node(child)));
        child = nodeGetRightSibling(checkp(child));
    }

    crmout << XML_docTagEnd;

    while (child != XNULL) {
        SednaElementIterator it(child);
        printElement(&it);
        child = nodeGetRightSibling(checkp(child));
    }
}

using namespace std;

static void inline print_indent(se_ostream& crmout, int level, const char * indent) {
    for (int i=0; i < level; i++) { crmout << indent; }
}

ElementContext::ElementContext(const schema_node_cptr node) : snode(node), tagName(getTagName(snode)), defaultNamespace(NULL_XMLNS) {}

void XMLSerializer::printNamespace(xmlns_ptr ns) {
    crmout << "xmlns";
    if (ns->has_prefix()) { crmout << ":" << ns->get_prefix(); }
    crmout << "=\"";
    stm->parse(s,n,write_func,this,(int)pat_attribute);

    crmout.writeattribute(ns->get_uri(), strlen(ns->get_uri()));
    crmout << "\"";
}

void XMLSerializer::printElement(SerializedElementIterator * elementInterface) {
    CHECK_TIMER_FLAG;

    ElementContext * parentContext = this->elementContext;
    ElementContext context(elementInterface->getSchemaNode());
    elementContext = &context;
    bool indented = indentNext;
    tuple childt(1);
    tuple_cell * childItem = childt.cells;

    if (indentNext) { crmout << "\n"; print_indent(crmout, indentLevel++, this->options->indentSequence); }
    crmout << "<" << context.tagName.c_str();
    indentNext = options->indent;

    /* Default namespace handling. There are two signs of the default namespace:
     * empty prefix xmlns node and empty prefix in schema node. We output each
     * default namespace only once */
    if (parentContext != NULL && parentContext->defaultNamespace == context.snode->get_xmlns()) {
        context.defaultNamespace = parentContext->defaultNamespace;
    } else {
        context.defaultNamespace = context.snode->get_xmlns();
        if (context.defaultNamespace != NULL_XMLNS) {
            crmout << " ";
            printNamespace(context.defaultNamespace);
        } else if (parentContext != NULL && parentContext->defaultNamespace != NULL_XMLNS) {
            crmout << " ";
            // undeclare namespace
            printNamespace(xmlns_touch("", ""));
        }
    }

    U_ASSERT(context.defaultNamespace == NULL_XMLNS || !context.defaultNamespace->has_prefix());

    elementInterface->next(childt);

    while (!childt.is_eos() && childItem->is_node()) {
        const xptr node = childItem->get_node();
        if (isNamespace(node)) {
            xmlns_ptr ns = NSNode(node).getNamespaceLocal();
            if (ns->has_prefix()) {
                crmout << " ";
                printNamespace(ns);
            }
        } else if (isAttribute(node)) {
            crmout << " ";
            this->printAttribute(node);
        } else {
            break;
            /* If this is not an namespace or attribute node
             * It must be processed in the next loop */
        }
        elementInterface->next(childt);
    }

    if (childt.is_eos()) {
        crmout << "/>";
    } else {
        crmout << ">";

        while (!childt.is_eos()) {
            const tuple_cell tc = *childItem;
            if (tc.is_portal()) {
                U_ASSERT(dynamic_cast<portal::VirtualElementIterator*>(elementInterface) != NULL);
                portal::VirtualElementIterator it(reinterpret_cast<portal::VirtualElementIterator*>(elementInterface));
                printElement(&it);
                it.close();
            } else if (tc.is_node()) {
                const Node node = tc.get_node();
                node.checkp();
                if (node.getNodeType() == element) {
                    SednaElementIterator it(node.getPtr());
                    printElement(&it);
                } else {
                    U_ASSERT((node.getNodeType() & (text | comment | pr_ins)) > 0);
                    printText(node.getPtr());
                }
            } else {
                U_ASSERT(false);
            }
            elementInterface->next(childt);
        }

        if (indented && indentNext) { crmout << "\n"; print_indent(crmout, indentLevel-1, this->options->indentSequence); }
        crmout << "</" << context.tagName.c_str() << ">";
    }
    indentNext = options->indent;
    if (indented) { indentLevel--; }

    elementContext = parentContext;
}

void XMLSerializer::printAttribute(xptr node) {
    TextBufferReader reader(text_source_node(node));
    CHECKP(node);
    crmout << getTagName(getSchemaNode(node)).c_str() << "=\"";
    while (reader.read()) { crmout.writeattribute(reader.buffer, reader.size); }
    crmout << "\"";
}

static char * strcdata(char * str, size_t len) {
    int state = 0;
    while (len > 0) {
        switch (state) {
        case 1: if (*str == ']') { ++state; } else { state = 0; } break;
        case 2: if (*str == '>') { return str+1; } else if (*str != ']') { state = 0; } break;
        case 0: default: {
            char * tmp = (char*) memchr(str, ']', len);
            if (tmp == NULL) { return NULL; }
            len += (str - tmp);
            str = tmp;
        }
        }
        len--; str++;
    }
    return NULL;
}

void XMLSerializer::printText(xptr node) {
    CHECK_TIMER_FLAG;
    CHECKP(node);

    const t_item nodetype = getNodeType(node);
    TextBufferReader reader(text_source_node(node));

    if (nodetype == text) {
        indentNext = false;
        if (options->cdataSectionElements) {
            crmout << "<![CDATA[";
            while (reader.read()) {
                char * pos, * buffer = reader.buffer;
                size_t size = reader.size;
                while ((pos = strcdata(buffer, size)) != NULL) {
                    ptrdiff_t passed = (pos - buffer);
                    crmout << "]]>]]<![CDATA[<";
                    if (passed > 3) { crmout.write(buffer, passed - 3); }
                    buffer = pos;
                    size -= passed;
                }
                if (size > 0) { crmout.write(buffer, size); }
            }
            crmout << "]]>";
        } else {
            while (reader.read()) { crmout.writextext(reader.buffer, reader.size); }
        }
    } else {
        U_ASSERT(nodetype == comment || nodetype == pr_ins);

        if (indentNext) { crmout << "\n"; }
        if (nodetype == comment) { crmout << "<!--"; } else { crmout << "<?"; };
        while (reader.read()) { crmout.writextext(reader.buffer, reader.size); }
        if (nodetype == comment) { crmout << "-->"; } else { crmout << "?>"; }
    };
}


SXMLSerializer::SXMLSerializer(dynamic_context * a_cxt, se_ostream &a_crmout)
{
}

SXMLSerializer::~SXMLSerializer()
{
}

void SXMLSerializer::serialize(tuple & t)
{
}


void SXMLSerializer::printDocument(xptr node)
{
    crmout << XML_docTagStart;

    xptr child=getFirstChild(node);
    while (child != XNULL && isNamespaceOrAttribute(child)) {
        this->serialize(tuple(tuple_cell::node(child)));
        child = nodeGetRightSibling(checkp(child));
    }

    crmout << XML_docTagEnd;

    while (child != XNULL) {
        this->serialize(tuple(tuple_cell::node(child)));
        child = nodeGetRightSibling(checkp(child));
    }
}

void SXMLSerializer::printElement(SerializedElementIterator * element)
{
}

void SXMLSerializer::printNamespace(xmlns_ptr ns)
{
}

void SXMLSerializer::printAttribute(xptr node)
{
}

void SXMLSerializer::printText(xptr node)
{
}
