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

static const char * XML_docTagStart = "<?xml version=\"1.0\" standalone=\"yes\"";
static const char * XML_docTagEnd = "?>";

static std::string getTagName(const schema_node_cptr kind) {
    const xmlns_ptr ns = kind->get_xmlns();
    std::string tagName;
    if (ns != NULL_XMLNS && ns->has_prefix()) {
        char * tagNameBuffer = (char *) malloc(strlen(ns->get_prefix()) + strlen(kind->get_name()) + 2);
        tagName = sprintf(tagNameBuffer, "%s:%s", ns->get_prefix(), kind->get_name());
        free(tagNameBuffer);
    } else {
        tagName = kind->get_name();
    }
    return tagName;
}

str_cursor * getTextCursor(const text_source_t text) {
    str_cursor * result = NULL;
    switch (text.type) {
    case text_source_t::text_mem:
        result = new mem_cursor((char *) text.u.cstr, text.size);
    case text_source_t::text_pstr: {
        result = new pstr_cursor(text.u.data, text.size);
    } break;
    case text_source_t::text_estr: {
        result = new pstr_long_cursor(text.u.data);
    } break;
    case text_source_t::text_pstrlong: {
        result = new estr_cursor(text.u.data, text.size);
    } break;
    }
    return result;
}

class TextBufferReader {
private:
    str_cursor * cursor;
public:
    char * buffer;
    int size;

    TextBufferReader(const text_source_t text) : cursor(getTextCursor(text)), buffer((char*) malloc(PAGE_SIZE)) {};
    ~TextBufferReader() { free(cursor); free(buffer); };

    bool read() { return ((size = cursor->copy_blk(buffer)) != 0); }
};


void XMLSerializer::serializeTuple(tuple * t) {
    if (t->is_eos()) {
        return;
    }

    indentLevel = 0;
    indentNext = true;

    for (int i=0; i < t->cells_number; i++) {
        if (t->cells[i].is_portal()) {
            portal::VirtualElementIterator it(t->cells[i]);
            printElement(&it);
            it.close();
        } else if (t->cells[i].is_node()) {
            this->serialize(t->cells[i].get_node());
        } else if (t->cells[i].is_light_atomic()) {
            const tuple_cell tc = t->cells[i];
            if (is_fixed_size_type(tc.get_atomic_type())) {
                get_lexical_representation_for_fixed_size_atomic(executor_globals::mem_str_buf2, tc, xml);
                crmout.writextext(executor_globals::mem_str_buf2, strlen(executor_globals::mem_str_buf2));
            } else if (tc.get_atomic_type() == xs_QName) {
                const char *prefix = xs_QName_get_prefix(tc.get_str_mem());
                if (prefix && strlen(prefix) != 0) {
                    crmout.writextext(prefix, strlen(prefix));
                    crmout.writextext(":", 1);
                }
                crmout.writextext((char*)xs_QName_get_local_name(tc.get_str_mem()),
                    strlen(xs_QName_get_local_name(tc.get_str_mem())));
            } else {
                crmout.writextext(tc.get_str_mem(), tc.get_strlen_mem());
            }
        } else {
//            print_tuple_cell(output->getOutBuffer(), tup.cells[i]);
        }
    }
}

inline static
bool isNamespaceOrAttribute(const xptr node) {
    CHECKP(node);
    return (getNodeType(node) & (attribute | xml_namespace)) > 0;
}

inline static
bool isNamespace(const xptr node) {
    CHECKP(node);
    return (getNodeType(node) & xml_namespace) > 0;
}

inline static
bool isAttribute(const xptr node) {
    CHECKP(node);
    return (getNodeType(node) & attribute) > 0;
}

static const SerializationOptions defaultOptions = { true, " ", true };

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
        this->serialize(tuple(tuple_cell::node(child)));
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
    crmout.writeattribute(ns->get_uri(), strlen(ns->get_uri()));
    crmout << "\"";
}

struct SednaElementIterator : public ElementChildIterator {
private:
    mutable schema_node_xptr snode;
    xptr node;
public:
    SednaElementIterator(const xptr parent) {
        CHECKP(parent);
        snode = getSchemaPointer(parent);
        node = getFirstChild(parent);
    };

    virtual schema_node_cptr getSchemaNode() const { return snode; };

    virtual void next(tuple &t) {
        if (node == XNULL) {
            t.set_eos();
        } else {
            t.copy(tuple_cell::node(node));
            CHECKP(node);
            node = nodeGetRightSibling(node);
        };
    };
};

void XMLSerializer::printElement(ElementChildIterator * elementInterface) {
    ElementContext * parentContext = this->elementContext;
    ElementContext context(elementInterface->getSchemaNode());
    elementContext = &context;
    bool indented = indentNext;
    tuple childt(1);

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

    while (!childt.is_eos() && childt.cells[0].is_node()) {
        const xptr node = childt.cells[0].get_node();
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
            const tuple_cell tc = childt.cells[0];
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

