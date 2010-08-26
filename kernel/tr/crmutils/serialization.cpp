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

static const char * XML_docTagStart = "<?xml version=\"1.0\" standalone=\"yes\"";
static const char * XML_docTagEnd = "?>";
static const char * XML_elTagEnd = ">";


StdXMLOutput::StdXMLOutput(dynamic_context * a_cxt, se_ostream &a_crmout) :
    crmout(a_crmout), cxt(a_cxt) { }

StdXMLOutput::~StdXMLOutput() { }

void StdXMLOutput::reset() {
    finishing = NULL;
    tagStarted = false;
}

void StdXMLOutput::tuple(const tuple_cell &tc) {
    U_ASSERT(tc.is_light_atomic());
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
}

void StdXMLOutput::startDocument() {
}

void StdXMLOutput::finishTag() {
    if (tagStarted) {
        crmout << finishing;
        tagStarted = false;
    }
}

void StdXMLOutput::endDocument() {
    finishTag();
}

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

void * StdXMLOutput::startElement(const schema_node_cptr kind) {
    finishTag();
    tagStack.push(getTagName(kind));
    tagStarted = true;
    finishing = XML_elTagEnd;
    crmout << '<' << tagStack.top().c_str();
}

void StdXMLOutput::endElement(void * context) {
    U_ASSERT(!tagStack.empty());
    if (!tagStarted) {
        crmout << "</" << tagStack.top().c_str() << '>';
    } else {
        crmout << "/>";
        tagStarted = false;
    }
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

void StdXMLOutput::attribute(const schema_node_cptr kind, const text_source_t value) {
    crmout << " " << getTagName(kind).c_str() << "=\"";
    TextBufferReader reader(value);
    while (reader.read()) { crmout.writeattribute(reader.buffer, reader.size); }
    crmout << "\"";
}

void StdXMLOutput::text(const text_source_t value , void * markup) {
    finishTag();
    TextBufferReader reader(value);
    while (reader.read()) { crmout.writextext(reader.buffer, reader.size); }
}

void StdXMLOutput::comment(const text_source_t value) {
    finishTag();
    crmout << "<!--";
    TextBufferReader reader(value);
    while (reader.read()) { crmout.write(reader.buffer, reader.size); }
    crmout << "-->";
}

void StdXMLOutput::pi(const text_source_t value, int target_offset) {
    finishTag();
}

void StdXMLOutput::xmlnamespace(const xmlns_ptr ns) {
    finishTag();
}




Serializer::Serializer(XMLOutput * a_output) : output(a_output) {

};

void XMLSerializer::serializeTuple(tuple * t) {
    if (t->is_eos()) {
        return;
    }

    for (int i=0; i < t->cells_number; i++) {
        output->reset();
        if (t->cells[i].is_portal()) {
            t->cells[i].get_portal()->print(this);
        } else if (t->cells[i].is_node()) {
            this->serialize(t->cells[i].get_node());
        } else if (t->cells[i].is_light_atomic()) {
            output->tuple(t->cells[i]);
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

void XMLSerializer::printDocument(xptr node) {
    crmout << XML_docTagStart;

    xptr child=getFirstChild(node);
    while (child != XNULL && isNamespaceOrAttribute(child)) {
        this->serialize(child);
        child = nodeGetRightSibling(checkp(child));
    }

    crmout << XML_docTagEnd;

    while (child != XNULL) {
        this->serialize(child);
        child = nodeGetRightSibling(checkp(child));
    }
}

using namespace std;

ElementContext::ElementContext(xptr node) : snode(getSchemaNode(node)), tagName(getTagName(snode)), defaultNamespace(NULL_XMLNS) {}

void XMLSerializer::printNamespace(xmlns_ptr ns) {
    crmout << "xmlns";
    if (ns->has_prefix()) { crmout << ":" << ns->get_prefix(); }
    crmout << "=\"";
    crmout.writeattribute(ns->get_uri(), strlen(ns->get_uri()));
    crmout << "\"";
}

void XMLSerializer::printElement(xptr node) {
    ElementContext * parentContext = this->elementContext;
    ElementContext context(node, parentContext);
    elementContext = &context;
    std::vector<xmlns_ptr> localNamespaces;
    std::map<std::string, xmlns_ptr> localNamespaceMap;

    crmout << "<" << context.tagName.c_str();

    /* Default namespace handling. There are two signs of the default namespace:
     * en empty prefix xmlns node and empty prefix in schema node. We output each
     * default namespace only once */
    if (parentContext != NULL && parentContext->defaultNamespace == context.snode->get_xmlns()) {
        context.defaultNamespace = parentContext->defaultNamespace;
    } else {
        context.defaultNamespace = context.snode->get_xmlns();
        if (context.defaultNamespace != NULL_XMLNS) {
            printNamespace(context.defaultNamespace);
        }
    }

    U_ASSERT(context.defaultNamespace == NULL_XMLNS || !context.defaultNamespace->has_prefix());

    /* Handle namespaces. Save them to globally defined and also to local list for easy deletion. */
    xptr child=getFirstChild(node);
    while (child != XNULL && isNamespace(child)) {
        xmlns_ptr ns = NSNode(child).getNamespaceLocal();
        if (definedNamespaces.find(ns) == definedNamespaces.end()) {
            definedNamespaces.insert(ns);
            localNamespaces.push_back(ns);
            printNamespace(ns);
        }
        child = nodeGetRightSibling(checkp(child));
    }

    while (child != XNULL && isAttribute(child)) {
        child = nodeGetRightSibling(checkp(child));
    }

    if (child == XNULL) {
        crmout << "/>";
    } else {
        crmout << ">";

        while (child != XNULL) {
            this->serialize(child);
            child = nodeGetRightSibling(checkp(child));
        }

        crmout << "</" << tagName.c_str() << ">";
    }

    if (!context.localNamespaces.empty()) {
        definedNamespaces.erase(localNamespaces.begin(), localNamespaces.end());
    }

    this->elementContext = &parentContext;
}


void XMLSerializer::serialize(xptr node) {
    CHECK_TIMER_FLAG;

    CHECKP(node);
    switch(getNodeType(node)) {
    case virtual_root:
        // exception
        break;
    case document: { printDocument(node); } break;
    case element: { printElement(node); } break;
    case xml_namespace: {
        printNamespace(NSNode(node).getNamespaceLocal());
    } break;
    case attribute: {
        TextBufferReader reader(text_source_node(node));
        crmout << getTagName(node).c_str() << "=\"";
        while (reader.read()) { crmout.writeattribute(reader.buffer, reader.size); }
        crmout << "\"";
    } break;
    case text: {
        output->text(text_source_node(node), NULL);
    } break;
    case comment: {
        TextBufferReader reader(text_source_node(node));
        crmout << "<!--";
        while (reader.read()) { crmout.writextext(reader.buffer, reader.size); }
        crmout << "-->";
    } break;
    case pr_ins: {
        TextBufferReader reader(text_source_node(node));
        crmout << "<?";
        while (reader.read()) { crmout.writextext(reader.buffer, reader.size); }
        crmout << "?>";
    } break;
    }
}

