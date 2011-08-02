/*
 * File: xmlserializer.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/base.h"
#include "common/sedna.h"

#include "tr/crmutils/xmlserializer.h"
#include "tr/strings/strings.h"
#include "tr/structures/portal.h"
#include "tr/structures/nodeutils.h"
#include "tr/crmutils/exec_output.h"
#include "tr/executor/base/PPBase.h"
#include "tr/crmutils/xdm.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/xsd.h"
#include "tr/crmutils/xdm.h"
#include "tr/structures/xmlns.h"

#define MAX_INDENT 64

inline static
void filterText(StrMatcher &stm, se_ostream * crmout, int sclass, TextBufferReader &reader)
{
    while (reader.read()) {
        stm.parse(reader.buffer, reader.size, write_func, crmout, sclass);
    }
    stm.flush(write_func, crmout);
}

static const char * XML_docTagStart = "<?xml version=\"1.0\" standalone=\"yes\"";
static const char * XML_docTagEnd = "?>";
static const char * XML_openTag = "<";
static const char * XML_closeTag = ">";

void XMLPrintQName(xsd::QName qname, se_ostream* crmout)
{
    U_ASSERT(qname.valid());

    if (qname.getXmlNs() != NULL_XMLNS && qname.getXmlNs()->has_prefix()) {
        *crmout << qname.getXmlNs()->get_prefix() << ":";
    }

    *crmout << qname.getLocalName();
}

inline static
bool isAttributeAt(IXDMNodeList * list) {
    const IXDMNode * node = list->getNode();
    return (node != NULL && (node->getNodeKind() & ti_first_children) != 0);
}

inline static
bool isNamespaceAt(IXDMNodeList * list) {
    const IXDMNode * node = list->getNode();
    return (node != NULL && node->getNodeKind() == xml_namespace);
}

inline static
void printIndent(se_ostream * crmout, int level, const char * indent_cache, size_t indent_len) {
    if (level >= MAX_INDENT) { level = MAX_INDENT - 1; }
    crmout->write(indent_cache, indent_len * level);
}

inline static
struct text_source_t getTupleText(const tuple_cell &t) {
    if (is_fixed_size_type(t.get_atomic_type())) {
        return text_source_cstr(get_lexical_representation_for_fixed_size_atomic(executor_globals::mem_str_buf2, t));
    } else {
        return text_source_tuple_cell(t);
    }
}

struct ElementContext {
    xsd::QName qname;
    bool preserveCData;
};


XDMSerializer::XDMSerializer() : savedCData(false) {
    const xmlns_ptr ns = xmlns_touch("xml", "http://www.w3.org/XML/1998/namespace");
    nsPrefixMap.insert(std::pair<std::string, xmlns_ptr>(std::string(ns->get_prefix()), ns));
}

bool XDMSerializer::declareNamespace(xmlns_ptr ns) {
    NSPrefixMap::iterator nsi = nsPrefixMap.find(std::string(ns->get_prefix()));

    if (nsi == nsPrefixMap.end()) {
        /* We are declaring a new namespace */
        nsNamespaceStack.push(NSSubsitutionPair(ns, NULL_XMLNS));
        nsPrefixMap.insert(std::pair<std::string, xmlns_ptr>(std::string(ns->get_prefix()), ns));

        return true;
    } else if (nsi->second != ns) {
        /* This prefix is already bound to a different namespace */
        nsNamespaceStack.push(NSSubsitutionPair(ns, nsi->second));
        nsi->second = ns;

        return true;
    }

    return false;
}

xmlns_ptr XDMSerializer::getDefaultNamespace() {
    NSPrefixMap::iterator nsi = nsPrefixMap.find("");

    if (nsi == nsPrefixMap.end() || nsi->second == NULL_XMLNS || nsi->second->empty_uri()) {
        return NULL_XMLNS;
    } else {
        return nsi->second;
    }
}


void XDMSerializer::undeclareNamespaces(int count) {
    while (count-- > 0) {
        const NSSubsitutionPair x = nsNamespaceStack.top();

        if (x.second != NULL_XMLNS) {
            nsPrefixMap.find(std::string(x.first->get_prefix()))->second = x.second;
        } else {
            nsPrefixMap.erase(std::string(x.first->get_prefix()));
        }

        nsNamespaceStack.pop();
    }
}


void XDMSerializer::printNode(const Node node)
{
    node.checkp();
    const t_item t = node.getNodeType();
    switch (t) {
      case element : {
          SednaNode element(node.getPtr());
          printElement(&element);
      } break;
      case document : {
          SednaNode document(node.getPtr());
          printDocument(text_source_node(node.getPtr()), &document);
      } break;
      case xml_namespace :
      case attribute : {
          SednaNode attribute(node.getPtr());
          printAttribute(&attribute);
      } break;
      case comment : case pr_ins : {
          printText(t, text_source_node(node.getPtr()));
      } break;
      case text : {
          if (getTextFlags(node.getPtr(), cdata_section) > 0) {
              savedCData = true;
          }

          printText(t, text_source_node(node.getPtr()));

          savedCData = false;
      } break;
      default : throw USER_EXCEPTION(SE2302);
    }
}

void XDMSerializer::printNode(IXDMNode * node)
{
    const t_item t = node->getNodeKind();
    switch (t) {
      case element :
          printElement(node); break;
      case document :
          printDocument(node->getValue(), node); break;
      case xml_namespace :
      case attribute :
          printAttribute(node); break;
      case comment :
      case pr_ins :
          printText(t, node->getValue()); break;
      case text : {
          SednaNode* _cdataHack = dynamic_cast<SednaNode*>(node);

          if (_cdataHack != NULL && getTextFlags(_cdataHack->getXptr(), cdata_section) > 0) {
              savedCData = true;
          }

          printText(t, node->getValue());

          savedCData = false;
      } break;
      default :
        throw USER_EXCEPTION(SE2302);
    }
}

void XDMSerializer::serialize(tuple & t) {
    if (crmout == NULL) { return; }

    if (t.is_eos()) { return; }

    for (int i = 0; i < t.cells_number; i++) {
        if (separatorNeeded && i > 0) { (*crmout) << " "; }
        const tuple_cell tc = t.cells[i];

        if (tc.is_portal()) {
            portal::VirtualNode element(tc);
            printElement(&element);
        } else if (tc.is_node()) {
            printNode(tc.get_node());
        } else {
            U_ASSERT(tc.is_atomic());
            printAtomic(tc);
        }
    }
}

void XMLSerializer::printAtomic(const tuple_cell &t)
{
     if (t.is_atomic_type(xs_QName)) {
        const char * prefix = t.get_xs_qname().getPrefix();
        const char * local_name = t.get_xs_qname().getLocalName();

        if (prefix != NULL && *prefix != '\0') {
            stringFilter.parse(prefix, strlen(prefix), write_func, crmout, pat_element);
            stringFilter.parse(":", 1, write_func, crmout, pat_element);
        }
        stringFilter.parse(local_name, strlen(local_name), write_func, crmout, pat_element);
        stringFilter.flush(write_func, crmout);
    } else {
        TextBufferReader reader(getTupleText(t));
        filterText(stringFilter, crmout, pat_element | useCharmapFlag, reader);
    }
}

void XMLSerializer::printDocument(const text_source_t docname, IXDMNode * content)
{
    // The following stands for backwards compatibility and will be removed in future
    const bool printDocTags = (dynamic_cast<SednaNode *>(content) != NULL) && IS_DATA_BLOCK(dynamic_cast<SednaNode *>(content)->getXptr());

    IXDMNodeList * children = content->getAllChildren();

    if (printDocTags) {
        (*crmout) << docPISeqOpen;

        while (isAttributeAt(children)) {
            printAttribute(children->getNode());
            children->next();
        }

        (*crmout) << docPISeqClose;
    }

    while (!children->end()) {
        printNode(children->getNode());
        children->next();
    }
}

void XMLSerializer::initialize()
{
    docPISeqOpen = XML_docTagStart;
    docPISeqClose = XML_docTagEnd;
    openTagSeq = XML_openTag;
    closeTagSeq = XML_closeTag;

    elementContext = NULL;
    indentLevel = 0;
    indentElements = options->indent;
    indentNext = indentElements;
    separatorNeeded = options->separateTuples;
    indentSequence = options->indentSequence;
    indentSequenceLength = strlen(indentSequence);

    if (indentCache != NULL) {
        free(indentCache);
    }

    indentCache = (char *) malloc(indentSequenceLength * MAX_INDENT);

    char * c = indentCache;
    for (size_t i = 0; i < MAX_INDENT; ++i) {
        memcpy(c, indentSequence, indentSequenceLength);
        c += indentSequenceLength;
    }

    useCharmapFlag = (int) (options->useCharmap ? pat_charmap : 0);

    // FIXME: we should reinitialize StrMatcher here
    for (GlobalSerializationOptions::Stringmap::const_iterator it = options->charmap.begin();
      it != options->charmap.end(); it++) {
        stringFilter.add_str(it->first.c_str(), it->second.c_str(), pat_charmap);
    }
}

void XMLSerializer::printElementName(IXDMNode* element)
{
    XMLPrintQName(element->getQName(), crmout);
}


void XMLSerializer::printElement(IXDMNode * elementInterface)
{
    CHECK_TIMER_FLAG;

    ElementContext * parentContext = this->elementContext;
    ElementContext context = {elementInterface->getQName(), false};
    elementContext = &context;
    bool indented = indentNext;
    int namespaceCount = 0;

    if (indentNext) {
        if (indentLevel > 0) {
            (*crmout) << "\n";
            printIndent(crmout, indentLevel, indentCache, indentSequenceLength);
        }
        ++indentLevel;
    }
    indentNext = indentElements;

    (*crmout) << openTagSeq;

    printElementName(elementInterface);

    /* If null element namespace implies non-null default namespace */

    if (context.qname.getXmlNs() != NULL_XMLNS || getDefaultNamespace() != NULL_XMLNS) {
        xmlns_ptr ns = context.qname.getXmlNs();
      /* Fix of bad update policy, where default namespace is not added with element */
        if (ns == NULL_XMLNS) { ns = xmlns_touch("", ""); }

        if (declareNamespace(ns)) {
            (*crmout) << " ";
            printNamespace(ns);
            ++namespaceCount;
        }
    }

    IXDMNodeList * children = elementInterface->getAllChildren();

    while (isAttributeAt(children)) {
        IXDMNode * node = children->getNode();
        t_item kind = node->getNodeKind();
        const xmlns_ptr ns = kind == attribute ? node->getQName().getXmlNs() : node->getNamespaceValue();

        if (ns != NULL_XMLNS && declareNamespace(ns)) {
            (*crmout) << " ";
            printNamespace(ns);
            ++namespaceCount;
        }

        if (kind == attribute) {
            (*crmout) << " ";
            printAttribute(node);
        }

        children->next();
    }

    if (children->end()) {
        (*crmout) << "/" << closeTagSeq;
    } else {
        (*crmout) << closeTagSeq;

        do {
            printNode(children->getNode());
        } while (children->next());

        if (indented && indentNext) { (*crmout) << "\n"; printIndent(crmout, indentLevel-1, indentCache, indentSequenceLength); }

        (*crmout) << openTagSeq << "/";
        printElementName(elementInterface);
        (*crmout) << closeTagSeq;
    }

    indentNext = indentElements;
    if (indented) { indentLevel--; }

    undeclareNamespaces(namespaceCount);
    elementContext = parentContext;
}

void XMLSerializer::printNamespace(xmlns_ptr ns)
{
    (*crmout) << "xmlns";
    if (ns->has_prefix()) { (*crmout) << ":" << ns->get_prefix(); }
    (*crmout) << "=\"";
    stringFilter.parse(ns->get_uri(), strlen(ns->get_uri()), write_func, crmout, pat_attribute);
    stringFilter.flush(write_func, crmout);
    (*crmout) << "\"";
}

void XMLSerializer::printAttribute(IXDMNode * attribute)
{
    XMLPrintQName(attribute->getQName(), crmout);
    (*crmout) << "=\"";
    TextBufferReader reader(attribute->getValue());
    filterText(stringFilter, crmout, pat_attribute | useCharmapFlag, reader);
    (*crmout) << "\"";
}

void XMLSerializer::printText(t_item type, const text_source_t value)
{
    CHECK_TIMER_FLAG;

    TextBufferReader reader(value);

    if (type == text) {
        indentNext = false;
        if (savedCData || (elementContext != NULL && options != NULL &&
              (options->cdataSectionElements.find(elementContext->qname) != options->cdataSectionElements.end()))) {
            (*crmout) << "<![CDATA[";
            // StringMatcher must substitute "]]>" with "]]>]]<![CDATA[<"
            filterText(stringFilter, crmout, pat_cdata, reader);
            (*crmout) << "]]>";
        } else {
            filterText(stringFilter, crmout, pat_element | useCharmapFlag, reader);
        }
    } else {
        U_ASSERT(type == comment || type == pr_ins);

        if (indentNext) { (*crmout) << "\n"; }
        if (type == comment) { (*crmout) << openTagSeq << "!--"; } else { (*crmout) << openTagSeq << "?"; };
        filterText(stringFilter, crmout, useCharmapFlag, reader);
        if (type == comment) { (*crmout) << "--" << closeTagSeq; } else { (*crmout) << "?" << closeTagSeq; }
    };
}

void SXMLSerializer::printText(t_item type, const text_source_t value)
{
    CHECK_TIMER_FLAG;

    TextBufferReader reader(value);

    if (type == text) {
        (*crmout) << " \"";
        filterText(stringFilter, crmout, pat_text, reader);
        (*crmout) << "\"";
    } else {
        U_ASSERT(type == comment || type == pr_ins);

        if (type == comment) { (*crmout) << "(*COMMENT* \""; } else { (*crmout) << "(*PI* \""; };
        filterText(stringFilter, crmout, pat_text, reader);
        if (type == comment) { (*crmout) << "\")"; } else { (*crmout) << "\")"; }
    };
}

void SXMLSerializer::printDocument(const text_source_t docname, IXDMNode * content)
{
    const bool printDocTags = true;

    IXDMNodeList * children = content->getAllChildren();

    if (printDocTags) {
        (*crmout) << "(*TOP* ";

        if (isAttributeAt(children)) {
            (*crmout) << "(@";
            while (isAttributeAt(children)) {
                printAttribute(children->getNode());
                children->next();
            }
            (*crmout) << ")";
        }
    }

    while (!children->end()) {
        printNode(children->getNode());
        children->next();
    }

    (*crmout) << ")";
}

void SXMLSerializer::printAtomic(const tuple_cell &t)
{
//    (*crmout) << " (";
    if (t.is_atomic_type(xs_boolean)) {
        (*crmout) << (t.get_xs_boolean() ? "#t" : "#f");
    } else {
        const bool quoted = !is_fixed_size_type(t.get_atomic_type());
        if (quoted) { (*crmout) << "\""; }
        XMLSerializer::printAtomic(t);
        if (quoted) { (*crmout) << "\""; }
    }
//    (*crmout) << ")";
}

void SXMLSerializer::printAttribute(IXDMNode * attribute)
{
    TextBufferReader reader(attribute->getValue());
    (*crmout) << " (";
    XMLPrintQName(attribute->getQName(), crmout);
    (*crmout) << " \"";
    filterText(stringFilter, crmout, pat_text, reader);
    (*crmout) << "\")";
}

void SXMLSerializer::printElement(IXDMNode * elementInterface)
{
    CHECK_TIMER_FLAG;

    ElementContext * parentContext = this->elementContext;
    ElementContext context = {elementInterface->getQName(), false};
    elementContext = &context;

    (*crmout) << " (";
    XMLPrintQName(elementInterface->getQName(), crmout);

    IXDMNodeList * children = elementInterface->getAllChildren();

    if (isAttributeAt(children)) {
        (*crmout) << " (@ ";
        while (isAttributeAt(children)) {
            (*crmout) << " ";
            printAttribute(children->getNode());
            children->next();
        }
        (*crmout) << ")";
    }

    if (!children->end()) do {
        printNode(children->getNode());
    } while (children->next());

    (*crmout) << ")";

    elementContext = parentContext;
}

XMLSerializer::XMLSerializer() : indentCache(NULL), docPISeqOpen(XML_docTagStart), docPISeqClose( XML_docTagEnd), openTagSeq(XML_openTag), closeTagSeq(XML_closeTag)
{
    stringFilter.add_str(">","&gt;", pat_attribute | pat_element);
    stringFilter.add_str("<","&lt;", pat_attribute | pat_element);
    stringFilter.add_str("&","&amp;", pat_attribute | pat_element);
    stringFilter.add_str("\"","&quot;", pat_attribute);
    stringFilter.add_str("]]>", "]]]]><![CDATA[>", pat_cdata);
}

SXMLSerializer::SXMLSerializer() :
    XMLSerializer(0) /* Call for dummy constructor of parent class */
{
    stringFilter.add_str("\"","\\\"", pat_text);
    stringFilter.add_str("(","\\(", ~pat_text);
    stringFilter.add_str(")","\\)", ~pat_text);
}
