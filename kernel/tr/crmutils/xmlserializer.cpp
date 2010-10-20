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

/* static filter, used when no user filter is passed */
static StrMatcher plainFilter;

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
      case comment : case pr_ins : case text : {
          printText(t, text_source_node(node.getPtr()));
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
      case text :
          printText(t, node->getValue()); break;
      default :
        throw USER_EXCEPTION(SE2302);
    }
}

void XDMSerializer::serialize(tuple & t) {
    if (crmout == NULL) { return; }
    if (stm == NULL) { stm = &plainFilter; }

    if (t.is_eos()) { return; }

    for (int i = 0; i < t.cells_number; i++) {
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

inline static
struct text_source_t get_tuple_text(const tuple_cell &t) {
    if (is_fixed_size_type(t.get_atomic_type())) {
        return text_source_cstr(get_lexical_representation_for_fixed_size_atomic(executor_globals::mem_str_buf2, t));
    } else {
        return text_source_tuple_cell(t);
    }
}

void XMLSerializer::printAtomic(const tuple_cell &t)
{
    if (t.is_atomic_type(xs_QName)) {
        const char * prefix = xs_QName_get_prefix(t.get_str_mem());
        const char * local_name = xs_QName_get_local_name(t.get_str_mem());

        if (prefix != NULL && *prefix != '\0') {
            stm->parse(prefix, strlen(prefix), write_func, crmout, pat_xml_element);
            stm->parse(":", 1, write_func, crmout, pat_xml_element);
        }
        stm->parse(local_name, strlen(local_name), write_func, crmout, pat_xml_element);
    } else {
        TextBufferReader reader(get_tuple_text(t));

        while (reader.read()) {
            stm->parse(reader.buffer, reader.size, write_func, crmout, pat_xml_element);
        }
    }
    stm->flush(write_func, crmout);
}

static const char * XML_docTagStart = "<?xml version=\"1.0\" standalone=\"yes\"";
static const char * XML_docTagEnd = "?>";

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

void XMLSerializer::printDocument(const text_source_t docname, IXDMNode * content)
{
    // The following stands for backwards compatibility and will be removed in future
    const bool printDocTags = (dynamic_cast<SednaNode *>(content) != NULL) && IS_DATA_BLOCK(dynamic_cast<SednaNode *>(content)->getXptr());

    IXDMNodeList * children = content->getAllChildren();

    if (printDocTags) {
        (*crmout) << XML_docTagStart;

        while (isAttributeAt(children)) {
            printAttribute(children->getNode());
            children->next();
        }

        (*crmout) << XML_docTagEnd;
    }

    while (!children->end()) {
        printNode(children->getNode());
        children->next();
    }
}

inline static
void print_indent(se_ostream * crmout, int level, const char * indent) {
    for (int i=0; i < level; i++) { (*crmout) << indent; }
}

void XMLSerializer::initialize()
{
    elementContext = NULL;
    indentLevel = 0;
    indentNext = options->indent;
}

struct ElementContext {
    const char * name;
    const xmlns_ptr ns;
    xmlns_ptr defaultNamespace;
};

void XMLSerializer::printElement(IXDMNode * elementInterface)
{
    CHECK_TIMER_FLAG;

    ElementContext * parentContext = this->elementContext;
    ElementContext context = {elementInterface->getLocalName(), elementInterface->getNamespace()};
    elementContext = &context;
    bool indented = indentNext;
    int namespaceCount = 0;

    if (indentNext) { (*crmout) << "\n"; print_indent(crmout, indentLevel++, this->options->indentSequence); }
    indentNext = options->indent;

    (*crmout) << "<";
    elementInterface->printNodeName(*crmout);

    /* specially handle default namespace */
    if (context.ns != NULL_XMLNS && !context.ns.has_prefix() && declareNamespace(context.ns)) {
        printNamespace(context.ns);
    }

    IXDMNodeList * children = elementInterface->getAllChildren();

    while (isAttributeAt(children)) {
        const xmlns_ptr ns = children->getNode()->getNamespace();

        if (ns != NULL_XMLNS && declareNamespace(ns)) {
            (*crmout) << " ";
            printNamespace(context.ns);
        }

        if (children->getNode()->getNodeKind() == attribute) {
            (*crmout) << " ";
            printAttribute(children->getNode());
        }
        children->next();
    }

    if (children->end()) {
        (*crmout) << "/>";
    } else {
        (*crmout) << ">";

        do {
            printNode(children->getNode());
        } while (children->next());

        if (indented && indentNext) { (*crmout) << "\n"; print_indent(crmout, indentLevel-1, this->options->indentSequence); }

        (*crmout) << "</";
        elementInterface->printNodeName(*crmout);
        (*crmout) << ">";
    }

    indentNext = options->indent;
    if (indented) { indentLevel--; }

    elementContext = parentContext;
}

void XMLSerializer::printNamespace(xmlns_ptr ns)
{
    (*crmout) << "xmlns";
    if (ns->has_prefix()) { (*crmout) << ":" << ns->get_prefix(); }
    (*crmout) << "=\"";
    stm->parse(ns->get_uri(), strlen(ns->get_uri()), write_func, crmout, (int) pat_xml_attribute);
    stm->flush(write_func, crmout);
    (*crmout) << "\"";
}

void XMLSerializer::printAttribute(IXDMNode * attribute)
{
    attribute->printNodeName(*crmout);
    TextBufferReader reader(attribute->getValue());
    (*crmout) << "=\"";
    while (reader.read()) {
        stm->parse(reader.buffer, reader.size, write_func, crmout, (int) pat_xml_attribute);
    }
    stm->flush(write_func, crmout);
    (*crmout) << "\"";
}

inline static
bool set_contains_string(std::set<std::string> * set, const char * item) {
    std::string s = item;
    return set->find(s) != set->end();
}

void XMLSerializer::printText(t_item type, const text_source_t value)
{
    CHECK_TIMER_FLAG;

    TextBufferReader reader(value);

    if (type == text) {
        indentNext = false;
        if (elementContext != NULL && set_contains_string(options->cdataSectionElements, elementContext->name)) {
            (*crmout) << "<![CDATA[";
            // StringMatcher must substitute "]]>" with "]]>]]<![CDATA[<"
            while (reader.read()) {
                stm->parse(reader.buffer, reader.size, write_func, crmout, pat_cdata);
            }
            stm->flush(write_func, crmout);
            (*crmout) << "]]>";
        } else {
            while (reader.read()) {
                stm->parse(reader.buffer, reader.size, write_func, crmout, pat_xml_element);
            }
            stm->flush(write_func, crmout);
        }
    } else {
        U_ASSERT(type == comment || type == pr_ins);

        if (indentNext) { (*crmout) << "\n"; }
        if (type == comment) { (*crmout) << "<!--"; } else { (*crmout) << "<?"; };
        while (reader.read()) {
            stm->parse(reader.buffer, reader.size, write_func, crmout, pat_xml_attribute);
        }
        stm->flush(write_func, crmout);
        if (type == comment) { (*crmout) << "-->"; } else { (*crmout) << "?>"; }
    };
}

void SXMLSerializer::printText(t_item type, const text_source_t value)
{
    CHECK_TIMER_FLAG;

    TextBufferReader reader(value);

    if (type == text) {
        (*crmout) << " \"";
        while (reader.read()) {
            stm->parse(reader.buffer, reader.size, write_func, crmout, pat_xml_element);
        }
        stm->flush(write_func, crmout);
        (*crmout) << "\"";
    } else {
        U_ASSERT(type == comment || type == pr_ins);

        if (type == comment) { (*crmout) << "(*COMMENT* "; } else { (*crmout) << "(*PI*"; };
        while (reader.read()) {
            stm->parse(reader.buffer, reader.size, write_func, crmout, pat_xml_attribute);
        }
        stm->flush(write_func, crmout);
        if (type == comment) { (*crmout) << ")"; } else { (*crmout) << ")"; }
    };
}

void SXMLSerializer::printDocument(const text_source_t docname, IXDMNode * content)
{
    indentLevel = 0;
    indentNext = true;
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
    (*crmout) << " (";
    if (t.is_atomic_type(xs_boolean)) {
        (*crmout) << (t.get_xs_boolean() ? "#t" : "#f");
    } else {
        (*crmout) << "\"";
        XMLSerializer::printAtomic(t);
        (*crmout) << "\"";
    }
    (*crmout) << ")";
}

void SXMLSerializer::printAttribute(IXDMNode * attribute)
{
    TextBufferReader reader(attribute->getValue());
    (*crmout) << " (";
    attribute->printNodeName(*crmout);
    (*crmout) << " ";
    while (reader.read()) {
        // For backwords compatibility reason quotes in attributes are not escaped
        stm->parse(reader.buffer, reader.size, write_func, crmout, (int) pat_xml_element);
    }
    stm->flush(write_func, crmout);
    (*crmout) << ")";
}

void SXMLSerializer::printElement(IXDMNode * elementInterface)
{
    CHECK_TIMER_FLAG;

    ElementContext * parentContext = this->elementContext;
    ElementContext context = {elementInterface->getLocalName(), elementInterface->getNamespace()};
    elementContext = &context;

    bool hasAttributes = false;

    (*crmout) << " (";
    elementInterface->printNodeName(*crmout);

    /* specially handle default namespace */
    if (context.ns != NULL_XMLNS && !context.ns.has_prefix() && declareNamespace(context.ns)) {
        printNamespace(context.ns);
    }

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


