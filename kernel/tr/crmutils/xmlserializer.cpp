#include "common/base.h"
#include "common/sedna.h"

#include "tr/crmutils/xmlserializer.h"
#include "tr/strings/strings.h"
#include "tr/structures/portal.h"
#include "tr/structures/nodeutils.h"
#include "tr/crmutils/exec_output.h"
#include "tr/executor/base/PPBase.h"

#include "tr/crmutils/xdm.h"

struct ElementContext {
    const char * name;
    const xmlns_ptr ns;
    xmlns_ptr defaultNamespace;

    ElementContext(const schema_node_cptr node);
};


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
      default : throw USER_EXCEPTION(xxx);
    }
}

void XDMSerializer::printNode(IXDMNode * node)
{
    const t_item t = node->getNodeKind();
    switch (t) {
      case element :
          printElement(&node); break;
      case document :
          printDocument(node->getValue(), &node); break;
      case xml_namespace :
      case attribute :
          printAttribute(&node); break;
      case comment :
      case pr_ins :
      case text :
          printText(t, node->getValue()); break;
      default :
        throw USER_EXCEPTION(xxx);
    }
}



XDMSerializer::serialize(tuple & t) {
    if (t.is_eos()) { return; }

    for (int i = 0; i < t.cells_number; i++) {
        const tuple_cell tc = t.cells[i];

        if (tc.is_portal()) {
          /* Printing portal */
          portal::VirtualNode element(tc);
          printElement(&element);
        } else if (tc.is_node()) {
          /* Printing node */
          printNode(tc.get_node());
        } else {
          /* Printing atomic */
            U_ASSERT(tc.is_atomic());

//            if (!ind && is_atomic) crmout<<" ";
//            is_atomic=true;

            printAtomic(tc);
        }

//        if (ind && i<(tup.cells_number-1)) crmout<<" ,";
    }
}

XMLSerializer::XMLSerializer(dynamic_context * a_cxt, const GlobalSerializationOptions * a_options, StrMatcher * a_stm, se_ostream &a_out)
    : cxt(a_cxt), options(a_options), stm(a_stm), crmout(a_out), elementContext(NULL) { }

void XMLSerializer::printAtomic(const tuple_cell &t)
{
    stm->parse_tc(&t, write_func, &crmout, pat_element);
    stm->flush(write_func, &crmout);
}

static const char * XML_docTagStart = "<?xml version=\"1.0\" standalone=\"yes\"";
static const char * XML_docTagEnd = "?>";

inline static
bool isAttributeAt(IXDMNodeList * list) {
    const IXDMNode * node = list->getNode();
    return (node != NULL && (node->getNodeKind() & ti_first_children) != 0);
}

void XMLSerializer::printDocument(const char * docname, IXDMNode * content)
{
    indentLevel = 0;
    indentNext = true;
    const bool printDocTags = true;

    IXDMNodeList * children = content->getAllChildren();

    if (printDocTags) {
        crmout << XML_docTagStart;

        while (isAttributeAt(children)) {
            printAttribute(children->getNode());
            children->next();
        }

        crmout << XML_docTagEnd;
    }

    while (!children->end()) {
        printNode(children->getNode());
        children->next();
    }
}

inline static
void print_indent(se_ostream& crmout, int level, const char * indent) {
    for (int i=0; i < level; i++) { crmout << indent; }
}

xmlns_ptr XMLSerializer::handleDefaultNamespace(const xmlns_ptr defaultNamespace)
{
    /* Default namespace handling. There are two signs of the default namespace:
     * empty prefix xmlns node and empty prefix in schema node. We output each
     * default namespace only once */

    if (defaultNamespace == elementContext->ns) {
      elementContext->defaultNamespace = defaultNamespace;
    } else {
        elementContext->defaultNamespace = elementContext->ns;
        if (defaultNamespace != NULL_XMLNS && elementContext->ns == NULL_XMLNS) {
            return xmlns_touch("", "");
        } else {
            return elementContext->defaultNamespace;
        }
    }
    return NULL_XMLNS;
}


void XMLSerializer::printElement(IXDMNode * elementInterface)
{
    CHECK_TIMER_FLAG;

    ElementContext * parentContext = this->elementContext;
    ElementContext context(elementInterface);
    elementContext = &context;
    bool indented = indentNext;

    if (indentNext) { crmout << "\n"; print_indent(crmout, indentLevel++, this->options->indentSequence); }

    crmout << "<";
    elementInterface->

    indentNext = options->indent;

    const xmlns_ptr ns = handleDefaultNamespace(parentContext->defaultNamespace);
    if (ns != NULL_XMLNS) { crmout << " "; printNamespace(ns); }

    XDMNodeIterator * childIterator = elementInterface->getChildren();

    childIterator->first();

    if (childIterator->getAttribute() != NULL) do {
        XDMAttribute * attribute = childIterator->getAttribute();
        crmout << " ";
        printAttribute(attribute);
    } while (childIterator->nextAttribute());

    if (childIterator->getNode() == NULL) {
        crmout << "/>";
    } else {
        crmout << ">";

        do {
            printNode(childIterator->getNode());
        } while (childIterator->next());

        if (indented && indentNext) { crmout << "\n"; print_indent(crmout, indentLevel-1, this->options->indentSequence); }
        crmout << "</";
        if (context.ns.has_prefix()) {
            crmout << context.ns.get_prefix() << ":";
        }
        crmout << context.name << ">";
    }

    indentNext = options->indent;
    if (indented) { indentLevel--; }

    elementContext = parentContext;
}

void XMLSerializer::printNamespace(xmlns_ptr ns)
{
    crmout << "xmlns";
    if (ns->has_prefix()) { crmout << ":" << ns->get_prefix(); }
    crmout << "=\"";
    stm->parse(ns->get_uri(), strlen(ns->get_uri()), write_func, &crmout, (int) pat_attribute);
    stm->flush(write_func, &crmout);
    crmout << "\"";
}

void XMLSerializer::printNodeName(xmlns_ptr ns, const char * nodename) {
    if (ns != NULL_XMLNS) {
        crmout << ns.get_prefix() << ":" << nodename;
    } else {
        crmout << nodename;
    }
}

void XMLSerializer::printAttribute(XDMAttribute * attribute)
{
    TextBufferReader reader(attribute->getValue());
    printNodeName(attribute->getNamespace(), attribute->getName());
    crmout << "=\"";
    while (reader.read()) {
        stm->parse(reader.buffer, reader.size, write_func, &crmout, (int) pat_attribute);
    }
    stm->flush(write_func, &crmout);
    crmout << "\"";
}

void XMLSerializer::printText(t_item type, const text_source_t value)
{
    CHECK_TIMER_FLAG;

    TextBufferReader reader(value);

    if (type == text) {
        indentNext = false;
        if (options->cdataSectionElements) {
            crmout << "<![CDATA[";
            // StringMatcher must substitute "]]>" with "]]>]]<![CDATA[<"
            while (reader.read()) {
                stm->parse(reader.buffer, reader.size, write_func, &crmout, pat_cdata);
            }
            stm->flush(write_func, &crmout);
            crmout << "]]>";
        } else {
            while (reader.read()) {
                stm->parse(reader.buffer, reader.size, write_func, &crmout, pat_element);
            }
            stm->flush(write_func, &crmout);
        }
    } else {
        U_ASSERT(type == comment || type == pr_ins);

        if (indentNext) { crmout << "\n"; }
        if (type == comment) { crmout << "<!--"; } else { crmout << "<?"; };
        while (reader.read()) {
            stm->parse(reader.buffer, reader.size, write_func, &crmout, pat_attribute);
        }
        stm->flush(write_func, &crmout);
        if (type == comment) { crmout << "-->"; } else { crmout << "?>"; }
    };
}

void SXMLSerializer::printAttribute(XDMAttribute * attribute)
{
    TextBufferReader reader(attribute->getValue());
    printNodeName(attribute->getNamespace(), attribute->getName());
    while (reader.read()) {
        // For backwords compatibility reason quotes in attributes are not escaped
        stm->parse(reader.buffer, reader.size, write_func, &crmout, (int) pat_element);
    }
    stm->flush(write_func, &crmout);
}

void SXMLSerializer::printElement(XDMElement * elementInterface)
{
    CHECK_TIMER_FLAG;

    ElementContext * parentContext = this->elementContext;
    ElementContext context(elementInterface);
    elementContext = &context;

    crmout << "(";
    printNodeName(context.ns, context.name);
    indentNext = options->indent;

    const xmlns_ptr ns = handleDefaultNamespace(parentContext->defaultNamespace);
    if (ns != NULL_XMLNS) { crmout << " "; printNamespace(ns); }

    XDMNodeIterator * childIterator = elementInterface->getChildren();

    childIterator->first();

    if (childIterator->getAttribute() != NULL) {
        crmout << "(@";
        do {
            XDMAttribute * attribute = childIterator->getAttribute();
            crmout << " ";
            printAttribute(attribute);
        } while (childIterator->nextAttribute());
        crmout << ")";
    }

    if (childIterator->getNode() != NULL) do {
        printNode(childIterator->getNode());
    } while (childIterator->next());

    crmout << ")";

    elementContext = parentContext;
}


