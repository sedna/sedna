#include "common/base.h"
#include "common/sedna.h"

#include "tr/crmutils/xmlserializer.h"
#include "tr/strings/strings.h"
#include "tr/structures/portal.h"
#include "tr/structures/nodeutils.h"
#include "tr/crmutils/exec_output.h"
#include "tr/executor/base/PPBase.h"

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





struct XMLElement : public XDMElement {
private:
    mutable schema_node_xptr snode;
    xptr node;
public:
    XMLElement(const xptr parent) {
        CHECKP(parent)
        snode = getSchemaPointer(parent);
        node = getFirstChild(parent);
    };

    virtual schema_node_cptr getSchemaNode() const { return snode; };

    virtual void next(tuple &t) {
        if (node == XNULL) {
            t.set_eos();
        } else {
            t.copy(tuple_cell::node(node));
            CHECKP(node)
            node = nodeGetRightSibling(node);
        };
    };
};

XDMSerializer::printNode(const Node node)
{
    node.checkp();
    t_item t = node.getNodeType();
    switch (t) {
      case element : { XMLElement element = node.getPtr(); printElement(&element); } break;
      case document : { XMLElement element = node.getPtr(); printDocument(text_source_node(node.getPtr()), &element); } break;
      case attribute : { printAttribute(node.getSchemaNode(), text_source_node(node.getPtr())); } break;
      case xml_namespace : { printNamespace(NSNode(node).getNamespaceLocal()); } break;
      case comment : case pr_ins : case text : { printText(t, text_source_node(node.getPtr())); } break;
      default : throw USER_EXCEPTION(xxx);
    }
}


XDMSerializer::serialize(tuple & t) {
    if (t.is_eos()) { return; }

    for (int i = 0; i < t.cells_number; i++) {
        const tuple_cell tc = t.cells[i];

        if (tc.is_portal()) {
          /* Printing portal */
          portal::VirtualElementIterator element(tc);
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

void XMLSerializer::printDocument(const char * docname, XDMElement * content)
{
    indentLevel = 0;
    indentNext = true;
    const bool printDocTags = true;

    if (printDocTags) { crmout << XML_docTagStart; }

    xptr child=getFirstChild(node);
    while (child != XNULL && isNamespaceOrAttribute(child)) {
        this->serialize(tuple(tuple_cell::node(child)));
        child = nodeGetRightSibling(checkp(child));
    }

    if (printDocTags) { crmout << XML_docTagEnd; }

    while (child != XNULL) {
//        SednaElementIterator it(child);
//        printElement(&it);
        child = nodeGetRightSibling(checkp(child));
    }
}

void XMLSerializer::printElement(XDMElement * element)
{

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

void XMLSerializer::printAttribute(schema_node_cptr snode, const text_source_t value)
{
    TextBufferReader reader(value);
    crmout << getTagName(snode).c_str() << "=\"";
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

void SXMLSerializer::printText(t_item type, const text_source_t value)
{
    CHECK_TIMER_FLAG;

    if (type == text) {
        TextBufferReader reader(value);
        while (reader.read()) {
            stm->parse(reader.buffer, reader.size, write_func, &crmout, pat_element);
        }
        stm->flush(write_func, &crmout);
    } else {
        XMLSerializer::printText(type, value);
    };
}


