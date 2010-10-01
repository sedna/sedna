#include "tr/crmutils/xmlserializer.h"
#include "tr/strings/strings.h"
#include "tr/structures/portal.h"
#include "tr/structures/nodeutils.h"

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


XMLSerializer::XMLSerializer(dynamic_context * a_cxt, const GlobalSerializationOptions * a_options, StrMatcher * a_smt, se_ostream &a_out)
    : cxt(a_cxt), options(a_options), smt(a_smt), crmout(a_out), elementContext(NULL) { }


void XMLSerializer::printAtomic(const tuple_cell &t)
{}

static const char * XML_docTagStart = "<?xml version=\"1.0\" standalone=\"yes\"";
static const char * XML_docTagEnd = "?>";

void XMLSerializer::printDocument(const char * docname, XDMElement * content)
{
    indentLevel = 0;
    indentNext = true;
    const bool printDocTags = false;

    if (printDocTags) { crmout << XML_docTagStart; }

    xptr child=getFirstChild(node);
    while (child != XNULL && isNamespaceOrAttribute(child)) {
        this->serialize(tuple(tuple_cell::node(child)));
        child = nodeGetRightSibling(checkp(child));
    }

    if (printDocTags) { crmout << XML_docTagEnd; }

    while (child != XNULL) {
        SednaElementIterator it(child);
        printElement(&it);
        child = nodeGetRightSibling(checkp(child));
    }
}

void XMLSerializer::printElement(XDMElement * element)
{
}

void XMLSerializer::printNamespace(xmlns_ptr ns)
{
}

void XMLSerializer::printAttribute(schema_node_cptr snode, const text_source_t value)
{
}

void XMLSerializer::printText(t_item type, const text_source_t value)
{
}
