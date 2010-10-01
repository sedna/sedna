/*
 * File: xmlserializer.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef XMLSERIALIZER_H_
#define XMLSERIALIZER_H_

#include "common/base.h"
#include "common/sedna.h"

#include "tr/crmutils/serialization.h"
#include "tr/crmutils/str_matcher.h"

class XDMElement {
    virtual schema_node_cptr getSchemaNode() const = 0;
    virtual void next(tuple &t) = 0;
};

class XDMSerializer {
  protected:
    void printNode(const Node node);
    void traverseVirtualElement(XDMElement * element);
    void traverseElement(ElementNode node);

    virtual void printAtomic(const tuple_cell &t) = 0;
    virtual void printDocument(const text_source_t docname, XDMElement * content) = 0;
    virtual void printElement(XDMElement * element) = 0;
    virtual void printNamespace(xmlns_ptr ns) = 0;
    virtual void printAttribute(schema_node_cptr snode, const text_source_t value) = 0;
    virtual void printText(t_item type, const text_source_t value) = 0;
  public:
    virtual void serialize(tuple &t);
};







struct ElementContext {
    const schema_node_cptr snode;
    const std::string tagName;
    xmlns_ptr defaultNamespace;

    ElementContext(const schema_node_cptr node);
};

struct dynamic_context;

class XMLSerializer : public XDMSerializer {
  protected:
    dynamic_context * cxt;
    const GlobalSerializationOptions * options;
    StrMatcher * smt;
    se_ostream &crmout;

    ElementContext * elementContext;
    bool indentNext;
    int indentLevel;

    virtual void printAtomic(const tuple_cell &t);
    virtual void printDocument(const char * docname, XDMElement * content);
    virtual void printElement(XDMElement * element);
    virtual void printNamespace(xmlns_ptr ns);
    virtual void printAttribute(schema_node_cptr snode, const text_source_t value);
    virtual void printText(t_item type, const text_source_t value);
  public:
    XMLSerializer(dynamic_context * a_cxt, const GlobalSerializationOptions * a_options, StrMatcher * a_smt, se_ostream &a_out);
    virtual ~XMLSerializer();

    virtual bool supports(enum se_output_method method) { return method == se_output_method_xml; };
    virtual bool reusable() { return true; };
    virtual void initialize() { };
};

/** SXML serializer outputs a tuple Scheme list form. This is still needed
 *  for a Sedna scheme driver that is used at least at our testing system.
 */

class SXMLSerializer : public XMLSerializer {
private:
    void printDocument(xptr node);
    void printElement(XMLElementIterator * element);
    void printNamespace(xmlns_ptr ns);
    void printAttribute(xptr node);
    void printText(xptr node);
public:
    SXMLSerializer(dynamic_context * a_cxt, const GlobalSerializationOptions * a_options, StrMatcher * a_smt, se_ostream &a_out);
    ~SXMLSerializer();

    virtual void serialize(tuple & t);

    virtual bool supports(enum se_output_method method) { return method == se_output_method_sxml; };
};


#endif /* XMLSERIALIZER_H_ */
