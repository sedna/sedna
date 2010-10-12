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

class IXDMNode;

struct dynamic_context;
struct ElementContext;

class XDMSerializer {
  protected:
    void printNode(const Node node);
    void printNode(IXDMNode * node);

    virtual void printAtomic(const tuple_cell &t) = 0;
    virtual void printDocument(const text_source_t docname, IXDMNode * content) = 0;
    virtual void printElement(IXDMNode * element) = 0;
    virtual void printNamespace(xmlns_ptr ns) = 0;
    virtual void printAttribute(IXDMNode * attribute) = 0;
    virtual void printText(t_item type, const text_source_t value) = 0;
  public:
    virtual void serialize(tuple &t);
};

class XMLSerializer : public XDMSerializer {
  protected:
    dynamic_context * cxt;
    const GlobalSerializationOptions * options;
    StrMatcher * stm;
    se_ostream &crmout;

    ElementContext * elementContext;
    bool indentNext;
    int indentLevel;

    xmlns_ptr handleDefaultNamespace(const xmlns_ptr defaultNamespace);

    virtual void printAtomic(const tuple_cell &t);
    virtual void printDocument(const char * docname, IXDMNode * content);
    virtual void printElement(IXDMNode * element);
    virtual void printNamespace(xmlns_ptr ns);
    virtual void printAttribute(IXDMNode * attribute);
    virtual void printText(t_item type, const text_source_t value);
  public:
    XMLSerializer(dynamic_context * a_cxt, const GlobalSerializationOptions * a_options, StrMatcher * a_stm, se_ostream &a_out);
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
    virtual void printDocument(const char * docname, IXDMNode * content);
    virtual void printElement(IXDMNode * element);
    virtual void printAttribute(IXDMNode * attribute);
public:
    SXMLSerializer(dynamic_context * a_cxt, const GlobalSerializationOptions * a_options, StrMatcher * a_stm, se_ostream &a_out);
    virtual ~SXMLSerializer();

    virtual bool supports(enum se_output_method method) { return method == se_output_method_sxml; };
};


#endif /* XMLSERIALIZER_H_ */
