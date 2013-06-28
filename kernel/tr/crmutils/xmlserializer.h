/*
 * File: xmlserializer.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef XMLSERIALIZER_H_
#define XMLSERIALIZER_H_

#include "common/base.h"
#include "common/sedna.h"

#include "tr/crmutils/xdm.h"
#include "tr/crmutils/global_options.h"
#include "tr/crmutils/serialization.h"
#include "tr/crmutils/str_matcher.h"

#include <map>
#include <stack>

struct ElementContext;
class StrMatcher;

enum pat_class
{
    pat_attribute = 1,
    pat_element = 2,
    pat_cdata = 4,
    pat_text = 8,
    pat_charmap = 64
};

class XDMSerializer : public Serializer {
  public:
    typedef std::map<std::string, xmlns_ptr> NSPrefixMap;
    typedef std::pair<xmlns_ptr, xmlns_ptr> NSSubsitutionPair;
    typedef std::stack< std::pair<xmlns_ptr, xmlns_ptr> > NSNamespaceStack;
    typedef std::map<xmlns_ptr, xmlns_ptr> NSSwizzlingMap;
  private:
    NSPrefixMap nsPrefixMap;
    NSNamespaceStack nsNamespaceStack;
  protected:
    NSSwizzlingMap nsSwizzlingMap;

    bool separatorNeeded;
    bool savedCData;

    /*  declareNamespace appears in traversing the element subtree if element
      has or implies any namespace declaration. If namespace is unknown or swizzeled
      function returns true. */
    bool declareNamespace(xmlns_ptr ns);

    /* returns default namespace */
    xmlns_ptr getDefaultNamespace();

    /* undeclareNamespaces undeclares "count" namespaces from stack */
    void undeclareNamespaces(int count);

    void printColonizedQName(const xsd::QName & qname);
public:
    void printNode(const Node node);
    void printNode(IXDMNode * node);

    virtual void printAtomic(const tuple_cell &t) = 0;
    virtual void printDocument(const text_source_t docname, IXDMNode * content) = 0;
    virtual void printElement(IXDMNode * element) = 0;
    virtual void printNamespace(xmlns_ptr ns) = 0;
    virtual void printAttribute(IXDMNode * attribute) = 0;
    virtual void printText(t_item type, const text_source_t value) = 0;

    virtual void printElementName(IXDMNode * element) = 0;

    XDMSerializer();

    virtual void serialize(xqp_tuple &t);
};

class XMLSerializer : public XDMSerializer {
  protected:
    bool indentNext;
    int indentLevel;
    bool indentElements;
    int useCharmapFlag;

    char * indentCache;
    const char* indentSequence;
    size_t indentSequenceLength;

    const char * docPISeqOpen;
    const char * docPISeqClose;
    const char * openTagSeq;
    const char * closeTagSeq;

    ElementContext * elementContext;
    StrMatcher stringFilter;

    /* Dummy constructor, used only by heir classes */
    inline XMLSerializer(int dummy) : indentCache(NULL) {};
  public:
    /* Default constructor, it do implements some stuff, i.e. stringFilter initialization */
    XMLSerializer();
    inline ~XMLSerializer() { free(indentCache); };

    virtual void printAtomic(const tuple_cell &t);
    virtual void printDocument(const text_source_t docname, IXDMNode * content);
    virtual void printElement(IXDMNode * element);
    virtual void printNamespace(xmlns_ptr ns);
    virtual void printAttribute(IXDMNode * attribute);
    virtual void printText(t_item type, const text_source_t value);

    virtual void printElementName(IXDMNode * element);

    virtual bool supports(enum se_output_method method) { return method == se_output_method_xml; };
    virtual void initialize();
};

/** SXML serializer outputs a tuple Scheme list form. This is still needed
 *  for a Sedna scheme driver that is used at least at our testing system.
 */

class SXMLSerializer : public XMLSerializer {
  protected:
    /* Dummy constructor, used only by heir classes */
    inline SXMLSerializer(int dummy) {};
  public:
    SXMLSerializer();
    inline ~SXMLSerializer() {};

    virtual void printText(t_item type, const text_source_t value);
    virtual void printAtomic(const tuple_cell &t);
    virtual void printDocument(const text_source_t docname, IXDMNode * content);
    virtual void printElement(IXDMNode * element);
    virtual void printAttribute(IXDMNode * attribute);

    virtual bool supports(enum se_output_method method) { return method == se_output_method_sxml; };
};

#endif /* XMLSERIALIZER_H_ */
