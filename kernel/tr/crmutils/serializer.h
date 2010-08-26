/*
 * File: serializer.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SERIALIZER_H_
#define SERIALIZER_H_

#include "common/sedna.h"
#include "common/base.h"

#include "tr/strings/strings_base.h"
#include "tr/crmutils/serialization.h"
#include "tr/executor/base/dynamic_context.h"

#include <set>
#include <vector>

struct ElementContext {
    const schema_node_cptr snode;
    const std::string tagName;
    xmlns_ptr defaultNamespace;

    ElementContext(xptr node);
};

class XMLSerializer : public Serializer {
private:
    se_ostream &crmout;
    bool tagStarted;
    bool hasContent;
    const char * finishing;
    dynamic_context * cxt;
    std::set<xmlns_ptr> definedNamespaces;
    ElementContext * elementContext;

    static const char * indentSequence = "  ";

    void finishTag();
public:
    XMLSerializer(dynamic_context * a_cxt, se_ostream &a_crmout);
    ~XMLSerializer();

    virtual void serialize(xptr node);
    virtual void serializeTuple(tuple * t);

    void printElement(xptr node);
    void printNamespace(xmlns_ptr ns);
};

class SXMLOutput : public XMLOutput {
public:
    virtual void reset();
    virtual void tuple(const tuple_cell &tc);
    virtual void startDocument();
    virtual void endDocument();
    virtual void * startElement(const schema_node_cptr kind);
    virtual void endElement(void *);
    virtual void attribute(const schema_node_cptr kind, const text_source_t value);
    virtual void text(const text_source_t value , void * markup);
    virtual void comment(const text_source_t value);
    virtual void pi(const text_source_t value, int target_offset);
    virtual void xmlnamespace(const xmlns_ptr ns);
};


#endif /* SERIALIZER_H_ */
