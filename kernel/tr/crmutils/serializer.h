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

    ElementContext(const schema_node_cptr node);
};

struct ElementChildIterator {
    virtual schema_node_cptr getSchemaNode() const = 0;
    virtual void next(tuple &t) = 0;
};

struct SerializationOptions {
    bool preserveNamespaces;
    const char * indentSequence;
    bool indent;
    bool cdataSectionElements;
};

class XMLSerializer : public Serializer {
private:
    dynamic_context * cxt;
    se_ostream &crmout;
    const SerializationOptions * options;
    ElementContext * elementContext;
    bool indentNext;
    int indentLevel;

    void finishTag();
public:
    XMLSerializer(dynamic_context * a_cxt, se_ostream &a_crmout);
    ~XMLSerializer();

    virtual void serialize(xptr node);
    virtual void serializeTuple(tuple * t);

    void printDocument(xptr node);
    void printElement(ElementChildIterator * element);
    void printNamespace(xmlns_ptr ns);
    void printAttribute(xptr node);
};


#endif /* SERIALIZER_H_ */
