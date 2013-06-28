/*
 * File:  portal.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef PORTAL_H_
#define PORTAL_H_

#include "common/sedna.h"
#include "common/base.h"

#include "tr/structures/schema.h"
#include "tr/executor/base/sequence.h"
#include "tr/crmutils/xdm.h"
#include "tr/structures/producer.h"
#include "tr/strings/strings.h"

#include <vector>
#include <stack>

namespace portal {
class SequenceReader {
private:
    sequence * s;
    int pos;
public:
    SequenceReader(tuple_cell t) : s(t.get_portal().p), pos(t.get_index()) { U_ASSERT(t.get_atomic_type() == se_sequence_element); };

    inline void next() { pos++; };
    inline void get(xqp_tuple &t) { t.copy((*s)[pos]); };
    inline tuple_cell getCell() { return (*s)[pos].cells[0]; };
    inline bool sameSequence(const sequence * t) { return t == s; };
};

/* Virtual node can be iterated only once, so its instance is it's own iteratator */
class VirtualNodeIterator : public IXDMNode, public IXDMNodeList  {
private:
    bool atEnd;
    schema_node_cptr snode;
    counted_ptr<SequenceReader> reader;
    tuple_cell nodeValue;

    scoped_ptr<SednaNode> nodeSedna;
    scoped_ptr<VirtualNodeIterator> nodeVirtual;

    IXDMNode * currentNode;
public:
    explicit VirtualNodeIterator(const tuple_cell t);
    explicit VirtualNodeIterator();
    ~VirtualNodeIterator();

    void setNode(counted_ptr<SequenceReader> newReader, schema_node_cptr parent_snode);

    virtual t_item getNodeKind() const;
    virtual const text_source_t getValue() const;
    virtual xsd::QName getQName() const;
    virtual xmlns_ptr getNamespaceValue() const;
    virtual bool hasText() const;

    virtual IXDMNodeList * getAllChildren();

    virtual bool next();
    virtual bool end();

    virtual IXDMNode * getNode();
};

class VirtualElementGenerator : public IElementProducer {
private:
    std::stack<sequence *> tmp_sequence;
    schema_node_cptr virtualRoot;
public:
    VirtualElementGenerator(schema_node_cptr _virtualRoot);
    virtual ~VirtualElementGenerator();

    void returnSequence(sequence * s) { tmp_sequence.push(s); };

    virtual tuple_cell addAtomic(const tuple_cell& node);
    virtual tuple_cell addAttribute(const xsd::QName& qname, const text_source_t value, xmlscm_type type);
    virtual tuple_cell addComment(const text_source_t value);
    virtual IElementProducer* addElement(const xsd::QName& qname, xmlscm_type type);
    virtual tuple_cell addNode(const tuple_cell& node, bool preserveType);
    virtual tuple_cell addNS(const xmlns_ptr ns);
    virtual tuple_cell addPI(const xsd::NCName& name, const text_source_t value);
    virtual tuple_cell addText(const text_source_t value);
    virtual tuple_cell close();
    virtual bool hasNode(const tuple_cell& node);
};

class VirtualElementProducer : public IElementProducer {
private:
    bool noMoreAttributes;
    VirtualElementGenerator * sequenceReturnStack;
    int index;
    bool opened, closed;

    dynamic_context * cxt;
    sequence* seq;
    sequence textAccum;

    schema_node_cptr snode;
public:
    VirtualElementProducer(const schema_node_xptr _snode, sequence * _seq, VirtualElementGenerator * owner);
    ~VirtualElementProducer();

    tuple_cell addTupleCell(const tuple_cell& tc);
    void processAtomics();

    virtual tuple_cell addAtomic(const tuple_cell& node);
    virtual tuple_cell addAttribute(const xsd::QName& qname, const text_source_t value, xmlscm_type type);
    virtual tuple_cell addComment(const text_source_t value);
    virtual IElementProducer* addElement(const xsd::QName& qname, xmlscm_type type);
    virtual tuple_cell addNode(const tuple_cell& node, bool preserveType);
    virtual tuple_cell addNS(const xmlns_ptr ns);
    virtual tuple_cell addPI(const xsd::NCName& name, const text_source_t value);
    virtual tuple_cell addText(const text_source_t value);
    virtual tuple_cell close();
    virtual bool hasNode(const tuple_cell& node);
};
}

#endif /* PORTAL_H_ */
