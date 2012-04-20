/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef SCELEMENTPRODUCER_H
#define SCELEMENTPRODUCER_H

#include "tr/structures/producer.h"

class sequence;

text_source_t concatTextSequence(sequence * textSequence);

class SCElementProducer : public IElementProducer
{
  private:
    SCElementProducer * parent;

    static xptr virtualRoot;
    static SCElementProducer * virtualRootProducer;

    t_item node_kind;
    xptr parentNode;
    xptr self;
    xptr left;
    bool noMoreAttributes;

    sequence * textAccum;

    tuple_cell processAtomics();

    SCElementProducer(SCElementProducer* _parent, xptr node);
    SCElementProducer(xptr existent);
  public:
    virtual tuple_cell close();

    virtual bool hasNode(const tuple_cell& node);
    virtual tuple_cell addNode(const tuple_cell& node, bool preserveType);
    virtual tuple_cell addAtomic(const tuple_cell& atomic);

    virtual tuple_cell addAttribute(const xsd::QName& qname, const text_source_t value, xmlscm_type type);
    virtual tuple_cell addAttributeValue(const xsd::QName& qname, const tuple_cell value);

    virtual tuple_cell addComment(const text_source_t value);
    virtual IElementProducer* addElement(const xsd::QName& qname, xmlscm_type type);
    virtual tuple_cell addNS(const xmlns_ptr ns);
    virtual tuple_cell addPI(const xsd::NCName& name, const text_source_t value);
    virtual tuple_cell addText(const text_source_t value);

    virtual ~SCElementProducer();

    Node getNode();

    static void deleteVirtualRoot();
    static SCElementProducer * getVirtualRoot(xptr vr);

    static SCElementProducer * createTemporaryDocument(const xsd::AnyURI& name, dynamic_context * cxt);
};

#endif /* SCELEMENTPRODUCER_H */
