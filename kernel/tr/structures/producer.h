/*
 * File:  portal.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef PRODUCER_H_
#define PRODUCER_H_

#include "tr/strings/strings_base.h"

#include "tr/executor/base/xsd.h"
#include "tr/executor/base/tuple.h"
#include "tr/strings/strings.h"
#include "tr/executor/fo/casting_operations.h"

class IElementProducer {
  public:
    virtual ~IElementProducer() {};

    virtual tuple_cell addNS(const xmlns_ptr ns) = 0;
    virtual tuple_cell addAttribute(const xsd::QName &qname, const text_source_t value, xmlscm_type type = xs_untypedAtomic) = 0;
    virtual tuple_cell addText(const text_source_t value) = 0;
    virtual tuple_cell addPI(const xsd::NCName& name, const text_source_t value) = 0;
    virtual tuple_cell addComment(const text_source_t value) = 0;

    virtual tuple_cell addAttributeValue(const xsd::QName &qname, const tuple_cell value) {
        return addAttribute(qname, text_source_tuple_cell(cast(value, xs_string)), value.get_atomic_type());
    };

    virtual IElementProducer * addElement(const xsd::QName &qname, xmlscm_type type = xs_anyType) = 0;

    virtual tuple_cell addNode(const tuple_cell & node, bool preserveType) = 0;
    virtual tuple_cell addAtomic(const tuple_cell & node) = 0;

    virtual bool hasNode(const tuple_cell & node) = 0;

    virtual tuple_cell close() = 0;
};

#endif /* PRODUCER_H_ */
