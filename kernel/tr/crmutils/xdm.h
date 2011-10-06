/*
 * File: xdm.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef XDM_H_
#define XDM_H_

#include "tr/structures/xmlns.h"
#include "tr/strings/strings_base.h"
#include "tr/crmutils/exec_output.h"
#include "tr/executor/base/xsd.h"

/* This is serialization wrapper for xml nodes.
 * One can implement any node represenation, but for it to be serialized well to any output,
 * all representation must provide the following interfaces: IXDMNode, IXDMNodeList
 */

class IXDMNodeList;

struct xdm_node_t {
    t_item kind;
    xsd::QName qname;
    xmlns_ptr ns_value;
    text_source_t text_value;
};

/* interface */ class IXDMNode {
  public:
    virtual ~IXDMNode() {};

    virtual t_item getNodeKind() const = 0;
    virtual xsd::QName getQName() const = 0;
    virtual const text_source_t getValue() const = 0;
    virtual xmlns_ptr getNamespaceValue() const = 0;
    virtual bool hasText() const = 0;

    /* Here "all children" means children and attributes  */
    virtual IXDMNodeList * getAllChildren() = 0;
};

/* interface */ class IXDMNodeList {
  public:
    virtual ~IXDMNodeList() {};
    virtual bool next() = 0;
    virtual bool end() = 0;
    virtual IXDMNode * getNode() = 0;
};

class SednaNodeList;

class SednaNode : public IXDMNode {
  private:
    xptr node;

    mutable schema_node_cptr snode;
    mutable SednaNodeList * childList;
    mutable enum { a_unknown, a_false, a_true } m_hasText;
  public:
    xptr getXptr() const;
    void setNode(xptr node);

    SednaNode(xptr a_node);
    ~SednaNode();

    virtual t_item getNodeKind() const;
    virtual xsd::QName getQName() const;
    virtual const text_source_t getValue() const;
    virtual xmlns_ptr getNamespaceValue() const;
    virtual bool hasText() const;

    virtual IXDMNodeList * getAllChildren();
};

class SednaNodeList : public IXDMNodeList {
  private:
    friend class SednaNode;

    xptr child;
    mutable SednaNode * node;
  public:
    SednaNodeList(xptr firstChild);
    ~SednaNodeList();

    virtual bool next();
    virtual bool end();
    virtual IXDMNode * getNode();
};



#endif /* XDM_H_ */
