/*
 * File: xdm.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef XMLSERIALIZER_H_
#define XMLSERIALIZER_H_

#include <tr/structures/xmlns.h>
#include <strings/strings_base.h>
#include <crmutils/exec_output.h>


/* This is serialization wrapper for xml nodes.
 * One can implement any node represenation, but for it to be serialized well to any output,
 * all representation must provide the following interfaces: IXDMNode, IXDMNodeList
 */

class IXDMNodeList;

/* interface */ class IXDMNode {
  public:
    virtual ~IXDMNode() = 0;

    virtual t_item getNodeKind() const = 0;
    virtual const char * getLocalName() const = 0;
    virtual xmlns_ptr getNamespace() const = 0;
    virtual const text_source_t getValue() const = 0;

    virtual void printNodeName(se_ostream & out) const = 0;

    /* Here "all children" means children and attributes  */
    virtual IXDMNodeList * getAllChildren() = 0;
};

/* interface */ class IXDMNodeList {
  public:
    virtual ~IXDMNodeIterator() = 0;
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
  public:
    xptr getXptr() const;
    void setNode(xptr node);

    SednaNode(xptr a_node);
    virtual ~SednaNode();

    virtual t_item getNodeKind() const;
    virtual const char * getLocalName() const;
    virtual xmlns_ptr getNamespace() const;
    virtual const text_source_t getValue() const;

    virtual void printNodeName(se_ostream & out) const;

    virtual IXDMNodeList * getAllChildren();
};

class SednaNodeList : public IXDMNodeList {
  private:
    friend class SednaNode;

    xptr child;
    mutable SednaNode * node;
  public:
    SednaNodeList(xptr firstChild);
    virtual ~SednaNodeList();

    virtual bool next();
    virtual bool end();
    virtual IXDMNode * getNode();
};



#endif /* XDM_H_ */
