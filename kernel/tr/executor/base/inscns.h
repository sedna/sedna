/*
 * File:  inscns.h
 * Copyright (C) 2011 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef INSCNS_H
#define INSCNS_H

#include "tr/structures/nodeinterface.h"
#include "tr/structures/xmlns.h"
#include "INamespaceMap.h"
#include "tr/executor/base/namespaces.h"

class InscopeNamespaceMap;

class InscopeNamespaceIterator : public INamespaceIterator {
  friend class InscopeNamespaceMap;
  private:
    Node node;
    NamespaceMap map;
    xmlns_ptr current;
    INamespaceMap * skn;

    enum { ii_predefined, ii_self, ii_namespaces, ii_attributes, ii_parent } state;

    Node stateNode;

    bool setCurrent(xmlns_ptr p);
  public:
    InscopeNamespaceIterator(Node _node, INamespaceMap * _skn) :
        node(_node), current(NULL_XMLNS), skn(_skn), state(ii_predefined) {  };

    virtual ~InscopeNamespaceIterator() { };

    virtual bool empty() { return node.isNull(); };
    virtual xmlns_ptr get() { return current; };

    virtual bool next();
};

class InscopeNamespaceMap : public INamespaceMap {
  private:
    bool mapReady;
    bool started;
    InscopeNamespaceIterator iterator;

    void buildMap();
  public:
    InscopeNamespaceMap(Node _node, INamespaceMap * _skn) : mapReady(false), started(false), iterator(_node, _skn) {};
    virtual ~InscopeNamespaceMap() {};

    virtual INamespaceIterator* getIterator();
    virtual xmlns_ptr resolvePrefix(const char* prefix);
};


#endif /* INSCNS_H */