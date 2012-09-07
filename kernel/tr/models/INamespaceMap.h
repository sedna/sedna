/*
 * File:  INamespaceMap.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef INAMESPACEMAP_H
#define INAMESPACEMAP_H

#include "tr/structures/xmlns.h"

class INamespaceIterator
{
  public:
    virtual ~INamespaceIterator() {};
    virtual xmlns_ptr get() = 0;
    virtual bool empty() = 0;
    virtual bool next() = 0;
};

class INamespaceMap
{
  public:
    virtual ~INamespaceMap() {};
    virtual xmlns_ptr resolvePrefix(const char * prefix) = 0;
    virtual xmlns_ptr getDefaultNamespace() { return resolvePrefix(""); };

    virtual INamespaceIterator * getIterator() = 0;
};

#endif // INAMESPACEMAP_H
