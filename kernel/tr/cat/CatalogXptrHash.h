/*
 * File: CatalogXptrHash.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _FAST_XPTR_HASH_H
#define _FAST_XPTR_HASH_H

#include "common/sedna.h"
#include "common/xptr/xptr.h"

extern uint64_t total_alloc;
extern uint64_t miss_count;
//export uint64_t hits;

class RealHashTable;

class FastCatalogXptrHash {
private :
    RealHashTable * topLayer;
public :
    FastCatalogXptrHash();
    ~FastCatalogXptrHash();

    void * get(const xptr &a) const;
    void set(const xptr &a, void *v);
};


#endif // _FAST_XPTR_HASH_H
