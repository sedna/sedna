/*
 * File: CatalogXptrHash.cpp 
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "CatalogXptrHash.h"

#include <string.h>
#include <stdio.h>
#include <new>

#include "tr/vmm/vmm.h"
#include "catstore.h"

#define MAGIC_NUMBER 2654435761UL

#define hashSize 1024
//const uint16_t hashSize = 32;

uint64_t total_alloc = 0;
uint64_t miss_count = 0;

inline void * my_malloc(const size_t s) {
//    total_alloc += s;
    return malloc(s);
}

struct SecondLayer {
    const xptr block;
    SecondLayer * next;
    void * values[CS_CELL_COUNT];

    inline uint32_t hash(const xptr &a) const {
        return ((uint32_t) (a.to_logical_int() >> CS_CELL_INS_BITS)) % CS_CELL_COUNT;
    }

    inline SecondLayer(const xptr &a) : block(a), next(NULL) {
        memset(this->values, 0, sizeof(void *) * CS_CELL_COUNT);
    };

    inline ~SecondLayer() { };

    inline void * get(const xptr &a) const {
        return values[hash(a)];
    };

    inline void set(const xptr &a, void * v) {
        values[hash(a)] = v;
    };
};

class RealHashTable {
private:
    SecondLayer * buckets[hashSize];

    inline uint32_t hash(const xptr &a) const {
      return ((uint32_t) ((a.to_logical_int() >> PAGE_BIT_SIZE) & 0xffffffff) * MAGIC_NUMBER) % hashSize;
    }
public :
    inline RealHashTable() {
        memset(this->buckets, 0, sizeof(SecondLayer *) * hashSize);
    };

    inline ~RealHashTable() {
        for (int i = 0; i < hashSize; i++) {
            while (buckets[i] != NULL) {
                SecondLayer * b = buckets[i];
                buckets[i] = b->next;
                b->~SecondLayer();
                free(b);
            }
        }
    };

    inline void * get(const xptr &a) const {
        SecondLayer * b = buckets[hash(a)];
        while ((b != NULL) && (b->block != BLOCKXPTR(a))) { b = b->next; miss_count++; }

        if (b == NULL) { return NULL; }
        else { return b->get(a); }
    };

    inline void set(const xptr &a, void * v) {
        uint32_t bi = hash(a);
        SecondLayer * b = buckets[bi];
        while ((b != NULL) && (b->block != BLOCKXPTR(a))) { b = b->next; miss_count++; }

        if (b == NULL) {

            b = new (my_malloc(sizeof(SecondLayer))) SecondLayer(BLOCKXPTR(a));

            b->next = buckets[bi];
            buckets[bi] = b;
        };

        b->set(a, v);
    };
};


FastCatalogXptrHash::FastCatalogXptrHash() {
    this->topLayer = new (my_malloc(sizeof(RealHashTable))) RealHashTable;
};

FastCatalogXptrHash::~FastCatalogXptrHash() {
    this->topLayer->~RealHashTable();
    free(this->topLayer);
};

void * FastCatalogXptrHash::get(const xptr &a) const
{
    return this->topLayer->get(a);
};

void FastCatalogXptrHash::set(const xptr &a, void *v)
{
    this->topLayer->set(a, v);
};
