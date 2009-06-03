
#include "common/FastXptrHash.h"

#define MAGIC_NUMBER 2654435761UL
#define CS_CELL_COUNT 256
#define CS_CELL_SIN_BITS 8

#define hashSize 1024
//const uint16_t hashSize = 32;

struct SecondLayer {
    SecondLayer * next;
    const xptr block;
    void * values[CS_CELL_COUNT];

    inline uint32_t hash(const xptr &a) {
        return (uint32_t) ((a.to_uint64() >> CS_CELL_SIN_BITS) & CS_CELL_COUNT);
    }

    inline SecondLayer(const xptr &a) : block(a), next(NULL) {
        memset(&(this->values), 0, sizeof(void *) * CS_CELL_COUNT);
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

    inline uint32_t hash(const xptr &a) {
      return ((uint32_t) ((a.to_uint64() >> PAGE_BIT_SIZE) & 0xffffffff) * MAGIC_NUMBER) % hashSize;
    }
public :
    inline RealHashTable() {
        memset(&(this->buckets), 0, sizeof(SecondLayer *) * hashSize);
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
        while ((b != NULL) && (b->block != BLOCKXPTR(a))) { b = b->next; }

        if (b == NULL) { return NULL; }
        else { return b->get(a); }
    };

    inline void set(const xptr &a, void * v) {
        uint32_t bi = hash(a);
        SecondLayer * b = buckets[bi];
        while ((b != NULL) && (b->block != BLOCKXPTR(a))) { b = b->next; }

        if (b == NULL) {

            b = new (malloc(sizeof(SecondLayer))) RealHashTable;

            b->next = buckets[bi];
            buckets[bi] = b;
        };

        b->set(a, v);
    };
};


FastCatalogXptrHash::FastCatalogXptrHash() {
    this->topLayer = new (malloc(sizeof(RealHashTable))) RealHashTable(BLOCKXPTR(a));
};

FastCatalogXptrHash::~FastCatalogXptrHash() {
    this->topLayer::~RealHashTable();
    free(this->topLayer);
};

void * FastCatalogXptrHash::get(const xptr &a) const
{
    return this->topLayer->get(a);
};

void FastCatalogXptrHash::set(const xptr &a, const void *v)
{
    this->topLayer->set(a, v);
};
