/*
 * File: catmem.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef CATMEM_H
#define CATMEM_H

#include "common/sedna.h"
#include "common/errdbg/d_printf.h"

/*
 * Catalog memory auxiliary classes
 */

/* Number of pointers in one chunk */
#define FPA_SIZE 1023

class FastPointerArray {
private :
    struct FastPointerArrayChunk {
        FastPointerArrayChunk * next;
        void * data[FPA_SIZE];
    } * chunks;

    unsigned lastPtr, chunk_num;

public :
    inline void init() {
        chunks = (FastPointerArrayChunk *) malloc(sizeof(FastPointerArrayChunk));
        chunks->next = NULL;
        chunk_num = 1;
    };

    FastPointerArray() : chunks(NULL), lastPtr(0), chunk_num(0) {
        init();
    };

    inline void _internal_clear() {
        FastPointerArrayChunk * j, * i = chunks;
        while (i != NULL) {
            j = i->next;
            free(i);
            i = j;
        }

        chunk_num = 0;
    }

    inline void clear() {
        _internal_clear();
        init();
        lastPtr = 0;
    };

    ~FastPointerArray() {
        _internal_clear();
    };

    inline void * add(void * ptr) {
        if (lastPtr == FPA_SIZE) {
            FastPointerArrayChunk * newChunk = (FastPointerArrayChunk *)malloc(sizeof(FastPointerArrayChunk));
            newChunk->next = chunks;
            chunks = newChunk;
            lastPtr = 0;
            chunk_num++;
        };

        chunks->data[lastPtr++] = ptr;

        return ptr;
    };


    /*
     * Treats every address as type T, calls the destructor of T and free_func
     */
    template <class T> void destroyAll(void (*free_func)(void *)) {
        FastPointerArrayChunk * i = chunks;
        unsigned c = lastPtr;

        while (i != NULL) {
            for (unsigned j = 0; j < c; j++) {
                T * tmp = (T *) i->data[j]; tmp->~T();

                if (free_func)
                    free_func(tmp);
            }

            c = FPA_SIZE;
            i = i->next;
        }
    };

    /*
     * Treats every address as a chunk of memory and calls free_func
     */
    void freeAll(void (*free_func)(void *)) {
        FastPointerArrayChunk * i = chunks;
        unsigned c = lastPtr;

        while (i != NULL) {
            for (unsigned j = 0; j < c; j++) { free_func(i->data[j]); };
            c = FPA_SIZE;
            i = i->next;
        }
    };

    /*
     * Treats every address as a start of a chunk of size cust_chunk_size and
     * returns addr of the chunk that includes it
     */
    void *search_for_chunk_addr(uint32_t cust_chunk_size, void *addr);

    /*
     * Returns element by the given index
     */
    void *get_elem(uint32_t ind);
};

/* DEF_CHUNK_SIZE defines defsult size of a chunk for context */
#ifdef SEDNA_X64
/* 
 * On x64 we could probably ask for more but I don't see it right now
 * Anyway, since context is multi-chunk it'll work anyway
 * In case of performance problems feel free to increase the value (AK)
 */
#define DEF_CHUNK_SIZE 0xA00000 /* 10mb */
#else
#define DEF_CHUNK_SIZE 0xA00000 /* 10mb */
#endif

/* Context verify stamp */
#define CONTEXT_MAGIC 0xC0C0C0C0

/*
  These are two variables, that can be used as root for contexts
*/

extern void * local_space_base;
extern void * catalog_space_base;

/*
 * This class represents some type of memory context.
 * That is: it allocates and frees memory by chunks of size TEMP_CHUNK_SIZE.
 *
 * In a nutshell: context contains several chunks. Each chunk consists of chunk_head, and continuous
 *                area of allocations (as in malloc). Each allocation is of 'size' bytes (malloc(size))
 *                plus alloc_head in the begining.
 */
class CatalogMemoryContext
{
    struct chunk_head
    {
        uint32_t magic;
        CatalogMemoryContext *context;
        uint32_t chunk_num;
    };

    struct alloc_head
    {
        chunk_head *chunk_addr;
#ifdef CATMEM_DEBUG
        uint32_t magic;
#endif
    };

    FastPointerArray chunks;
    size_t curr_chunk_offset;
    uint32_t chunk_num;
    void *curr_chunk_addr;
    uint32_t chunk_size;

    int allocate_chunk();

    static alloc_head *get_alloc_head(void *addr)
    {
        alloc_head *res = (alloc_head *)((char *)addr - sizeof(struct alloc_head));
#ifdef CATMEM_DEBUG
        U_ASSERT(res->magic == CONTEXT_MAGIC);
#endif
        return res;
    }

    static chunk_head *get_chunk_head(void *addr)
    {
        alloc_head *header = get_alloc_head(addr);

        U_ASSERT(header->chunk_addr->magic == CONTEXT_MAGIC);

        return header->chunk_addr;
    }

public:
    CatalogMemoryContext(uint32_t chunk_size_) : curr_chunk_offset(0), chunk_num(0),
        curr_chunk_addr(NULL), chunk_size(chunk_size_)
    {
    }

    ~CatalogMemoryContext()
    {
        chunks.freeAll(free);
    }

    void *cust_malloc(size_t size);

    /*
     * Implements somewhat hackish mapping addr<->offs.
     * Needed since we want to represent an address
     * in uint32_t form for Sedna's xptr.
     */
    uint32_t addr2offs(void *addr)
    {
        uint32_t offs = 0;

#ifdef SEDNA_X64
        void *res_chunk;
        res_chunk = get_chunk_head(addr);
        offs = (uint32_t)((char *)addr - (char *)res_chunk);
#else
        offs = (uint32_t)addr;
#endif
        return offs;
    }

    /*
     * Returns chunk number to use for offs2addr mapping
     * Chunks are numbered from 1, 0 represents non-existent chunk
     */
    uint32_t addr2chunk(void *addr)
    {
        uint32_t res_chunk = 0;

#ifdef SEDNA_X64
        chunk_head *header;

        header = get_chunk_head(addr);
        res_chunk = header->chunk_num + 1;
#endif
        return res_chunk;
    }

    void *offs2addr(uint32_t chunk, uint32_t offs)
    {
        void *addr, *chunk_addr;

        if (chunk)
        {
            chunk_addr = chunks.get_elem(chunk - 1);
            addr = (char *)chunk_addr + offs;
        }
        else
        {
            addr = (void *)(uintptr_t)offs;
        }

        return addr;
    }

    /*
     * Function determines if diven addr lies in the context.
     * It doesn't make any assumptions: it just searches each chunk
     * for the given addr.
     *
     * NOTE: could be somewhat slow if multiple chunks are involved
     */
    bool is_addr_in_context(void *addr)
    {
        if (addr == NULL)
            return false;

        return (chunks.search_for_chunk_addr(chunk_size, addr) != NULL);
    }

    /*
     * Finds context for the given address. It makes assumption
     * about structure of the context. Thus it works only for
     * addresses returned by cust_malloc.
     *
     * If you want the answer for ANY address, then use _safe function instead
     */
    static CatalogMemoryContext *find_context_for_addr(void *addr)
    {
        return get_chunk_head(addr)->context;
    }

    /*
     * Somewhat safe implementation of the function above. It doesn't
     * make any assumptions about context infrastructure. It just
     * searches any known contexts for the address.
     */
    static CatalogMemoryContext *find_context_for_addr_safe(void *addr);
};

/***********************************************
 * Some common catalog contexts.
 *
 * If you want to add something don't forget to
 * edit find_context_for_addr_safe() function
 *
 ***********************************************/

/*
 * context for persistent data: should be shared between transactions, t-o-l: transaction
 * use for: storing persistent database data
 * persistent context equals temporary for now
 */
#define CATALOG_PERSISTENT_CONTEXT  CATALOG_TEMPORARY_CONTEXT

/*
 * context for temporary data: not shared, t-o-l: session
 * use for: storing some cache-hash common data that lives all session
 */
extern CatalogMemoryContext *CATALOG_COMMON_CONTEXT;

/* context for temporary data: not shared, t-o-l: transaction
 * use for: storing temporary database objects (e.g. constructed data), local catalog objects
 */
extern CatalogMemoryContext *CATALOG_TEMPORARY_CONTEXT;

/*****************************
    Catalog memory management
*/

/*
 * Catalog debug and trace facilities
 */

#ifdef CATMEM_TRACE

extern unsigned allocated_objects;
extern unsigned deallocated_objects;

#define TRACE_CAT_ALLOC(addr, file, line) \
    do \
    { \
        allocated_objects++; \
        d_printf4("{%p alloc on %s:%d}\n", (addr), file, line); \
    } while(0)

#define TRACE_CAT_FREE(addr, file, line) \
    do \
    { \
        deallocated_objects++; \
        d_printf4("{%p free on %s:%d}\n", (addr), file, line); \
    } while(0)

#else /* CATMEM_TRACE */

#define TRACE_CAT_ALLOC(addr, file, line)
#define TRACE_CAT_FREE(addr, file, line)

#endif /* CATMEM_TRACE */

#ifdef CATMEM_DEBUG

void *cat_malloc_context_debug(CatalogMemoryContext *context, size_t size, const char *file, unsigned line);
#define cat_malloc_context(cont, size) cat_malloc_context_debug(cont, size, __FILE__, __LINE__)

void cat_free_debug(void *p, const char *file, unsigned line);
#define cat_free(addr) \
    do \
    { \
        cat_free_debug(addr, __FILE__, __LINE__); \
        addr = NULL; \
    } while(0)

#else /* CATMEM_DEBUG */

#define cat_malloc_context(cont, size) cat_malloc_context_(cont, size)
#define cat_free(addr) cat_free_(addr)

#endif /* CATMEM_DEBUG */

/*
 * Allocates memory in context. If context is NULL, uses system malloc
 * If allocation isn't successful throws bad_alloc exception
 */
void *cat_malloc_context_(CatalogMemoryContext *context, size_t size);

/*
 * Allocates size bytes in the context of parent.
 * If parents belongs to no context, behaviour is not defined
 */
void *cat_malloc(void * parent, size_t size);

/*
 * Somewhat safer but more slow implementation of the above.
 * 'Safer' because it finds context for sure. But it is done
 * by trading off speed. Should be tolerable, if contexts consists
 * of small number of chunk.
 * Should be mainly used if addr is not alligned on the boundary of a chunk
 *
 * NOTE: if 'addr' doesn't belong to any context system malloc will be used instead
 */
void *cat_malloc_safe(void * parent, size_t size);

/*
 * cat_free essentially is deprecated since we don't need to free context memory.
 * for now, it is in somewhat slow version, which checks for context and either does
 * nothing (addr in some contex) or calls system 'free'
 *
 * should not be used in common code too lightly. main purpose is to make a pair to
 * cat_malloc_safe, which calls malloc if parent doesn't belong to any context.
 */
void cat_free_(void * p);

// Allocates string in the context of parent and copies src to new string
// src MUST be null-terminated.
char * cat_strcpy(void * parent, const char * src);

#endif /* CATMEM_H */
