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
    void *search_for_chunk_addr(uint32_t cust_chunk_size, void *addr)
    {
        FastPointerArrayChunk *i = chunks;
        unsigned c = lastPtr;

        while (i != NULL)
        {
            for (unsigned j = 0; j < c; j++)
            {
                uintptr_t chunk_start = (uintptr_t)i->data[j];
                uintptr_t chunk_boundary = chunk_start + cust_chunk_size;

                if ((uintptr_t)addr >= chunk_start && (uintptr_t)addr < chunk_boundary)
                    return i->data[j];
            }

            c = FPA_SIZE;
            i = i->next;
        }

        return NULL;
    }

    void *get_elem(uint32_t ind)
    {
        unsigned i, j;
        FastPointerArrayChunk *chunk = chunks;

        i = chunk_num - ind / FPA_SIZE - 1;
        j = ind % FPA_SIZE;

        U_ASSERT(i <= chunk_num);

        while (i--)
            chunk = chunk->next;

        return chunk->data[j];
    }
};

/*
 * This class represents some type of memory context.
 * That is: it allocates and frees memory by chunks of size TEMP_CHUNK_SIZE.
 */

/* DEF_CHUNK_SIZE defines defsult size of a chunk for context */
#ifdef SEDNA_X64
#define DEF_CHUNK_SIZE 0x64000000 /* 100mb */
#else
#define DEF_CHUNK_SIZE 0xA00000 /* 10mb */
#endif

#define CONTEXT_MAGIC 0xFDB3E6AC

class CatalogMemoryContext
{
    struct chunk_head
    {
        uint32_t magic;
        CatalogMemoryContext *context;
        size_t chunk_num;
    };

    struct alloc_head
    {
        chunk_head *chunk_addr;
    };

    FastPointerArray chunks;
    size_t curr_chunk_offset;
    uint32_t chunk_num;
    void *curr_chunk_addr;
    uint32_t chunk_size;

    int allocate_chunk()
    {
        chunk_head *header;

        if (chunk_num == UINT32_MAX - 1)
            return -1;

        curr_chunk_addr = malloc(chunk_size);

        if (!curr_chunk_addr)
            return -1;

        header = (chunk_head *)curr_chunk_addr;
        header->context = this;
        header->chunk_num = chunk_num++;
        header->magic = CONTEXT_MAGIC;

        chunks.add(curr_chunk_addr);
        curr_chunk_offset = sizeof(struct chunk_head);

        return 0;
    }

    static alloc_head *get_alloc_head(void *addr)
    {
        return (alloc_head *)((char *)addr - sizeof(struct alloc_head));
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

    void *cust_malloc(size_t size)
    {
        char *res = NULL;
        alloc_head *header;

        if (size == 0) return NULL;

        size += sizeof(struct alloc_head);

        if (size > chunk_size)
            return NULL;

        if (chunk_num == 0 && allocate_chunk() != 0) /* first allocation */
            return NULL;

        if (curr_chunk_offset + size > chunk_size && allocate_chunk() != 0)
            return NULL;

        res = (char *)curr_chunk_addr + curr_chunk_offset;
        header = (alloc_head *)res;
        header->chunk_addr = (chunk_head *)curr_chunk_addr;
        res += sizeof(struct alloc_head);
        curr_chunk_offset += size;

        return res;
    }

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
        offs = (char *)addr - (char *)res_chunk;
#else
        offs = (uint32_t)addr;
#endif
        return offs;
    }

    /*
     * Returns chunk number to use for offs2addr mapping
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
            addr = (void *)offs;
        }

        return addr;
    }

    bool is_addr_in_context(void *addr)
    {
        if (addr == NULL)
            return false;

        return (chunks.search_for_chunk_addr(chunk_size, addr) != NULL);
    }

    static CatalogMemoryContext *find_context_for_addr(void *addr)
    {
        return get_chunk_head(addr)->context;
    }
};

/***********************************************
 * Some common catalog contexts.
 *
 * If you want to add something don't forget to
 * edit find_context_for_addr() function
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
 * use for: storing temporary database objects, local catalog objects
 */
extern CatalogMemoryContext *CATALOG_TEMPORARY_CONTEXT;

/*
 * Finds context address belongs to (SLOW DEBUG VERSION)
 * If address belongs to no context returns NULL.
 */
inline
CatalogMemoryContext *find_context_for_addr(void *addr)
{
    if (CATALOG_TEMPORARY_CONTEXT && CATALOG_TEMPORARY_CONTEXT->is_addr_in_context(addr))
        return CATALOG_TEMPORARY_CONTEXT;
    else if (CATALOG_COMMON_CONTEXT && CATALOG_COMMON_CONTEXT->is_addr_in_context(addr))
        return CATALOG_COMMON_CONTEXT;
    else if (CATALOG_PERSISTENT_CONTEXT && CATALOG_PERSISTENT_CONTEXT->is_addr_in_context(addr))
        return CATALOG_PERSISTENT_CONTEXT;

    return NULL;
}

/*****************************
    Catalog memory management
*/

#ifdef CATMEM_TRACE

extern int allocated_objects;
extern int deallocated_objects;

// Allocate  memory in the same context as specified pointer

inline void * _cat_malloc(void * parent, size_t size) {
    allocated_objects++;
    return (size == 0) ? NULL : malloc(size);
};

inline void * _malloc_print_return(const char * s1, int s2, void * parent, size_t size) {
    void * a = _cat_malloc(parent, size);
    d_printf4("{%p alloc on %s:%d}\n", a, s1, s2);
    return a;
}

#define cat_malloc(p, s) _malloc_print_return(__FILE__, __LINE__, p, s)
#define cat_malloc_context(p, s) _malloc_print_return(__FILE__, __LINE__, p, s)

// Allocate memory in the specified context
inline void * _cat_malloc_context(void * context, size_t size) { allocated_objects++; return (size == 0) ? NULL : malloc(size); };

inline void _cat_free(void * p) { deallocated_objects++; return free(p); };

inline void _free_print_return(const char * s1, int s2, void * p) {
    d_printf4("{%p free on %s:%d}\n", p, s1, s2);
    _cat_free(p);
}

#define cat_free(p) _free_print_return(__FILE__, __LINE__, p)


#else /* !CATMEM_TRACE */
#ifdef CATMEM_DEBUG

inline void * cat_malloc(void * parent, size_t size) {
    if (size == 0) return NULL;
    size += 4;
    uint32_t * a = (uint32_t*) malloc(size);
    *(uint32_t*) a = 0xdeadbeaf;
    return (a + 1);

//    return (size == 0) ? NULL : malloc(size);
};

inline void * cat_malloc_context(void * context, size_t size)
{
    if (context)
        return context->cust_malloc(size);
    else
        return cat_malloc(context, size);
//    return (size == 0) ? NULL : malloc(size);
};

inline void _cat_free(void * p) {
    if (p == NULL) return;
    uint32_t * a = (uint32_t*) p;

    a -= 1;
    U_ASSERT(*(uint32_t*) a == 0xdeadbeaf);
    *(uint32_t*) a = 0;

    return free(a);
};

#define cat_free(p) _cat_free(p); (p) = NULL

#else /* !CATMEM_DEBUG */

/*
 * Allocates memory in context. If context is NULL, uses system malloc
 * If allocation isn't successful throws bad_alloc exception
 */
inline void *cat_malloc_context(CatalogMemoryContext *context, size_t size)
{
    void *mem;

    if (size == 0) return NULL;

    if (context)
        mem = context->cust_malloc(size);
    else
        mem = malloc(size);

    if (!mem) throw std::bad_alloc();

    return mem;
}

/*
 * Allocates size bytes in the context of parent.
 * If parents belongs to no context, behaviour is not defined
 */
inline void *cat_malloc(void * parent, size_t size)
{
    CatalogMemoryContext *parent_context = CatalogMemoryContext::find_context_for_addr(parent);

    return cat_malloc_context(parent_context, size);
}

/*
 * cat_free essentially is deprecated since we don't need to free context memory.
 * for now, it is in pretty slow version, which checks for context and either does
 * nothing (addr in some contex) or calls system 'free'
 *
 * should not be used in common code. main purpose is to make a pair to
 * cat_malloc_context(NULL), which calls malloc anyway and thus should not be used.
 */
inline void cat_free(void * p)
{
    /* we don't want to call free for context address since we do bulk free there */
    if (find_context_for_addr(p))
        return;

    return free(p);
}

#endif /* CATMEM_DEBUG */
#endif /* CATMEM_TRACE */

// Allocates string in the context of parent and copies src to new string
// src MUST be null-terminated.
char * cat_strcpy(void * parent, const char * src);

#endif /* CATMEM_H */
