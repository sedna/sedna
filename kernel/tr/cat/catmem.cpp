/*
 * File: catmem.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/cat/catmem.h"
#include "tr/cat/catmem.h"

#include <stack>

CatalogMemoryContext *CATALOG_TEMPORARY_CONTEXT = NULL;
CatalogMemoryContext *CATALOG_COMMON_CONTEXT = NULL;

void * local_space_base = NULL;
void * catalog_space_base = NULL;
void * default_context_space = NULL;

static std::stack<void *> contextstack;

void setDefaultSpace(void * space) {
    contextstack.push(default_context_space);
    default_context_space = space;
};

void * popDefaultSpace() {
    default_context_space = contextstack.top();
    contextstack.pop();
    return default_context_space;
};


/* As soon as persistent context is the same as temporary */

#ifdef CATMEM_TRACE
unsigned allocated_objects = 0;
unsigned deallocated_objects = 0;
#endif

#ifdef CATMEM_DEBUG

void *cat_malloc_context_debug(CatalogMemoryContext *context, size_t size, const char *file, unsigned line)
{
    void *res;

    if (!size) return NULL;

    res = cat_malloc_context_(context, (context) ? size : size + 4);

    if (context == NULL)
    {
        *(uint32_t *)res = 0xDEADBEAF;
        res = (char *)res + 4;
    }

    TRACE_CAT_ALLOC(res, file, line);

    return res;
}

void cat_free_debug(void *p, const char *file, unsigned line)
{
    TRACE_CAT_FREE(p, file, line);
    p = (char *)p - 4;
    U_ASSERT(*((uint32_t *)p) == 0xDEADBEAF || *((uint32_t *)p) == CONTEXT_MAGIC);
    *((uint32_t *)p) = 0;
    cat_free_(p);
}

#endif /* CATMEM_DEBUG */

void *cat_malloc_context_(CatalogMemoryContext *context, size_t size)
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

void *cat_malloc(void * parent, size_t size)
{
    CatalogMemoryContext *parent_context = CatalogMemoryContext::find_context_for_addr(parent);

    return cat_malloc_context(parent_context, size);
}

void *cat_malloc_safe(void * parent, size_t size)
{
    CatalogMemoryContext *parent_context = CatalogMemoryContext::find_context_for_addr_safe(parent);

    return cat_malloc_context(parent_context, size);
}

void cat_free_(void *p)
{
    /* we don't want to call free for context address since we do bulk free there */
    if (CatalogMemoryContext::find_context_for_addr(p))
        return;

    free(p);
}

void *FastPointerArray::search_for_chunk_addr(uint32_t cust_chunk_size, void *addr)
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

void *FastPointerArray::get_elem(uint32_t ind)
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

int CatalogMemoryContext::allocate_chunk()
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

void *CatalogMemoryContext::cust_malloc(size_t size)
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
#ifdef CATMEM_DEBUG
    header->magic = CONTEXT_MAGIC;
#endif
    res += sizeof(struct alloc_head);
    curr_chunk_offset += size;

    return res;
}

CatalogMemoryContext *CatalogMemoryContext::find_context_for_addr_safe(void *addr)
{
    if (CATALOG_TEMPORARY_CONTEXT && CATALOG_TEMPORARY_CONTEXT->is_addr_in_context(addr))
        return CATALOG_TEMPORARY_CONTEXT;
    else if (CATALOG_COMMON_CONTEXT && CATALOG_COMMON_CONTEXT->is_addr_in_context(addr))
        return CATALOG_COMMON_CONTEXT;
    /* CATALOG_PERSISTENT_CONTEXT is commented out since for now it equals CATALOG_TEMPORARY_CONTEXT (AK)

        else if (CATALOG_PERSISTENT_CONTEXT && CATALOG_PERSISTENT_CONTEXT->is_addr_in_context(addr))
        return CATALOG_PERSISTENT_CONTEXT;
    */

    return NULL;
}

char * cat_strcpy(void * parent, const char * src)
{
    if (src == NULL) { return NULL; }
    int n = strlen(src) + 1;
    char * dest = (char *) cat_malloc(parent, n);
    return strncpy(dest, src, n);
}

char* cat_strncpy(void* parent, const char* src, size_t len)
{
    if (src == NULL) { return NULL; }
    int n = strnlen(src, len - 1) + 1;
    char * dest = (char *) cat_malloc(parent, n);
    return strncpy(dest, src, n);
}
