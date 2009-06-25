
#ifndef CATMEM_H
#define CATMEM_H

#include "common/sedna.h"
#include "common/errdbg/d_printf.h"

#define CATALOG_PERSISTENT_CONTEXT  ((void *) NULL)
#define CATALOG_TEMPORARY_CONTEXT   ((void *) NULL)
#define CATALOG_COMMON_CONTEXT      ((void *) NULL)

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

inline void * cat_realloc(void * p, size_t new_size) { return realloc(p, new_size); };

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
    return cat_malloc(context, size);
//    return (size == 0) ? NULL : malloc(size);
};

inline void * cat_realloc(void * p, size_t new_size) {
    return realloc(p, new_size);
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

inline void * cat_malloc(void * parent, size_t size) {
    return (size == 0) ? NULL : malloc(size);
};

inline void * cat_malloc_context(void * context, size_t size)
{
    return (size == 0) ? NULL : malloc(size);
};

inline void * cat_realloc(void * p, size_t new_size) {
    return realloc(p, new_size);
};

inline void cat_free(void * p) {
    return free(p);
};

#endif /* CATMEM_DEBUG */
#endif /* CATMEM_TRACE */

// Allocates string in the context of parent and copies src to new string
// src MUST be null-terminated.
char * cat_strcpy(void * parent, const char * src);

#define FPA_SIZE 1023

class FastPointerArray {
private :
    struct FastPointerArrayChunk {
        FastPointerArrayChunk * next;
        void * data[FPA_SIZE];
    } * chunks;

    int lastPtr;

public :
    inline void init() {
        chunks = (FastPointerArrayChunk *) cat_malloc_context(CATALOG_COMMON_CONTEXT, sizeof(FastPointerArrayChunk));
        chunks->next = NULL;
    };

    FastPointerArray() : chunks(NULL), lastPtr(0) {
        init();
    };

    inline void _internal_clear() {
        FastPointerArrayChunk * j, * i = chunks;
        while (i != NULL) {
            j = i->next;
            cat_free(i);
            i = j;
        }
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
            FastPointerArrayChunk * newChunk = (FastPointerArrayChunk *) cat_malloc_context(CATALOG_COMMON_CONTEXT, sizeof(FastPointerArrayChunk));
            newChunk->next = chunks;
            chunks = newChunk;
            lastPtr = 0;
        };

        chunks->data[lastPtr++] = ptr;

        return ptr;
    };

    template <class T> void destroyAll() {
        FastPointerArrayChunk * i = chunks;
        int c = lastPtr;

        while (i != NULL) {
            for (int j = 0; j < c; j++) {
                T * tmp = (T *) i->data[j]; tmp->~T(); cat_free(tmp);
            };
            c = FPA_SIZE;
            i = i->next;
        }
    };

    void freeAll() {
        FastPointerArrayChunk * i = chunks;
        int c = lastPtr;

        while (i != NULL) {
            for (int j = 0; j < c; j++) { cat_free(i->data[j]); };
            c = FPA_SIZE;
            i = i->next;
        }
    };
};


#endif /* CATMEM_H */