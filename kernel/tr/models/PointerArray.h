#ifndef _POINTER_ARRAY_
#define _POINTER_ARRAY_

#include <stdlib.h>
#include <inttypes.h>

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
    };

    /*
     * Returns element by the given index
     */
    void *get_elem(uint32_t ind)
    {
        unsigned i, j;
        FastPointerArrayChunk *chunk = chunks;

        i = chunk_num - ind / FPA_SIZE - 1;
        j = ind % FPA_SIZE;

        while (i--) {
            chunk = chunk->next;
        }

        return chunk->data[j];
    };
};

#endif /* _POINTER_ARRAY_ */
