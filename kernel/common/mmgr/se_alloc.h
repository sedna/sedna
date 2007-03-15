/*
 * File: se_alloc.h
 *	  Memory allocator definitions.
 *
 * Portions Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * Portions Copyright (c) 1996-2005, PostgreSQL Global Development Group
 * Portions Copyright (c) 1994, Regents of the University of California
 *
 * This file contains the basic memory allocation interface that is
 * needed by almost every module. Keep it lean!
 *
 * Memory allocation occurs within "contexts".	Every chunk obtained from
 * se_alloc()/MemoryContextAlloc() is allocated within a specific context.
 * The entire contents of a context can be freed easily and quickly by
 * resetting or deleting the context --- this is both faster and less
 * prone to memory-leakage bugs than releasing chunks individually.
 * We organize contexts into context trees to allow fine-grain control
 * over chunk lifetime while preserving the certainty that we will free
 * everything that should be freed.  See utils/mmgr/README for more info.
 *
 */

#ifndef SE_ALLOC_H
#define SE_ALLOC_H

/// #define SE_MEMORY_MNG
/// #define SE_MEMORY_TRACK

#ifdef SE_MEMORY_TRACK

#ifdef __cplusplus
extern "C" {
#endif
              
extern char MT_SEDNA_DATA[];
extern void *track_malloc(usize_t size, const char* file, int line, const char* flag);
extern void track_free(void *pointer);
extern void *track_realloc(void *pointer, usize_t size, const char* file, int line, const char* flag);
extern void *track_calloc(usize_t num, usize_t size, const char* file, int line, const char* flag);
extern char *track_strdup(const char *source, const char* file, int line, const char* flag);
extern void DumpUnfreed(int component);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

inline void *operator new(usize_t size, const char *file, int line, const char* flag)
{
    return track_malloc(size, file, line, flag);
}
inline void operator delete(void* p, const char *file, int line, const char* flag)
{
    track_free(p);
}
inline void operator delete(void* p)
{
    track_free(p);
}
inline void *operator new[](usize_t size, const char *file, int line, const char* flag)
{
    return track_malloc(size, file, line, flag);
}
inline void operator delete[](void* p, const char *file, int line, const char* flag)
{
    track_free(p);
}
inline void operator delete[](void* p)
{
    track_free(p);
}

#define se_new                     new(__FILE__, __LINE__, "SE_NEW")
#define se_new_cxt(cxt)            new(__FILE__, __LINE__, "SE_NEW_CXT")

#endif

#define se_delete(p)               delete(p)
#define se_free(p)                 track_free(p)
#define se_alloc(size)             track_malloc(size, __FILE__, __LINE__, "SE_ALLOC")
#define se_realloc(pointer, size)  track_realloc(pointer, size, __FILE__, __LINE__, "SE_REALLOC")

#define malloc(size)               track_malloc(size, __FILE__, __LINE__, "MALLOC")
#define free(pointer)              track_free(pointer)
#define realloc(pointer, size)     track_realloc(pointer, size, __FILE__, __LINE__, "REALLOC")
#define calloc(num, size)          track_calloc(num, size, __FILE__, __LINE__, "CALLOC")
#define strdup(source)             track_strdup(source, __FILE__, __LINE__, "STRDUP")

#else /* SE_MEMORY_TRACK */

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Type MemoryContextData is declared in nodes/memnodes.h.	Most users
 * of memory allocation should just treat it as an abstract type, so we
 * do not provide the struct contents here.
 */
typedef struct MemoryContextData *MemoryContext;

/*
 * CurrentMemoryContext is the default allocation context for se_alloc().
 * We declare it here so that se_alloc() can be a macro.	Avoid accessing it
 * directly!  Instead, use MemoryContextSwitchTo() to change the setting.
 */
extern MemoryContext CurrentMemoryContext;

/*
 * Fundamental memory-allocation operations (more are in utils/memutils.h)
 */
extern void *MemoryContextAlloc(MemoryContext context, usize_t size);
extern void *MemoryContextAllocZero(MemoryContext context, usize_t size);
extern void *MemoryContextAllocZeroAligned(MemoryContext context, usize_t size);

#ifdef SE_MEMORY_MNG
#define se_alloc(sz)    MemoryContextAlloc(CurrentMemoryContext, (sz))

#define se_alloc0(sz)   MemoryContextAllocZero(CurrentMemoryContext, (sz))
#else
#define se_alloc        malloc
#endif

/*
 * The result of se_alloc() is always word-aligned, so we can skip testing
 * alignment of the pointer when deciding which MemSet variant to use.
 * Note that this variant does not offer any advantage, and should not be
 * used, unless its "sz" argument is a compile-time constant; therefore, the
 * issue that it evaluates the argument multiple times isn't a problem in
 * practice.
 */
#ifdef SE_MEMORY_MNG
#define se_alloc0fast(sz) \
	( MemSetTest(0, sz) ? \
		MemoryContextAllocZeroAligned(CurrentMemoryContext, sz) : \
		MemoryContextAllocZero(CurrentMemoryContext, sz) )
#endif


extern void se_free(void *pointer);

extern void *se_realloc(void *pointer, usize_t size);

/*
 * MemoryContextSwitchTo can't be a macro in standard C compilers.
 * But we can make it an inline function when using GCC.
 */
#ifdef __GNUC__
static __inline__ MemoryContext
MemoryContextSwitchTo(MemoryContext context)
{
	MemoryContext old = CurrentMemoryContext;

	CurrentMemoryContext = context;
	return old;
}
#else
extern MemoryContext MemoryContextSwitchTo(MemoryContext context);
#endif   /* __GNUC__ */

/*
 * These are like standard strdup() except the copied string is
 * allocated in a context, not with malloc().
 */
extern char *MemoryContextStrdup(MemoryContext context, const char *string);

#ifdef SE_MEMORY_MNG
#define se_strdup(str)  MemoryContextStrdup(CurrentMemoryContext, (str))
#else
#define se_strdup       strdup
#endif

#ifdef __cplusplus
}
#endif


#ifdef __cplusplus

/*
 * AF:
 * We can overload global new and global delete to use our Context
 * memory allocation routines. But for this moment we prefer not to
 * do that but rather to define placement new and placement delete
 * (with parameter MemoryContext). Placement delete is implemented
 * by the template function se_destroy, which calls destructor 
 * explicitly.
 * To conclude, STL routines are using default new and delete operators
 * (we can count on them that they do not have memory leaks). For
 * Sedna code we should use se_new and se_new_cxt macroses to allocate
 * memory and se_delete to deallocate memory. Note that se_new and 
 * se_new_cxt call constructor and se_delete calls destructor correctly.
 * Note that we still could call global new and delete operators
 * that are not overloaded.
 *
 * Example:
 * A *a = NULL;
 * int *i = NULL;
 *
 * // our new operator in CurrentMemoryContext
 * a = se_new A(some_parameters ...); 
 * se_delete(a); // destructor will be called
 *
 * // our new operator in TopMemoryContext
 * a = se_new_cxt(TopMemoryContext) A(some_parameters ...);
 * se_delete(a); // destructor will be called
 *
 * // Simple types are also supported correctly
 * i = se_new int;
 * se_delete(i);
 */

/* !!! Uncomment this if you want to replace global new and global
       delete.

inline void *operator new(usize_t size)
{
    return operator_new_global(size);
}

inline void operator delete(void* p)
{
    if (p) se_free(p);
}

inline void *operator new[](usize_t size)
{
    return operator_new_global(size);
}

inline void operator delete[](void* p)
{
    if (p) se_free(p);
}
*/

/*
 * Inline new and delete operators rely on these definitions.
 * 
 */
extern "C" int SafeMemoryContextInit(void);
extern "C" MemoryContext TopMemoryContext;



inline void *operator new(usize_t size, MemoryContext context)
{
#ifdef SE_MEMORY_MNG
    if (SafeMemoryContextInit()) context = TopMemoryContext;
    return MemoryContextAlloc(context, size);
#else
    return malloc(size);
#endif
}

inline void operator delete(void* p, MemoryContext context)
{
#ifdef SE_MEMORY_MNG
    if (p) se_free(p);
#else
    if (p) free(p);
#endif
}

inline void *operator new[](usize_t size, MemoryContext context)
{
#ifdef SE_MEMORY_MNG
    if (SafeMemoryContextInit()) context = TopMemoryContext;
    return MemoryContextAlloc(context, size);
#else
    return malloc(size);
#endif
}

inline void operator delete[](void* p, MemoryContext context)
{
#ifdef SE_MEMORY_MNG
    if (p) se_free(p);
#else
    if (p) free(p);
#endif
}

template<class T> void __se_delete(T* p, MemoryContext context)
{
    if (p) 
    {
        p->~T();
#ifdef SE_MEMORY_MNG
        se_free(p);
#else
        free(p);
#endif
    }
}

// FIXME: remove this when Sedna memory manager will be ready (AF)
#ifdef SE_MEMORY_MNG
inline void operator delete(void* p)
{
}
inline void operator delete[](void* p)
{
}
#endif


#ifdef SE_MEMORY_MNG
#define se_new                  new(CurrentMemoryContext)
#define se_new_cxt(cxt)         new(cxt)
#define se_delete(p)            __se_delete(p, NULL)
#else
#define se_new                  new
#define se_new_cxt(cxt)         new
#define se_delete(p)            delete(p);
#endif

#endif   /* __cplusplus__   */
#endif   /* SE_MEMORY_TRACK */
#endif   /* SE_ALLOC_H      */
