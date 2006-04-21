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

#define se_alloc(sz)    MemoryContextAlloc(CurrentMemoryContext, (sz))

#define se_alloc0(sz)   MemoryContextAllocZero(CurrentMemoryContext, (sz))

/*
 * The result of se_alloc() is always word-aligned, so we can skip testing
 * alignment of the pointer when deciding which MemSet variant to use.
 * Note that this variant does not offer any advantage, and should not be
 * used, unless its "sz" argument is a compile-time constant; therefore, the
 * issue that it evaluates the argument multiple times isn't a problem in
 * practice.
 */
#define se_alloc0fast(sz) \
	( MemSetTest(0, sz) ? \
		MemoryContextAllocZeroAligned(CurrentMemoryContext, sz) : \
		MemoryContextAllocZero(CurrentMemoryContext, sz) )

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

#define se_strdup(str)  MemoryContextStrdup(CurrentMemoryContext, (str))


#ifdef __cplusplus
}
#endif


#ifdef __cplusplus
/*
inline void *operator new(usize_t size)
{
    return MemoryContextAlloc(CurrentMemoryContext, size);
}

inline void *operator new(usize_t size, MemoryContext context)
{
    return MemoryContextAlloc(context, size);
}

inline void operator delete(void* p)
{
    se_free(p);
}

inline void *operator new[](usize_t size)
{
    return MemoryContextAlloc(CurrentMemoryContext, size);
}

inline void *operator new[](usize_t size, MemoryContext context)
{
    return MemoryContextAlloc(context, size);
}

inline void operator delete[](void* p)
{
    se_free(p);
}
*/
#endif /* __cplusplus__ */


#endif   /* SE_ALLOC_H */
