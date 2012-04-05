/*
 * File:  uatomic.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 *
 * Implementation was partially taken from Hans Boehm's atomic_ops package.
 */

#ifndef ATOMIC_OPS_H
#define ATOMIC_OPS_H


#define AO_INLINE static __inline

/*
 * Read write barriers for compilers.
 * Using them doesn't mean that you don't need something like
 */
#if defined(__GNUC__)
# define AO_compiler_barrier() __asm__ __volatile__("" : : : "memory")
#elif defined(_MSC_VER)
# if defined(_AMD64_) || defined(_M_X64) || _MSC_VER >= 1400
#   if defined(_WIN32_WCE)
/* #     include <cmnintrin.h> */
#   elif defined(_MSC_VER)
#     include <intrin.h>
#   endif
#   pragma intrinsic(_ReadWriteBarrier)
#   define AO_compiler_barrier() _ReadWriteBarrier()
        /* We assume this does not generate a fence instruction.        */
        /* The documentation is a bit unclear.                          */
# else
#   define AO_compiler_barrier() __asm { }
        /* The preceding implementation may be preferable here too.     */
        /* But the documentation warns about VC++ 2003 and earlier.     */
# endif
#else
  /* We conjecture that the following usually gives us the right        */
  /* semantics or an error.                                             */
# define AO_compiler_barrier() asm("")
#endif


/* GCC section                                                          */
#if defined(__GNUC__)

# if defined(__i386__)
    /* We don't define AO_USE_GCC_BUILTINS for x86 here because         */
    /* it might require specifying additional options (like -march)     */
    /* or additional link libraries (if -march is not specified).       */
#   include "u/atomic/gcc/x86.h"
# endif /* __i386__ */

# if defined(__x86_64__)
#   if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 2)
      /* It is safe to use built-in on this architecture.    */
#     define AO_USE_GCC_BUILTINS
#   endif
#   include "u/atomic/gcc/x86_64.h"
# endif /* __x86_64__ */

# if defined(__powerpc__) || defined(__ppc__) || defined(__PPC__) \
     || defined(__powerpc64__) || defined(__ppc64__)
#   include "u/atomic/gcc/powerpc.h"
# endif /* __powerpc__ */

#endif /* __GNUC__ */


/* Microsoft Compiler section                                           */
#if defined(_MSC_VER)
# if defined(_AMD64_) || defined(_M_X64)
#   include "u/atomic/msvc/x86_64.h"
# elif defined(_M_IX86) || defined(x86)
#   include "u/atomic/msvc/x86.h"
# endif
#endif

#endif /* ATOMIC_OPS_H */
