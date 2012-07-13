#ifndef COMMUTIL_H_INCLUDED
#define COMMUTIL_H_INCLUDED

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#ifndef __cplusplus
#define inline 
#endif /* __cplusplus */

static inline
int strcmpex(const char* c1, const char* c2)
{
    int res;
    if (c1==NULL && c2==NULL) return 0;
    if (c1==NULL || c2==NULL) return (c1==NULL) ? -1 : 1;
    res = strcmp(c1,c2); /// strcmp doesn't guarantee that return value either -1, 1 or 0!
    if (res < 0) return -1;
    if (res > 0) return 1;
    return res;
}

inline static
unsigned int strhash(const char * a) {
    unsigned long hash = 5381;
    int c;

    while ('\0' != (c = (* (unsigned char *) a++))) {
        hash = ((hash << 5) + hash) + c;
    }

    return hash;
};

inline static
unsigned int strnhash(const char * a, size_t len) {
    unsigned long hash = 5381;
    int c;

    while ('\0' != (c = (* (unsigned char *) a++)) && len-- > 0) {
        hash = ((hash << 5) + hash) + c;
    }

    return hash;
};

inline static
int strcmpnull(const char * str1, const char * str2)
{
    if ((str1 == NULL) && (str2 == NULL)) return 0;
    if (str1 == NULL) return -1;
    if (str2 == NULL) return 1;

    return strcmp(str1, str2);
}

/*	The following macroses are defined:

	MIN(A, B)			- minimum
	MAX(A, B)			- maximum
	IS_POWER_OF_2(NUMBER)		- whether a number is power of 2 and > 0
	PTR_DISTANCE(PTRBEGIN, PTREND)	- the length of [PTRBEGIN, PTREND] range, in bytes
	ALIGN_PTR(PTR, ALIGNMENT)	- syn. for RALIGN_PTR(...)
	RALIGN_PTR(PTR, ALIGNMENT)	- make pointer aligned by moving it towards higher addresses
	LALIGN_PTR(PTR, ALIGNMENT)	- make pointer aligned by moving it towards lower addresses
	OFFSET_PTR(PTR, DISTANCE)	- offset pointer by given distance in bytes
	ROUND_SIZE_UP(SIZE, K)		- round size up
	ROUND_SIZE_DOWN(SIZE, K)	- round size down

	Note: ALIGN_* and ROUND_* macros are defined with % (integer remainder) hence
	the alignment is not required to be power of 2. Any sane compiler will replace
	% with bitwize operators when alignment is power of 2 (the most common scenario).
*/

#ifndef MIN
#define MIN(A, B) \
	((A)<(B)?(A):(B))
#endif

#ifndef MAX
#define MAX(A, B) \
	((A)>(B)?(A):(B))
#endif

#define SET_FLAG(f, b) (f) = ((f) & ~(0x1UL << (b))) | (0x1UL << (b))
#define CLEAR_FLAG(f, b) (f) = ((f) & ~(0x1UL << (b)))
#define GET_FLAG(f, b) (((f) >> (b)) & 1)

#define IS_POWER_OF_2(NUMBER) \
	((NUMBER)&((NUMBER)-1)==0 && (NUMBER)>0)

#define PTR_DISTANCE(PTRBEGIN, PTREND) \
	(ptrdiff_t)((uintptr_t)PTREND-(uintptr_t)PTRBEGIN)

#define ALIGN_PTR(PTR, ALIGNMENT) \
	RALIGN_PTR(PTR, ALIGNMENT)

#define RALIGN_PTR(PTR, ALIGNMENT) \
	(void *)((uintptr_t)(PTR)+(ALIGNMENT)-1-((uintptr_t)(PTR)+(ALIGNMENT)-1)%(ALIGNMENT))

#define LALIGN_PTR(PTR, ALIGNMENT) \
	(void *)((uintptr_t)(PTR)-(uintptr_t)(PTR)%(ALIGNMENT))

#define OFFSET_PTR(PTR, OFFSET) \
	(void *)((uintptr_t)(PTR)+(OFFSET))

#define ROUND_SIZE_UP(SIZE, K) \
	((size_t)(SIZE)+(K)-1-((size_t)(SIZE)+(K)-1)%(K))

#define ROUND_SIZE_DOWN(SIZE, K) \
	((size_t)(SIZE)-(size_t)(SIZE)%(K))

#endif
