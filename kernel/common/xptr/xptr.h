/*
 * File:  xptr.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 *
 * Encapsulates xptr notion which is used as pointer to any data stored in
 * blocks in Sedna files.
 */

#ifndef __XPTR_H
#define __XPTR_H

#include "common/sedna.h"
#include "common/base.h"

#ifndef NULL
#define NULL_WAS_NOT_DEFINED
#define NULL	0
#endif

/* type for layer */
typedef int32_t t_layer;

/* type for layer size
 * Note this type is used also for 'vmm_cur_offs' for which we guarantee
 * atomicity of assignment, memory barrier, etc ...
 * So, change it very carefully. Review logic in vmm signal handler
 * (vmm thread on windows).
 */
typedef uint32_t lsize_t;

// TODO : move operations and boundaries to VMM
/* xptr layer-specific values */
/*
extern void  *LAYER_ADDRESS_SPACE_START_ADDR;
extern void  *LAYER_ADDRESS_SPACE_BOUNDARY;
extern uintptr_t LAYER_ADDRESS_SPACE_START_ADDR_INT;
extern uintptr_t LAYER_ADDRESS_SPACE_BOUNDARY_INT;
extern lsize_t LAYER_ADDRESS_SPACE_SIZE;
*/

#define TMP_LAYER_STARTS_WITH	0x40000000
/* catalog tmp layers go down as: -2, -3, -4, etc. depending on context chunk */
#define TEMPORARY_CATALOG_LAYER_START     -2
#define INVALID_LAYER			0xFFFFFFFF

#define IS_TMP_BLOCK(p)         ((p).layer >= TMP_LAYER_STARTS_WITH)
#define IS_DATA_BLOCK(p)        ((p).layer <  TMP_LAYER_STARTS_WITH)

#define IS_TMP_BLOCK_LP(ptr)    (((vmm_sm_blk_hdr*)(ptr))->p.layer >= TMP_LAYER_STARTS_WITH)
#define IS_DATA_BLOCK_LP(ptr)   (((vmm_sm_blk_hdr*)(ptr))->p.layer <  TMP_LAYER_STARTS_WITH)


/* use this macros when you want obtain pointer in terms of process' virtual address space */
/*
#define XADDR(p)                ((void *)(LAYER_ADDRESS_SPACE_START_ADDR_INT + (p).getOffs()))
#define XADDR_INT(p)            (LAYER_ADDRESS_SPACE_START_ADDR_INT + (p).getOffs())
#define BLOCKXPTR(a)            cxptr(a.layer,((a).getOffs() & PAGE_BIT_MASK))

#define ADDR2XPTR(a)            cxptr(*(t_layer*)(((uintptr_t)(a)) & PAGE_BIT_MASK),   \
                                      *(lsize_t *)((((uintptr_t)(a)) & PAGE_BIT_MASK) + sizeof(t_layer)) + \
                                      (lsize_t)(((uintptr_t)(a)) & PAGE_REVERSE_BIT_MASK))
#define TEST_XPTR(p)            (*(t_layer*)((LAYER_ADDRESS_SPACE_START_ADDR_INT + (p).getOffs()) & PAGE_BIT_MASK) == (p).layer)
#define ALIGN_ADDR(a)           ((void*)((uintptr_t)(a) & PAGE_BIT_MASK))
#define ALIGN_OFFS(a)           ((lsize_t)((lsize_t)(a) & PAGE_BIT_MASK))

#define XOFFS2ADDR(p)           ((void *)(LAYER_ADDRESS_SPACE_START_ADDR_INT + (p)))
#define LAYERS_EQUAL(a, p)      (*(t_layer*)((uintptr_t)(a) & PAGE_BIT_MASK) == ((p).layer))
*/

union uint64_lh_t { struct uint64_lh { uint32_t l; uint32_t h; } lh; uint64_t v; };

/* Struct for Extended Virtual Address Space Pointer */
struct xptr
{
    t_layer	layer;
    lsize_t offs;

    /* Careful! This should be changed if lsize_t becomes > 32bit */
    inline uint64_t to_logical_int() const {
        union uint64_lh_t v = * (uint64_lh_t *) this;
#ifndef BIG_ENDIAN_ORDER
        uint32_t x = v.lh.l;
        v.lh.l = v.lh.h;
        v.lh.h = x;
#endif
        return v.v;
    };
    /* XXX: Careful! This should be changed if lsize_t becomes > 32bit */
    inline void from_logical_int(uint64_t x) const {
        union uint64_lh_t *v = (uint64_lh_t *) &x;
#ifndef BIG_ENDIAN_ORDER
        uint32_t tmp = v->lh.l;
        v->lh.l = v->lh.h;
        v->lh.h = tmp;
#endif
		(* (uint64_t *) this) = v->v;
    };

    inline uint64_t to_uint64() const { return * (uint64_t *) this; };
    inline void from_uint64(const uint64_t x) { (* (uint64_t *) this) = x; };

    inline lsize_t getOffs() const
    {
        return offs;
    }

    inline void setOffs(lsize_t offs_)
    {
        offs = offs_;
    }

    /*
     * Smart inc and dec functions, which can go between layers. Should be
     * used instead of overloaded operators.
     */
//     void inc_offset(unsigned n);
//     void dec_offset(unsigned n);

    /*
     * CAVEAT: since we're using -n here INT_MIN shouldn't work,
     *         maybe others, but on all known platforms only that value.
     *         We could use something like dec_offset(INT_MAX), dec_offset(1)
     *         for this, but it's not 100% portable
     */
    xptr &operator +=(int n)
    {
        if (n >= 0)
            this->inc_offset((unsigned)n);
        else
            this->dec_offset((unsigned)-n);

        return *this;
    }

    /*
     * Same CAVEAT as above applies here too
     */
    xptr &operator -=(int n)
    {
        if (n >= 0)
            this->dec_offset((unsigned)n);
        else
            this->inc_offset((unsigned)-n);

        return *this;
    }

    void print() const;
    void clear() { layer = 0; offs = 0; }
};

inline xptr uint64_to_xptr(const uint64_t x) {
    xptr p;
    p.from_uint64(x);
    return p;
};

inline xptr logical_int_to_xptr(const uint64_t x) {
	xptr p;
	p.from_logical_int(x);
	return p;
};


inline xptr cxptr(t_layer l, lsize_t a) {
    xptr p = {l, a};
    return p;
}

/*
inline
void * xaddr(const xptr p) {
    return XADDR(p);
}
*/

inline xptr operator+(const xptr &p, int n)
{
    xptr new_p(p);

    new_p += n;

    return new_p;
}

inline xptr operator-(const xptr &p, int n)
{
    xptr new_p(p);

    new_p -= n;

    return new_p;
}

inline lsize_t operator-(const xptr &p1, const xptr &p2)
{
    if (p1.layer != p2.layer)
        throw USER_EXCEPTION2(SE1003, "Bad parameters in xptr operator-(const xptr &p1, const xptr &p2)");

    U_ASSERT(p1.getOffs() >= p2.getOffs());

    return p1.getOffs() - p2.getOffs();
}

// TODO: check for legacy logic: (l1, NULL) == (l2, NULL)
inline bool operator==(const xptr &p1, const xptr &p2)
{
    return p1.layer == p2.layer && (p1.layer == 0 || (p1.getOffs() == p2.getOffs()));
}

inline bool operator!=(const xptr &p1, const xptr &p2)
{
    return !(p1 == p2);
}

inline bool operator<(const xptr &p1, const xptr &p2)
{
    return p1.layer != p2.layer ? p1.layer < p2.layer : p1.getOffs() < p2.getOffs();
}

inline xptr block_xptr(const xptr p)
{
    return cxptr(p.layer, (p.getOffs()) & PAGE_BIT_MASK);
}
/*
inline xptr addr2xptr(const void * p)
{
    U_ASSERT(LAYER_ADDRESS_SPACE_START_ADDR_INT + ((xptr *) (((uintptr_t)p) & PAGE_BIT_MASK))->offs ==
             (((uintptr_t) p ) & PAGE_BIT_MASK));
    return cxptr(* (t_layer*) (((uintptr_t) p) & PAGE_BIT_MASK), (lsize_t)(((uintptr_t)p) - LAYER_ADDRESS_SPACE_START_ADDR_INT));
}
*/
inline
xptr block_offset(const xptr block, shft d) {
    return block_xptr(block) + d;
}


// TODO: check for legacy logic: (<any layer>, NULL) >= (any xptr)
inline int xptr_compare(const xptr& p1, const xptr& p2)
{
    if (p1.layer == 0 && p2.layer == 0) return 0;
    if (p1.layer == 0) return 1;
    if (p2.layer == 0) return -1;

    if (p1.layer != p2.layer)
    {
        return p1.layer < p2.layer ? -1 : 1;
    }
    else
    {
        if (p1.getOffs() < p2.getOffs()) return -1;
        else if (p1.getOffs() == p2.getOffs()) return 0;
        return 1;
    }
}

/*
inline bool same_block(const xptr& a, const xptr& b) {
    return (BLOCKXPTR(a) == BLOCKXPTR(b));
}
*/

// inline bool isTmpBlock(const xptr &p) { return p.layer >= TMP_LAYER_STARTS_WITH; };

/* NULL for xpointers */
/* Actually, any xptr with 0-layer equals XNULL    A.K. */
static const xptr XNULL = {};

#ifdef NULL_WAS_NOT_DEFINED
#undef NULL
#endif

struct vmm_region_values
{
    lsize_t LAYER_ADDRESS_SPACE_SIZE;
};

#endif /* __XPTR_H */


