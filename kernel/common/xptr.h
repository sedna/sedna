/*
 * File:  xptr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __XPTR_H
#define __XPTR_H

#include "common/sedna.h"
#include "common/base.h"

#ifndef NULL
#define NULL_WAS_NOT_DEFINED
#define NULL	0
#endif

// File incapsulates xptr notion. It has been written for 32-bit architecture
// but with the goal to be easyly ported to 64-bit architecture

#define TMP_LAYER_STARTS_WITH	0x40000000
#define INVALID_LAYER			0xFFFFFFFF

// user this macros when you want obtain pointer in terms of process' virtual
// address space
#define XADDR(p)				((p).addr)
#define CONSTRUCT_XPTR(l, a)	(xptr(l, a))
#define BLOCKXPTR(a)			xptr(a.layer,(void*)((__uint32)((a).addr) & PAGE_BIT_MASK))

#define ADDR2XPTR(a)			xptr(*(t_layer*)(((__uint32)(a)) & PAGE_BIT_MASK),					\
                                     (void*)(*(__uint32*)((((__uint32)(a)) & PAGE_BIT_MASK) + sizeof(t_layer)) + (((__uint32)(a)) & PAGE_REVERSE_BIT_MASK)))
#define TEST_XPTR(p)			(*(t_layer*)((__uint32)((p).addr) & PAGE_BIT_MASK) == (p).layer)
#define ALIGN_ADDR(a)			((void*)((__uint32)(a) & PAGE_BIT_MASK))
#define LAYERS_EQUAL(a, p)      (*(t_layer*)((__uint32)(a) & PAGE_BIT_MASK) == ((p).layer))



#define IS_TMP_BLOCK(p)			((p).layer >= TMP_LAYER_STARTS_WITH)
#define IS_DATA_BLOCK(p)		((p).layer <  TMP_LAYER_STARTS_WITH)

#define IS_TMP_BLOCK_LP(ptr)	(((vmm_sm_blk_hdr*)(ptr))->p.layer >= TMP_LAYER_STARTS_WITH)
#define IS_DATA_BLOCK_LP(ptr)	(((vmm_sm_blk_hdr*)(ptr))->p.layer <  TMP_LAYER_STARTS_WITH)

//struct vmm_sm_blk_hdr;

//inline bool isTmpBlock(const void * p) { return ( (const vmm_sm_blk_hdr *) p )->p.layer >= TMP_LAYER_STARTS_WITH; };

/* type for layer */
typedef int t_layer;

union uint64_lh_t { struct uint64_lh { uint32_t l; uint32_t h; } lh; uint64_t v; };

/* Struct for Extended Virtual Address Space Pointer */
struct xptr
{
    t_layer	layer;
    void *	addr;

    xptr() : layer(0), addr(NULL) {}
    xptr(t_layer _layer, void *_addr) : layer(_layer), addr(_addr) {}
    explicit xptr(const uint64_t x) { (* (uint64_t *) this) = x; }

    inline uint64_t to_logical_int() const {
        union uint64_lh_t v = * (uint64_lh_t *) this;
#ifndef BIG_ENDIAN_ORDER
        uint32_t x = v.lh.l;
        v.lh.l = v.lh.h;
        v.lh.h = x;
#endif
        return v.v;
    };

    inline uint64_t to_uint64() const { return * (uint64_t *) this; };

    xptr &operator +=(int n)
	{ 
		addr = (char*)(addr) + n;

		if ((unsigned int)addr >= LAYER_ADDRESS_SPACE_BOUNDARY_INT)
		{
			layer++;
			addr = (void*)((int)addr - LAYER_ADDRESS_SPACE_SIZE);
		}

		return *this; 
	}
    xptr &operator -=(int n) 
    { 
        addr = (char*)(addr) - n; 
        
        if ((unsigned int)addr < LAYER_ADDRESS_SPACE_START_ADDR_INT)
		{
			layer--;
			addr = (void*)((int)addr + LAYER_ADDRESS_SPACE_SIZE);
		}
        
        return *this; 
    }

    void print() const;
    void clear() { layer = 0; addr = NULL; }
};

inline xptr operator+(const xptr &p, int n)
{
    xptr new_p(p);
    new_p.addr = (char*)(p.addr) + n;

    if ((unsigned int)(new_p.addr) >= LAYER_ADDRESS_SPACE_BOUNDARY_INT)
    {
        new_p.layer++;
        new_p.addr = (void*)((int)(new_p.addr) - LAYER_ADDRESS_SPACE_SIZE);
    }

    return new_p;
}

inline xptr operator-(const xptr &p, int n)
{
    xptr new_p(p);
    new_p.addr = (char*)(p.addr) - n;

    if ((unsigned int)(new_p.addr) < LAYER_ADDRESS_SPACE_START_ADDR_INT)
    {
        new_p.layer--;
        new_p.addr = (void*)((int)(new_p.addr) + LAYER_ADDRESS_SPACE_SIZE);
    }

    return new_p;
}

inline __uint32 operator-(const xptr &p1, const xptr &p2)
{
    if (p1.layer != p2.layer) throw USER_EXCEPTION2(SE1003, "Bad paramters in xptr operator-(const xptr &p1, const xptr &p2)");
    return (__uint32)(p1.addr) - (__uint32)(p2.addr);
}

inline bool operator==(const xptr &p1, const xptr &p2)
{
    return p1.addr == p2.addr && (p1.addr == NULL || p1.layer == p2.layer);
}

inline bool operator!=(const xptr &p1, const xptr &p2)
{
    return !(p1 == p2);
}

inline bool operator<(const xptr &p1, const xptr &p2)
{
    return (p1.layer != p2.layer ? p1.layer < p2.layer : (__uint32)(p1.addr) < (__uint32)(p2.addr));
}

inline xptr block_xptr(const xptr &p)
{
    return xptr(p.layer, (void*)((__uint32)(p.addr) & PAGE_BIT_MASK));
}

inline xptr addr2xptr(const void * p)
{
    U_ASSERT(((xptr *) (((uint32_t) p ) & PAGE_BIT_MASK))->addr == (void *) (((uint32_t) p ) & PAGE_BIT_MASK));
    return xptr(* (t_layer*) (((uint32_t) p) & PAGE_BIT_MASK), const_cast<void *>(p));
}

inline void shed_state(xptr &p)
{
    p.layer = 0; 
    p.addr = NULL;
}


inline int xptr_compare(const xptr& p1, const xptr& p2)
{
    if (p1.addr == NULL && p2.addr == NULL) return 0;
    if (p1.addr == NULL) return 1;
    if (p2.addr == NULL) return -1;

    if(p1.layer != p2.layer)
    {
        return p1.layer < p2.layer ? -1 : 1;
    }
    else
    {
        if((__uint32)(p1.addr) < (__uint32)(p2.addr)) return -1;
        else if((__uint32)(p1.addr) == (__uint32)(p2.addr)) return 0;
        return 1;
    }    
}

inline bool same_block(const xptr& a, const xptr& b) {
    return (BLOCKXPTR(a) == BLOCKXPTR(b));
}

inline bool isTmpBlock(const xptr &p) { return p.layer >= TMP_LAYER_STARTS_WITH; };

/* NULL for xpointers */
const xptr XNULL;

#ifdef NULL_WAS_NOT_DEFINED
#undef NULL
#endif

#endif


