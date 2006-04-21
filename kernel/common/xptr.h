/*
 * File:  xptr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __XPTR_H
#define __XPTR_H

#include "base.h"
#include "exceptions.h"

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

// works only in transaction, does not work is SM
//#define ADDR2XPTR(a)			xptr(*(t_layer*)(((__uint32)(a)) & PAGE_BIT_MASK), (void*)(a))
// this one works both in transaction and SM
//#define ADDR2XPTR(a)			xptr(*(xptr*)(((__uint32)(a)) & PAGE_BIT_MASK) + (((__uint32)(a)) & PAGE_REVERSE_BIT_MASK))
#define ADDR2XPTR(a)			xptr(*(t_layer*)(((__uint32)(a)) & PAGE_BIT_MASK),					\
                                     (void*)(*(__uint32*)((((__uint32)(a)) & PAGE_BIT_MASK) + sizeof(t_layer)) + (((__uint32)(a)) & PAGE_REVERSE_BIT_MASK)))

#define TEST_XPTR(p)			(*(t_layer*)((__uint32)((p).addr) & PAGE_BIT_MASK) == (p).layer)
// old and don't work   !!!
//#define TEST_XPTR(p)			(*(xptr*)((__uint32)((p).addr) & PAGE_BIT_MASK) == (p))


#define ALIGN_ADDR(a)			((void*)((__uint32)(a) & PAGE_BIT_MASK))
// new wave in VMM :)
//#define ALIGN_XADDR(p)			((void*)((__uint32)((p).addr) & PAGE_BIT_MASK))
//#define TEST_XPTR(p, a)			((*(t_layer*)a == (p).layer) && (((xptr*)a)->addr == a))


#define IS_TMP_BLOCK(p)			((p).layer >= TMP_LAYER_STARTS_WITH)
#define IS_DATA_BLOCK(p)		((p).layer <  TMP_LAYER_STARTS_WITH)

#define IS_TMP_BLOCK_LP(ptr)	(((vmm_sm_blk_hdr*)(ptr))->p.layer >= TMP_LAYER_STARTS_WITH)
#define IS_DATA_BLOCK_LP(ptr)	(((vmm_sm_blk_hdr*)(ptr))->p.layer <  TMP_LAYER_STARTS_WITH)



/* type for layer */
typedef int t_layer;

/* Struct for Extended Virtual Address Space Pointer */
struct xptr
{
    t_layer	layer;
    void *	addr;

    xptr() { layer = 0; addr = NULL; }
    xptr(t_layer layer, void *addr) { this->layer = layer; this->addr = addr; }
    xptr(const xptr& x) { layer = x.layer; addr = x.addr; }

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
    xptr &operator -=(int n) { addr = (char*)(addr) - n; return *this; }

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

inline bool operator==(const xptr &p, void* addr)
{
    return p.addr == addr;
}

inline bool operator!=(const xptr &p, void* addr)
{
    return !(p == addr);
}

inline bool operator==(void *addr, const xptr &p)
{
    return addr == p.addr;
}

inline bool operator!=(void *addr, const xptr &p)
{
    return !(addr == p);
}

inline xptr block_xptr(const xptr &p)
{
    return xptr(p.layer, (void*)((__uint32)(p.addr) & PAGE_BIT_MASK));
}

inline void shed_state(xptr &p)
{
    p.layer = 0; 
    p.addr = NULL;
}



/* NULL for xpointers */
extern xptr XNULL;
/* Pointer to empty text string*/
//extern xptr XDUMMY;


#ifdef NULL_WAS_NOT_DEFINED
#undef NULL
#endif

#endif


