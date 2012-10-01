/*
 * File: catstore.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/xptr/sm_vmm_data.h"

#include "tr/cat/catstore.h"
#include "tr/vmm/vmm.h"
#include "tr/cat/catalog.h"
#include "tr/tr_globals.h"

#define SBA_SIZE ((int) CS_CELL_BITMAP_SIZE)
#include "tr/cat/smallbitarray.h"

// # define DEBUG_FIELD
// # define DEBUG_MEMCPY

#ifdef DEBUG_MEMCPY
inline void __debug_check_bounds(char * aaddr, cs_size_t asize, void * wdest, size_t wsize)
{
    uint32_t astart = (uint32_t) aaddr;
    uint32_t aend = astart + asize;
    uint32_t wstart = (uint32_t) wdest;
    uint32_t wend = astart + wsize;

    U_ASSERT((aend & 0xff) == 0);
    U_ASSERT(wstart >= astart);
    U_ASSERT(aend >= wend);
}
#endif

static xptr safe_block = XNULL;
static xptr hint_ptr = XNULL;
static bool safe_block_write = false;

typedef uint16_t cell_t;

struct cat_blk_hdr {
    vmm_sm_blk_hdr sm_vmm;
    xptr next_block;
    uint32_t free_space[CS_CELL_BITMAP_SIZE];
};

struct cs_chain_part_header {
    xptr addr;       // Absolute subsequence address
    char * eff_addr; // Pointer to data
    cs_size_t size;  // Size of data
    xptr next;       // Pointer to the next subsequence
};

struct cs_chain_header {
#ifdef DEBUG_FIELD
    uint32_t __debug_marker;
#endif
    cs_size_t full_size;
};

struct cs_inblock_part_descriptor {
#ifdef DEBUG_FIELD
    uint32_t __debug_marker;
#endif
    uint16_t count;
    uint64_t next_ptr;
};

int __debug_blockstack_level = 0;

void cs_initp()
{
    safe_block = XNULL;
    safe_block_write = false;
}

void cs_pushp()
{
    U_ASSERT(__debug_blockstack_level++ == 0);
    U_ASSERT(safe_block == XNULL);
    safe_block = vmm_cur_xptr;

    if (safe_block != XNULL) {
        vmm_sm_blk_hdr * bh = (vmm_sm_blk_hdr *) XADDR(BLOCKXPTR(safe_block));
        safe_block_write = bh->is_changed && bh->trid_wr_access == tr_globals::sid;
    }
}

void cs_popp()
{
    U_ASSERT(__debug_blockstack_level-- == 1);
    if (safe_block != XNULL) {
        CHECKP(safe_block);
        if (safe_block_write) { VMM_SIGNAL_MODIFICATION(safe_block); }
        safe_block = XNULL;
    }
}

static void cs_read(const xptr &p, char * data, size_t data_size);

xptr cs_new_block()
{
    xptr r;
    vmm_alloc_data_block(&r);
    CHECKP(r);
    VMM_SIGNAL_MODIFICATION(r);
    memset((char *) XADDR(r) + sizeof(vmm_sm_blk_hdr), 0, CS_CELL_SIZE - sizeof(vmm_sm_blk_hdr));
#ifdef DEBUG_FIELD
    memset((char *) XADDR(r) + sizeof(vmm_sm_blk_hdr), 0, PAGE_SIZE - sizeof(vmm_sm_blk_hdr));
#endif // DEBUG_FIELD
    sba_occupy((struct bit_array *) ((cat_blk_hdr *) XADDR(r))->free_space, 0, 1);
    return r;
}

inline void cs_unoccupy(const xptr b, cell_t cell, cell_t cell_count)
{
    // TODO : this function extremally needs optimization! It's very ram-coded.

    struct bit_array * sba = (struct bit_array *) ((cat_blk_hdr *) XADDR(b))->free_space;

    sba_unoccupy(sba, cell, cell_count);

    if (sba->a[0] != 0x1UL) { return; }
    for (int i = 1; i < SBA_SIZE; i++) { if (sba->a[i] != 0) { return; } }

    vmm_delete_block(b);
}

inline cell_t cs_find_and_occupy(xptr &b, cell_t &cell_count)
{
    // TODO : this function extremally needs optimization! It's very ram-coded.

    cat_blk_hdr * block;
    int cell = 0;
    int cells_found = 0;

    // First cell is always occupied

    while (cell < 1) {
        CHECKP(b);
        block = (cat_blk_hdr *) XADDR(b);
        cells_found = cell_count;
        cell = sba_find((struct bit_array *) block->free_space, &cells_found);

        if (cell > 0) {
//            char * p = (char *) XADDR(b) + cell * CS_CELL_SIZE;
            VMM_SIGNAL_MODIFICATION(b);
            sba_occupy((struct bit_array *) block->free_space, cell, cell_count);
        } else if (block->next_block == XNULL) {
            xptr nb = cs_new_block();
            CHECKP(b);
            VMM_SIGNAL_MODIFICATION(b);
            block->next_block = nb;
            b = nb;
        } else {
            b = block->next_block;
        }
    }

    cell_count = cells_found;
    return cell;
}

/*
 * cs_read_subsequence_descriptor(ptr, ph)
 *
 * Function reads subsequence header and forms the subsequence descriptor from it
 * ptr subsequence pointer (NOT an entrie sequence pointer)
 * ph  output parameter for a subsequence descriptor
 *
 * TODO: error handling
 */

void cs_read_subsequence_descriptor(const xptr ptr, cs_chain_part_header * ph)
{
    char * p = (char *) XADDR(ptr);
    char * pp = p;
    cs_inblock_part_descriptor desc;
    cell_t cell;

    CHECKP(ptr);

    memcpy(&(desc), pp, sizeof(cs_inblock_part_descriptor));
    pp += sizeof(cs_inblock_part_descriptor);

    ph->addr = ptr;
    ph->eff_addr = pp;
    cell = ((uintptr_t) p & PAGE_REVERSE_BIT_MASK) / CS_CELL_SIZE;
    ph->size = ((char *) XADDR(BLOCKXPTR(ptr)) + (cell + desc.count) * CS_CELL_SIZE) - pp;
    U_ASSERT((((uintptr_t) ph->eff_addr + ph->size) & 0xff) == 0);
    U_ASSERT(ph->size < 64000);
    ph->next = uint64_to_xptr(desc.next_ptr);
}



/*
 *  cs_allocate_sequence(b, ch, ph)
 *
 *  Allocates new storage sequence of size ch.full_size (or less).
 *  b is pointer to any place in the parent sequence. b can not be XNULL
 *  ch.fullsize is size of the sequence needed. If there is no such continuous sequence,
 *  allocated one may be shorter. Anyway, write function will find the place to
 *  write the rest of data. Fullsize is ALWAYS actual data length.
 *  ph is output parameter, identifying subsequence to write to.
 *
 *  Function returns xptr to the new sequence. This is persistent sequence identifier.
 */

xptr cs_allocate_sequence(xptr b, cs_chain_header * ch, cs_chain_part_header * ph)
{
    U_ASSERT(b != XNULL);

    cs_size_t total_size = ch->full_size + sizeof(cs_chain_header) + sizeof(cs_inblock_part_descriptor);
    cell_t cell_count = (total_size - 1) / CS_CELL_SIZE + 1;

    /* Find the sequence of the given size, b may be changed */
    cell_t cell_index = cs_find_and_occupy(b, cell_count);
    xptr result = b + CS_CELL_SIZE * cell_index;
    cs_inblock_part_descriptor desc;

    /* Write the sequence header */
    CHECKP(b); VMM_SIGNAL_MODIFICATION(b);
#ifdef DEBUG_FIELD
    ch->__debug_marker = 0xDEADBEAFUL;
#endif
    memcpy(XADDR(result), ch, sizeof(cs_chain_header));

    /* Calculating the subsequence address */
    ph->addr = result + sizeof(cs_chain_header);

    /* Write the subsequence header */
#ifdef DEBUG_FIELD
    desc.__debug_marker = 0xBEAFDEADUL;
#endif
    desc.count = cell_count;
    desc.next_ptr = 0;
    memcpy(XADDR(ph->addr), &desc, sizeof(cs_inblock_part_descriptor));

    /* Fill the subsequence descriptor - actually we reread it to make the code a bit simplier */
    cs_read_subsequence_descriptor(ph->addr, ph);

    return result;
}

void cs_allocate_subsequence(cs_size_t size_needed, cs_chain_part_header * ph)
{
    /* Frist of all, save old descriptor */
    cs_chain_part_header old_ph = *ph;
    xptr b = BLOCKXPTR(ph->addr);
    cs_inblock_part_descriptor desc;
    xptr new_subsequence_ptr;

    /* Calculate the cell number needed to store the data of given size */
    cell_t cell_count = (size_needed + sizeof(cs_inblock_part_descriptor) - 1) / CS_CELL_SIZE + 1;
    cell_t new_cell;

    /* Find the cell chain of given size or smaller one */
    new_cell = cs_find_and_occupy(b, cell_count);
    new_subsequence_ptr = b + new_cell * CS_CELL_SIZE;
#ifdef DEBUG_FIELD
    U_ASSERT(* (uint32_t *) XADDR(new_subsequence_ptr) != 0xDEADBEAFUL);
#endif

    /* Update the old subsequence's header */
    CHECKP(old_ph.addr); VMM_SIGNAL_MODIFICATION(old_ph.addr);
    memcpy(&desc, XADDR(old_ph.addr), sizeof(cs_inblock_part_descriptor));
    desc.next_ptr = new_subsequence_ptr.to_uint64();
    memcpy(XADDR(old_ph.addr), &desc, sizeof(cs_inblock_part_descriptor));

    /* Fill the new subsequence's header */
    CHECKP(new_subsequence_ptr); VMM_SIGNAL_MODIFICATION(new_subsequence_ptr);
#ifdef DEBUG_FIELD
    desc.__debug_marker = 0xBEAFDEADUL;
#endif
    desc.count = cell_count;
    desc.next_ptr = 0;
    memcpy(XADDR(new_subsequence_ptr), &desc, sizeof(cs_inblock_part_descriptor));

    /* Reread the new subsequence's header */
    cs_read_subsequence_descriptor(new_subsequence_ptr, ph);
}

/*
 * cs_read_sequence_header(p, ch, ph)
 *
 * Function reads sequence header and forms the sequence descriptor and
 * subsequence descriptor from it.
 * ptr subsequence pointer (NOT an entrie sequence pointer)
 * ch  output parameter for the sequence descriptor
 * ph  output parameter for the subsequence descriptor
 * TODO: error handling
 */

inline void cs_read_sequence_header(xptr ptr, cs_chain_header * ch, cs_chain_part_header * ph)
{
    CHECKP(ptr);
    memcpy(ch, XADDR(ptr), sizeof(cs_chain_header));
#ifdef DEBUG_FIELD
    U_ASSERT(ch->__debug_marker == 0xDEADBEAFUL);
#endif
    ptr += sizeof(cs_chain_header);
    cs_read_subsequence_descriptor(ptr, ph);
}


void cs_cutoff(cs_chain_part_header * ph)
{
    // TODO TODO TODO
}

void cs_write(xptr &p, const char * data, cs_size_t data_size)
{
 /* This function doesn't return any error code, because
  * any error occurs in this function is SYSTEM ERROR and
  * any activity may be stopped.
  */

    cs_chain_header chain_header = {0};
    cs_chain_part_header ph = {XNULL};
    char * di = (char *) data;
    cs_size_t data_left = data_size;
    cs_size_t to_write;

 /* If p is XNULL, we need to allocate new storage sequence
  * that contains up to data_size number of bytes
  */
    if (p == XNULL) {
        xptr block;

        /* hint_ptr is very important parameter, that tells the united block
         * sequence to put the object to. Actually the united block sequence
         * is a storage space dedicated only to some lock unit (document/collection)
         */

        if (hint_ptr == XNULL) {
            block = cs_new_block();
        } else {
            block = BLOCKXPTR(hint_ptr);
        }

        /* Find free space */
        chain_header.full_size = data_size;
        p = cs_allocate_sequence(block, &chain_header, &ph);
    }
    /* If p is given, we read the header of the sequence at that address */
    else {
        cs_read_sequence_header(p, &chain_header, &ph);
    }

 /* Write the new length of the sequence if changed */
    if (chain_header.full_size != data_size) {
        CHECKP(p); VMM_SIGNAL_MODIFICATION(p);
        chain_header.full_size = data_size;
        memcpy(XADDR(p), &chain_header, sizeof(chain_header));
    }

 /* Write data to the sequence */
    while (data_left > 0) {
        U_ASSERT(ph.addr != XNULL);

        CHECKP(ph.addr); VMM_SIGNAL_MODIFICATION(ph.addr);
        to_write = MIN(data_left, ph.size);

#ifdef DEBUG_MEMCPY
        __debug_check_bounds(ph.eff_addr, ph.size, ph.eff_addr, to_write);
#endif

        memcpy(ph.eff_addr, di, to_write);
        data_left -= to_write;
        di += to_write;

        if (data_left > 0) {
            if (ph.next == XNULL) {
                cs_allocate_subsequence(data_left, &ph);
            } else {
                cs_read_subsequence_descriptor(ph.next, &ph);
            }
        } else break;
    }

 /* Free unused chunks at the end of sequence (TODO) */

    if ((data_left <= 0) && (ph.next != XNULL)) {
        cs_cutoff(&ph);
    }

    return;
}

static void cs_read(const xptr &p, char * data, size_t data_size)
{
    cs_chain_header ch;
    cs_chain_part_header ph = {XNULL};
    char * di = data;
    cs_size_t data_left = data_size;
    cs_size_t to_read;

    U_ASSERT(p != XNULL);
    U_ASSERT(!IS_CATALOG_TMP_PTR(p));
    cs_read_sequence_header(p, &ch, &ph);
    U_ASSERT(ch.full_size >= data_size); // throw

    while (data_left > 0) {
        U_ASSERT(ph.addr != XNULL);

        CHECKP(ph.addr);
        to_read = MIN(ph.size, data_left);
        memcpy(di, ph.eff_addr, to_read);
        di += to_read;
        data_left -= to_read;

        if (data_left > 0) {
            U_ASSERT(ph.next != XNULL);
            cs_read_subsequence_descriptor(ph.next, &ph);
        } else break;
    };

    return;
}

void cs_set_hint(const xptr p)
{
    hint_ptr = p;
}

int       cs_get_magic(const xptr p)
{
    int magic;
    cs_read(p, (char *) &magic, sizeof(int));
    return magic;
}

cs_size_t cs_get_size(const xptr p)
{
    cs_chain_header ch;
    cs_chain_part_header ph;

    cs_read_sequence_header(p, &ch, &ph);

    return ch.full_size;
}

void cs_write_part(const xptr p, const char * data, off_t data_offset, size_t data_size)
{
// TODO
}

void cs_read_part(const xptr p, char * data, off_t data_offset, size_t data_size)
{
// TODO
}

void cs_free(xptr p)
{
    U_ASSERT(p != XNULL);
    if (IS_CATALOG_TMP_PTR(p)) { return ; }

    cs_chain_header ch;
    cs_chain_part_header ph;
    cs_size_t data_left;
    cell_t cell, cell_count;

    cs_read_sequence_header(p, &ch, &ph);
    data_left = ch.full_size;

    while (data_left > 0) {
        cell = (ph.addr - BLOCKXPTR(ph.addr)) / CS_CELL_SIZE;
        cell_count = ((ph.size - 1) / CS_CELL_SIZE) + 1;
        data_left -= MIN(ph.size, data_left);

        if (data_left > 0) {
            U_ASSERT(ph.next != XNULL);
            cs_read_subsequence_descriptor(ph.next, &ph);
        };

        CHECKP(ph.addr);
        VMM_SIGNAL_MODIFICATION(ph.addr);
        cs_unoccupy(BLOCKXPTR(ph.addr), cell, cell_count);
    };
}

/*
void cs_check_consistency(const xptr &p)
{
    cat_blk_hdr blk =

    for (int i = 0; i < SBA_SIZE; i++) {
        c = (struct bit_array *)
        for (int j = 0; j < 32; j++) {
            c >>= 1;
        }
    }
}
*/


cs_stream::cs_stream(const xptr _p, enum mode mode) : p(_p)
{
    _mode = mode;
    cs_size_t s;
    if (_mode == mode_read) {
        s = cs_get_size(_p);
        this->set_length(s);
        cs_read(_p, (char *) this->m_content, s);
    };
}


cs_stream::~cs_stream()
{
}


void cs_stream::commit()
{
    if (_mode == mode_write) {
        cs_write(p, (char *) this->m_content, this->m_length);
    }
}

