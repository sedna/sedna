/*
 * File:  e_string.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/strings/e_string.h"
#include "tr/executor/base/PPBase.h"
#include "tr/vmm/vmm.h"
#include "tr/pstr/pstr_long.h"



//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

namespace tr_globals
{

estr estr_global;

}

/// TODO: change recursion to iteration


void estr::init()
{
    U_ASSERT(first_blk == XNULL);
    U_ASSERT(last_blk == XNULL);

	vmm_alloc_tmp_block(&last_blk);
	e_str_blk_hdr::init(XADDR(last_blk), XNULL);
	first_blk = last_blk;
    m_blks = 1;
}

void estr::reset()
{
    CHECKP(first_blk);
	VMM_SIGNAL_MODIFICATION(first_blk);
    ((e_str_blk_hdr*)XADDR(first_blk))->cursor = sizeof(e_str_blk_hdr);


    m_size = 0;
    last_blk = first_blk;
	m_blks = 0;
}

void estr::truncate(const xptr &ptr)
{
	last_blk = BLOCKXPTR(ptr);
	CHECKP(last_blk);
	VMM_SIGNAL_MODIFICATION(last_blk);
	((e_str_blk_hdr*)XADDR(last_blk))->cursor = ptr - last_blk;


	m_size = 0;
}

xptr estr::append_mstr(const char *src)
{
    xptr dest = xptr_for_data();
    copy_text_mstr(dest, src);
    return dest;
}

xptr estr::append_mstr(const char *src, int count)
{
    xptr dest = xptr_for_data();
    copy_text_mstr(dest, src, count);
    return dest;
}


xptr estr::append_estr(const xptr &src, str_off_t count)
{
    xptr dest = xptr_for_data();
    estr_cursor cur(src, count);
    copy_text_cursor(dest, &cur);
    return dest;
}

xptr estr::append_pstr_long(const xptr &src)
{
    xptr dest = xptr_for_data();
    pstr_long_cursor cur(src);
    copy_text_cursor(dest, &cur);
    return dest;
}

//allocates block if needed
//pre: CHECKP(estr_blk)
//post: CHECKP(estr_blk), for the new value of estr_blk
static inline void estr_get_next_blk(xptr &estr_blk, int &m_blks)
{
	++m_blks;
	if (E_STR_BLK_HDR(estr_blk)->nblk == XNULL)
	{
		xptr new_blk;
		vmm_alloc_tmp_block(&new_blk);
		e_str_blk_hdr::init(XADDR(new_blk), estr_blk);
		CHECKP(estr_blk);
		VMM_SIGNAL_MODIFICATION(estr_blk);
		E_STR_BLK_HDR(estr_blk)->nblk = new_blk;

		estr_blk = new_blk;
		CHECKP(estr_blk);
	}
	else
	{
		estr_blk = E_STR_BLK_HDR(estr_blk)->nblk;
		CHECKP(estr_blk);
		VMM_SIGNAL_MODIFICATION(estr_blk);
		E_STR_BLK_HDR(estr_blk)->cursor = sizeof(e_str_blk_hdr);
	}
}


xptr estr::xptr_for_data()
{
	if (last_blk == XNULL) init();

    CHECKP(last_blk);

    if (E_STR_BLK_HDR(last_blk)->cursor >= PAGE_SIZE)
		estr_get_next_blk(last_blk, m_blks);

    return last_blk + E_STR_BLK_HDR(last_blk)->cursor;
}

void estr::copy_text_mstr(xptr dest, const char *src, int count)
{
    CHECKP(dest);

    int dest_spc_blk = E_STR_BLK_FREE_SPACE(E_STR_BLK_HDR(dest));
    int src_len = count;
    int real_count = s_min(dest_spc_blk, src_len);
	VMM_SIGNAL_MODIFICATION(dest);
    memcpy(XADDR(dest), src, real_count);
    E_STR_BLK_HDR(dest)->cursor += real_count;

    m_size += real_count;

    if (real_count == src_len)
        return;

	estr_get_next_blk(last_blk, m_blks);

    copy_text_mstr(last_blk + sizeof(e_str_blk_hdr), src + real_count, count - real_count);
}

void estr::copy_text_mstr(xptr dest, const char *src)
{
	estr::copy_text_mstr(dest, src, strlen(src));
}

void estr::copy_text_cursor(xptr dest, str_cursor *cur)
{
    e_str_blk_hdr *dest_blk = E_STR_BLK_HDR(dest);
    char *dest_addr = (char*)XADDR(dest);

    CHECKP(dest);
    int dest_spc_blk = E_STR_BLK_FREE_SPACE(dest_blk);
    int src_spc_blk = cur->copy_blk(tr_globals::e_string_buf);
    int copied_count = 0;

    while (src_spc_blk > 0)
    {
        if (dest_spc_blk == 0)
        {
            U_ASSERT(BLOCKXPTR(dest) == last_blk);
            estr_get_next_blk(last_blk, m_blks);
            dest = last_blk + sizeof(e_str_blk_hdr);
            dest_addr = (char*)XADDR(dest);
            dest_blk = E_STR_BLK_HDR(dest);
            dest_spc_blk = E_STR_BLK_FREE_SPACE(dest_blk);
        }
        const int real_count = s_min(dest_spc_blk, src_spc_blk - copied_count);
        CHECKP(dest);
        VMM_SIGNAL_MODIFICATION(dest);
        memcpy(dest_addr, tr_globals::e_string_buf + copied_count, real_count);
        m_size += real_count;
        dest_spc_blk -= real_count;
        copied_count += real_count;
        dest_blk->cursor += real_count;
        dest_addr += real_count;

        if (copied_count == src_spc_blk)
        {
            src_spc_blk = cur->copy_blk(tr_globals::e_string_buf);
            copied_count = 0;
        }
    }
}

int estr::blks_to_allocate(int str_len)
{
    CHECKP(last_blk);
    int free_space = E_STR_BLK_FREE_SPACE(XADDR(last_blk));
    if (str_len <= free_space) return 0;
    else return (str_len - free_space) / (PAGE_SIZE - sizeof(e_str_blk_hdr)) + 1;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// e_str_cursor class implementation
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
size_t estr_cursor::copy_blk(char *buf)
{
    if (!m_count) return 0;

    CHECKP(m_str);
    size_t len = BLK_BEGIN_INT(XADDR(m_str)) + PAGE_SIZE - (uintptr_t)XADDR(m_str);
    if (len >= (uint64_t) m_count)
    {
        len = (size_t)m_count;
        memcpy(buf, XADDR(m_str), len);
        m_str = m_str + len;
    }
    else
    {
        memcpy(buf, XADDR(m_str), len);
        m_str = E_STR_PROLONGATION(m_str);
    }
    m_count -= len;
    return len;
}

size_t estr_cursor::get_blk(char **ptr)
{
    if (!m_count) return 0;

    CHECKP(m_str);
    size_t len = BLK_BEGIN_INT(XADDR(m_str)) + PAGE_SIZE - (uintptr_t)XADDR(m_str);
    if (len >= (uint64_t) m_count)
    {
        len = (size_t)m_count;
        *ptr = (char*)(XADDR(m_str));
        m_str = m_str + len;
    }
    else
    {
        *ptr = (char*)(XADDR(m_str));
        m_str = E_STR_PROLONGATION(m_str);
    }
    m_count -= len;
    return len;
}


void estr_feed(string_consumer_fn fn, void *p, xptr src, str_off_t count) // or pstr
{
	while (count > 0)
	{
		CHECKP(src);

		int src_spc_blk = BLK_BEGIN_INT(XADDR(src)) + PAGE_SIZE - (uintptr_t)(XADDR(src));
		int real_count = (int)s_min(src_spc_blk, count);

		fn((char*)XADDR(src), real_count, p);

		if (real_count == count) return;

		src = E_STR_PROLONGATION(src);
		count = count - real_count;
	}
}

//almost the same as estr_feed
void estr_copy_to_buffer(char *dest, xptr src, str_off_t count)
{
	while (count > 0)
	{
		CHECKP(src);

		int src_spc_blk = BLK_BEGIN_INT(XADDR(src)) + PAGE_SIZE - (uintptr_t)(XADDR(src));
		int real_count = (int)s_min(src_spc_blk, count);

		memcpy(dest, XADDR(src), real_count);
		dest += real_count;

		if (real_count == count) return;

		src = E_STR_PROLONGATION(src);
		count = count - real_count;
	}
}

