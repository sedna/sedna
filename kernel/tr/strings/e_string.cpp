/*
 * File:  e_string.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"

#include "e_string.h"
#include "PPBase.h"
#include "vmm.h"
#include "pstr_long.h"



//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

namespace tr_globals 
{

e_str e_str_global;

}

/// TODO: change recursion to iteration


void e_str::init()
{
    U_ASSERT(first_blk == XNULL);
    U_ASSERT(last_blk == XNULL);

	vmm_alloc_tmp_block(&last_blk);
	e_str_blk_hdr::init(XADDR(last_blk));
	first_blk = last_blk;
    m_blks = 1;
}

void e_str::reset()
{
    CHECKP(first_blk);
    ((e_str_blk_hdr*)XADDR(first_blk))->cursor = sizeof(e_str_blk_hdr);
    VMM_SIGNAL_MODIFICATION(first_blk);

    m_size = 0;    
    last_blk = first_blk;
}

void e_str::truncate(const xptr &ptr)
{
	last_blk = BLOCKXPTR(ptr);
	CHECKP(last_blk);
	((e_str_blk_hdr*)XADDR(last_blk))->cursor = ptr - last_blk;
	VMM_SIGNAL_MODIFICATION(last_blk);

	m_size = 0;
}

xptr e_str::append_mstr(const char *src)
{
    xptr dest = xptr_for_data();
    copy_text_mstr(dest, src);
    return dest;
}

xptr e_str::append_mstr(const char *src, int count)
{
    xptr dest = xptr_for_data();
    copy_text_mstr(dest, src, count);
    return dest;
}


xptr e_str::append_estr(const xptr &src, int count)
{
    xptr dest = xptr_for_data();
    copy_text_estr(dest, src, count);
    return dest;
}

xptr e_str::append_pstr_long(const xptr &src)
{
    xptr dest = xptr_for_data();
    copy_text_pstr_long(dest, src);
    return dest;
}

xptr e_str::xptr_for_data()
{
	if (last_blk == NULL) init();

    CHECKP(last_blk);

    if (E_STR_BLK_HDR(last_blk)->cursor >= PAGE_SIZE)
    {
        ++m_blks;

		if (E_STR_BLK_HDR(last_blk)->nblk == XNULL)
		{
			xptr new_blk;
			vmm_alloc_tmp_block(&new_blk);
			e_str_blk_hdr::init(XADDR(new_blk));
			CHECKP(last_blk);
			E_STR_BLK_HDR(last_blk)->nblk = new_blk;
			VMM_SIGNAL_MODIFICATION(last_blk);
			last_blk = new_blk;
		}
		else
		{
			last_blk = E_STR_BLK_HDR(last_blk)->nblk;
			CHECKP(last_blk);
			E_STR_BLK_HDR(last_blk)->cursor = sizeof(e_str_blk_hdr);
			VMM_SIGNAL_MODIFICATION(last_blk);
		}


        CHECKP(last_blk);
    }

    return last_blk + E_STR_BLK_HDR(last_blk)->cursor;
}

void e_str::copy_text_mstr(xptr dest, const char *src, int count)
{
    CHECKP(dest);

    int dest_spc_blk = E_STR_BLK_FREE_SPACE(E_STR_BLK_HDR(dest));
    int src_len = count;
    int real_count = s_min(dest_spc_blk, src_len);

    memcpy(XADDR(dest), src, real_count);
    E_STR_BLK_HDR(dest)->cursor += real_count;
    VMM_SIGNAL_MODIFICATION(dest);
    m_size += real_count;

    if (real_count == src_len)
        return;

	if (E_STR_BLK_HDR(last_blk)->nblk == XNULL)
	{
		xptr new_dest;
		vmm_alloc_tmp_block(&new_dest);
		e_str_blk_hdr::init(XADDR(new_dest));

		CHECKP(last_blk);
		E_STR_BLK_HDR(last_blk)->nblk = new_dest;
		VMM_SIGNAL_MODIFICATION(last_blk);

		last_blk = new_dest;
		++m_blks;
	}
	else
	{
		last_blk = E_STR_BLK_HDR(last_blk)->nblk;
		CHECKP(last_blk);
		E_STR_BLK_HDR(last_blk)->cursor = sizeof(e_str_blk_hdr);
		VMM_SIGNAL_MODIFICATION(last_blk);
	}

    copy_text_mstr(last_blk + sizeof(e_str_blk_hdr), src + real_count, count - real_count);
}

void e_str::copy_text_mstr(xptr dest, const char *src)
{
	e_str::copy_text_mstr(dest, src, strlen(src));
}

void e_str::copy_text_estr(xptr dest, xptr src, int count)
{
    e_str_blk_hdr *dest_blk = E_STR_BLK_HDR(dest);

    CHECKP(dest);
    int dest_spc_blk = E_STR_BLK_FREE_SPACE(dest_blk);

    CHECKP(src);
    int src_spc_blk = BLK_BEGIN_INT(XADDR(src)) + PAGE_SIZE - (int)XADDR(src);
    int real_count = s_min(dest_spc_blk, s_min(src_spc_blk, count));

    memcpy(tr_globals::e_string_buf, XADDR(src), real_count);
    CHECKP(dest);
    memcpy(XADDR(dest), tr_globals::e_string_buf, real_count);
    m_size += real_count;

    dest_blk->cursor += real_count;
    VMM_SIGNAL_MODIFICATION(dest);

    if (real_count == count)
        return;

    // real_count < count ==> end of src or dest

    if (src_spc_blk == real_count && dest_spc_blk == real_count)
    { // end of src and dest blocks
		if (dest_blk->nblk == XNULL)
		{
			xptr new_dest;
			vmm_alloc_tmp_block(&new_dest);
			e_str_blk_hdr::init(XADDR(new_dest));
			last_blk = new_dest;

			CHECKP(dest);
			dest_blk->nblk = new_dest;
			VMM_SIGNAL_MODIFICATION(dest);
			++m_blks;
		}
		else
		{
			last_blk = dest_blk->nblk;
			CHECKP(last_blk);
			E_STR_BLK_HDR(last_blk)->cursor = sizeof(e_str_blk_hdr);
			VMM_SIGNAL_MODIFICATION(last_blk);
		}

        CHECKP(src);
        copy_text_estr(last_blk + sizeof(e_str_blk_hdr), E_STR_PROLONGATION(src), count - real_count);
        return;
    }

    if (src_spc_blk == real_count)
    { // end of src block, not of dest one
        CHECKP(src);
        copy_text_estr(dest + real_count, E_STR_PROLONGATION(src), count - real_count);
        return;
    }

    if (dest_spc_blk == real_count)
    { // end of dest block, not of src one
		if (dest_blk->nblk == XNULL)
		{
			xptr new_dest;
			vmm_alloc_tmp_block(&new_dest);
			e_str_blk_hdr::init(XADDR(new_dest));
			last_blk = new_dest;

			CHECKP(dest);
			dest_blk->nblk = new_dest;
			VMM_SIGNAL_MODIFICATION(dest);
		}
		else
		{
			last_blk = dest_blk->nblk;
			CHECKP(last_blk);
			E_STR_BLK_HDR(last_blk)->cursor = sizeof(e_str_blk_hdr);
			VMM_SIGNAL_MODIFICATION(last_blk);
		}

        copy_text_estr(last_blk + sizeof(e_str_blk_hdr), src + real_count, count - real_count);
        ++m_blks;
        return;
    }

    throw USER_EXCEPTION2(SE1003, "Impossible case in copy_text");
}


void e_str::copy_text_pstr_long(xptr dest, xptr src)
{
    e_str_blk_hdr *dest_blk = E_STR_BLK_HDR(dest);
	pstr_long_cursor src_cur(src);

    CHECKP(dest);
    int dest_spc_blk = E_STR_BLK_FREE_SPACE(dest_blk);
    int src_spc_blk = src_cur.copy_blk(tr_globals::e_string_buf);
	int copied_count = 0;

	while (src_spc_blk > 0)
	{
		if (dest_spc_blk == 0)
		{
			if (dest_blk->nblk == XNULL)
			{
				xptr new_dest;
				vmm_alloc_tmp_block(&new_dest);
				e_str_blk_hdr::init(XADDR(new_dest));
				last_blk = new_dest;
	    
				CHECKP(dest);
				dest_blk->nblk = new_dest;
				VMM_SIGNAL_MODIFICATION(dest);
				++m_blks;
			}
			else
			{
				last_blk = dest_blk->nblk;
				CHECKP(last_blk);
				E_STR_BLK_HDR(last_blk)->cursor = sizeof(e_str_blk_hdr);
				VMM_SIGNAL_MODIFICATION(last_blk);
			}
			dest = last_blk + sizeof(e_str_blk_hdr);
			dest_blk = E_STR_BLK_HDR(dest);
			dest_spc_blk = E_STR_BLK_FREE_SPACE(dest_blk);
    
		}
        const int real_count = s_min(dest_spc_blk, src_spc_blk - copied_count);
        CHECKP(dest);
        memcpy(XADDR(dest), tr_globals::e_string_buf + copied_count, real_count);
        m_size += real_count;
		dest_spc_blk -= real_count;
		copied_count += real_count;
        dest_blk->cursor += real_count;
        VMM_SIGNAL_MODIFICATION(dest);
    
		if (copied_count == src_spc_blk)
			src_spc_blk = src_cur.copy_blk(tr_globals::e_string_buf);
	}

    throw USER_EXCEPTION2(SE1003, "Impossible case in copy_text");
}

int e_str::blks_to_allocate(int str_len)
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
int e_str_cursor::copy_blk(char *buf)
{
    if (!m_count) return 0;

    CHECKP(m_str);
    int len = BLK_BEGIN_INT(XADDR(m_str)) + PAGE_SIZE - (int)XADDR(m_str);
    if (len >= m_count)
    {
        len = m_count;
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

int e_str_cursor::get_blk(char **ptr)
{
    if (!m_count) return 0;

    CHECKP(m_str);
    int len = BLK_BEGIN_INT(XADDR(m_str)) + PAGE_SIZE - (int)XADDR(m_str);
    if (len >= m_count)
    {
        len = m_count;
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







void estr_feed(string_consumer_fn fn, void *p, xptr src, int count) // or pstr,  FIXME: int count
{
	while (count > 0)
	{
		CHECKP(src);

		int src_spc_blk = BLK_BEGIN_INT(XADDR(src)) + PAGE_SIZE - (int)(XADDR(src));
		int real_count = s_min(src_spc_blk, count);

		fn((char*)XADDR(src), real_count, p);

		if (real_count == count) return;

		src = E_STR_PROLONGATION(src);
		count = count - real_count;
	}
}

//almost the same as estr_feed
void e_str_copy_to_buffer(char *dest, xptr src, int count)
{
	while (count > 0)
	{
		CHECKP(src);

		int src_spc_blk = BLK_BEGIN_INT(XADDR(src)) + PAGE_SIZE - (int)(XADDR(src));
		int real_count = s_min(src_spc_blk, count);

		memcpy(dest, XADDR(src), real_count);
		dest += real_count;

		if (real_count == count) return;

		src = E_STR_PROLONGATION(src);
		count = count - real_count;
	}
}

