/*
 * File:  e_string.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _E_STRING_H
#define _E_STRING_H

#include "sedna.h"

#include "sm_vmm_data.h"
#include "vmm.h"
#include "tuple.h"

#define BLK_BEGIN_INT(p)			((int)(p) & PAGE_BIT_MASK)
#define E_STR_PROLONGATION(p)		(((e_str_blk_hdr*)(BLK_BEGIN_INT(XADDR(p))))->nblk + sizeof(e_str_blk_hdr))
#define E_STR_BLK_HDR(p)			((e_str_blk_hdr*)(BLK_BEGIN_INT(XADDR(p))))
#define E_STR_BLK_FREE_SPACE(p)		(PAGE_SIZE - ((e_str_blk_hdr*)(p))->cursor)

#define E_STR_NOT_IN_ONE_BLOCK(p)  (((int)XADDR(p.get_str_vmm())& PAGE_REVERSE_BIT_MASK) +p.get_strlen_vmm()>PAGE_SIZE)

struct e_str_blk_hdr 
{
    vmm_sm_blk_hdr sm_vmm;	// sm/vmm parameters
    xptr nblk;				// next block
    int cursor;				// cursor

	static void init(void *p) 
    { 
        ((e_str_blk_hdr*)p)->nblk = XNULL; 
        ((e_str_blk_hdr*)p)->cursor = sizeof(e_str_blk_hdr);
        VMM_SIGNAL_MODIFICATION(ADDR2XPTR(p));
    }
};



void copy_text(char *dest,      xptr  src,       int count);

xptr copy_text(xptr  src,       int count,       xptr *txt_blk,           int *new_blks_num = NULL);
xptr copy_text(const char *src, xptr *txt_blk,   int *new_blks_num = NULL);

void copy_text(xptr  dest,      xptr  src,       int count,     xptr *txt_blk, int *new_blks_num = NULL);
void copy_text(xptr  dest,      const char *src, xptr *txt_blk, int *new_blks_num = NULL);

int get_length_of_last_str (xptr pos);

int blocks_to_allocate(xptr *txt_blk, int str_len);

extern xptr e_string_first_blk;
extern xptr e_string_last_blk;

void init_e_string_blks();





//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////

class e_str
{
private:
    xptr first_blk;
    xptr last_blk;

    int m_blks;
    int m_size;

    void init();
    xptr xptr_for_data();
    void copy_text_mstr(xptr dest, const char *src, int count);
    void copy_text_mstr(xptr dest, const char *src);
    void copy_text_estr(xptr dest, xptr src, int count);
    void copy_text_pstr_long(xptr dest, xptr src);

public:
    e_str() : m_blks(0), m_size(0) {}
    void reset();
	void clear()
	{
		m_blks = 0;
		m_size = 0;
		first_blk = XNULL;
		last_blk = XNULL;
	}

    xptr append(const tuple_cell& tc)
    {
        switch (tc.get_type())
        {
            case tc_light_atomic           : return append_mstr(tc.get_str_mem());
            case tc_heavy_atomic_estr      :
			case tc_heavy_atomic_pstr_short: return append_estr(tc.get_str_vmm(), tc.get_strlen_vmm());
            case tc_heavy_atomic_pstr_long : return append_pstr_long(tc.get_str_vmm());
            default                        : throw USER_EXCEPTION2(SE1003, "Impossible case in e_str::append(const tuple_cell& tc)");
        }
    }

    xptr append_mstr(const char *src); // memory strings
    xptr append_mstr(const char *src, int count); // memory strings
    xptr append_estr(const xptr &src, int count); // e_str 
    xptr append_pstr_short(const xptr &src, int count) { return append_estr(src, count); }
    xptr append_pstr_long(const xptr &src);
    xptr append_pstr(const xptr &src, int count) { return count > PSTRMAXSIZE ? 
                                                   append_pstr_long(src) :
                                                   append_pstr_short(src, count); }

    int blks() const { return m_blks; }
    int size() const { return m_size; }
    // how many new blocks will be allocated if we append a string of length str_len
    int blks_to_allocate(int str_len);
};

void e_str_copy_to_buffer(char *dest, const xptr &src, int count);


namespace tr_globals 
{

/// Global e_strings
extern e_str e_str_global;

}


class e_str_cursor
{
private:
    xptr m_str;
    int  m_count;
public:
    e_str_cursor(const xptr& str, int count) : m_str(str), m_count(count) {}
    /// Block oriented copy. buf must have size not less than a page size
    int copy_blk(char *buf)
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
	/// Gets a pointer to string part in the current block and moves cursor to the next block
	/// (same as copy_blk, but without copy)
	/// returns the length of the string part 
	/// or 0 if end of string reached (*ptr is not modified in this case)
    /// The function calls CHECKP on the given string, so the pointer is
    /// valid until next call to CHECKP
	int get_blk(char **ptr)
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
};


class e_str_buf
{
private:
    e_str *m_str;

    int m_size;
    xptr m_start;

public:
    e_str_buf(e_str *str = &tr_globals::e_str_global) : m_str(str), m_size(0) {}
    void reinit() { m_size = 0; m_start = XNULL; }

    void append(const tuple_cell& tc)
    {
        int old_size = m_str->size();
        if (m_size) m_str->append(tc);
        else m_start = m_str->append(tc);
        m_size += m_str->size() - old_size;
    }

    void append_mstr(const char *src) // memory strings
    {
        int old_size = m_str->size();
        if (m_size) m_str->append_mstr(src);
        else m_start = m_str->append_mstr(src);
        m_size += m_str->size() - old_size;
    }
	void append_mstr(const char *src, int count) // e_str
    {
        int old_size = m_str->size();
        if (m_size) m_str->append_mstr(src, count);
        else m_start = m_str->append_mstr(src, count);
        m_size += m_str->size() - old_size;
	}
    void append_estr(const xptr &src, int count) // e_str
    {
        if (m_size) m_str->append_estr(src, count);
        else m_start = m_str->append_estr(src, count);
        m_size += count;
    }
	
    void append_pstr_short(const xptr &src, int count)
    {
        append_estr(src, count);
    }

    void append_pstr_long(const xptr &src, int count)
    {
        if (m_size) m_str->append_pstr_long(src);
        else m_start = m_str->append_pstr_long(src);
        m_size += count;
    }

    void append_pstr(const xptr &src, int count)
    {
        if (m_size) m_str->append_pstr(src, count);
        else m_start = m_str->append_pstr(src, count);
        m_size += count;
    }


    tuple_cell content() { return tuple_cell::atomic(tc_heavy_atomic_estr, xs_string, m_size, m_start); }
};


#endif

