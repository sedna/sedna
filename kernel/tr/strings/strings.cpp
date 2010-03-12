/*
 * File:  strings.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/strings/utf8.h"
#include "tr/pstr/pstr.h"
#include "tr/pstr/pstr_long.h"
#include "tr/executor/fo/casting_operations.h"
#include "common/errdbg/d_printf.h"


CharsetHandler *charset_handler = NULL;

#define T_STR_MEMBUF_SIZE 100

void str_buf_base::move_to_mem_buf()
{
	if (m_flags & f_text_in_buf)
		return;
	if (m_buf_size <= m_len)
	{
		if (m_buf_size > 0)
			delete[] m_buf;
		m_buf_size = m_len + 1;
		if (m_buf_size < T_STR_MEMBUF_SIZE)
			m_buf_size = T_STR_MEMBUF_SIZE;
		m_buf = se_new char[m_buf_size];
	}
	if (m_len == 0)
	{
		m_buf[0] = 0;
		return;
	}
	switch (m_ttype)
	{
	case text_mem:
		strcpy(m_buf, m_str_ptr.get());
		m_flags |= f_text_in_buf;
		m_ttype = text_mem;
		return;
	case text_estr:
		estr_copy_to_buffer(m_buf, m_ptr, m_len);
		m_buf[m_len] = 0;
		m_flags |= f_text_in_buf;
		m_ttype = text_mem;
		return;
	case text_doc:
		if (m_len < PSTRMAXSIZE)
			estr_copy_to_buffer(m_buf, m_ptr, m_len);
		else
			pstr_long_copy_to_buffer2(m_buf, m_ptr, m_len);
		m_buf[m_len] = 0;
		m_flags |= f_text_in_buf;
		m_ttype = text_mem;
		return;
	}
	throw USER_EXCEPTION2(SE1003, "Impossible case in str_buf_base::move_to_mem_buf()");
}

//pre: text is not in estr buf
//post: m_ttype == text_estr
void str_buf_base::move_to_estr()
{
	U_ASSERT(!mem_only());
	if (m_flags & f_text_in_buf)
	{
		m_ptr = m_estr.append_mstr(m_buf);
		m_ttype = text_estr;
		m_flags |= f_text_in_estr_buf;
		return;
	}
	switch (m_ttype)
	{
	case text_mem:
		m_ptr = m_estr.append_mstr(m_str_ptr.get());
		m_ttype = text_estr;
		m_flags |= f_text_in_estr_buf;
		return;
	case text_estr:
		m_ptr = m_estr.append_estr(m_ptr, m_len);
		m_flags |= f_text_in_estr_buf;
		return;
	case text_doc:
		if (m_len < PSTRMAXSIZE)
			m_ptr = m_estr.append_estr(m_ptr, m_len);
		else
			m_ptr = m_estr.append_pstr_long(m_ptr);
		m_ttype = text_estr;
		m_flags |= f_text_in_estr_buf;
		return;
	}
	throw USER_EXCEPTION2(SE1003, "Impossible case in op_str_buf::move_to_estr()");
}

str_cursor *str_buf_base::get_cursor() const
{
	if (m_flags & f_text_in_buf)
		return new str_cursor_mem(m_buf, m_len);
	switch (m_ttype)
	{
	case text_mem:
		return new str_cursor_mem(m_str_ptr.get(), m_len);
	case text_estr:
		return new estr_cursor(m_ptr, m_len);
	case text_doc:
		if (m_len < PSTRMAXSIZE)
			return new estr_cursor(m_ptr, m_len);
		else
			return new pstr_long_cursor(m_ptr);
	default:
		U_ASSERT(false);
	}
}

void str_buf_base::clear()
{
	m_len = 0;
	if (m_flags & f_text_in_estr_buf)
		clear_estr_buf();
	m_flags = 0;
	m_ttype = text_mem;
}

void str_buf_base::append(const tuple_cell &tc)
{
	if (m_len == 0)
	{
		m_flags = 0;
		if (tc.is_light_atomic())
		{
			m_str_ptr = tc.get_str_ptr();
			m_len = tc.get_strlen_mem();
			m_flags = 0;
			m_ttype = text_mem;
		}
		else
		{
			m_len = tc.get_strlen_vmm();
			m_ptr = tc.get_str_vmm();
			if (tc.get_type() == tc_heavy_atomic_estr)
				m_ttype = text_estr;
			else
				m_ttype = text_doc;
		}
	}
	else
	{
		if (tc.is_light_atomic())
		{
			append(tc.get_str_mem());
		}
		else
		{
			const int add_len = tc.get_strlen_vmm();
			const int new_len = m_len + add_len;

			if (new_len < T_STR_MEMBUF_SIZE || mem_only())
			{
				if (m_buf_size == 0)
				{
					m_buf_size = T_STR_MEMBUF_SIZE;
					while (m_buf_size <= new_len)
						m_buf_size *= 2;
					m_buf = se_new char[m_buf_size];
				}
				else if (m_buf_size <= new_len)
				{
					char *old_buf;
					while (m_buf_size <= new_len)
						m_buf_size *= 2;
					
					old_buf = m_buf;
					m_buf = se_new char[m_buf_size];
					memcpy(m_buf, old_buf, m_len+1);
					delete[] old_buf;
				}
				move_to_mem_buf();
				if (m_flags & f_text_in_estr_buf)
				{
					m_flags = f_text_in_buf; //clear f_text_in_estr_buf
					clear_estr_buf();
				}
				tc.copy_string(m_buf + m_len);
				m_buf[new_len] = 0;
				m_len = new_len;
			}
			else
			{
				if (!(m_flags & f_text_in_estr_buf))
					move_to_estr();
				m_flags = f_text_in_estr_buf; //clear f_text_in_buf
				m_estr.append(tc);
				m_len = new_len;
			}
		}
	}
}

void str_buf_base::append(const char *str, int add_len)
{
	const int new_len = m_len + add_len;
	if (new_len < T_STR_MEMBUF_SIZE || mem_only())
	{
		if (m_buf_size == 0)
		{
			m_buf_size = T_STR_MEMBUF_SIZE;
			while (m_buf_size <= new_len)
				m_buf_size *= 2;
			U_ASSERT(m_buf == NULL);
			m_buf = se_new char[m_buf_size];
		}
		else if (m_buf_size <= new_len)
		{
			char *old_buf;
			while (m_buf_size <= new_len)
				m_buf_size *= 2;
			
			old_buf = m_buf;
			m_buf = se_new char[m_buf_size];
			memcpy(m_buf, old_buf, m_len+1);
			delete[] old_buf;
		}
		if (m_len > 0)
		{
			move_to_mem_buf();
			if (m_flags & f_text_in_estr_buf)
				clear_estr_buf();
		}
		m_flags = f_text_in_buf; //clear f_text_in_estr_buf
		m_ttype = text_mem;
		strncpy(m_buf + m_len, str, add_len);
		m_len = new_len;
		U_ASSERT(m_len < m_buf_size);
		m_buf[m_len] = 0;
	}
	else
	{
		char *str_copy = (char*)malloc(add_len); //FIXME (what if NULL)
		memcpy(str_copy, str, add_len);
		//TODO: add a flag/another func that avoids copying str & use it where possible
		if (!(m_flags & f_text_in_estr_buf))
			move_to_estr();
		m_estr.append_mstr(str_copy, add_len);
		m_len = new_len;
		m_flags = f_text_in_estr_buf; //clear f_text_in_buf
		free(str_copy); //FIXME: check that no exceptions can be thrown above
	}
}
	
void str_buf_base::append(const char *str)
{
	const int add_len = strlen(str);
	this->append(str, add_len);
}
	
char * str_buf_base::c_str()
{
	if (m_flags & f_text_in_buf)
		return m_buf;
	if (m_ttype == text_mem)
		return m_str_ptr.get();
	move_to_mem_buf();
	return m_buf;
}
char * str_buf_base::get_str()
{
	U_ASSERT(mem_only());
	move_to_mem_buf();
	char *res = m_buf;

	m_buf = NULL;
	m_buf_size = 0;
	U_ASSERT(!(m_flags & f_text_in_estr_buf));
	m_flags = 0;
	m_ptr = XNULL;
	m_len = 0;
	m_ttype = text_mem;

	return res;
}

str_buf_base::~str_buf_base()
{
	if (m_buf_size > 0)
		delete[] m_buf;
}

stmt_str_buf_impl stmt_str_buf::buf_impl;
bool stmt_str_buf::used = false;

void feed_tuple_cell(string_consumer_fn fn, void *p, const tuple_cell& tc)
{
	tuple_cell cell = cast(tc, xs_string);

	switch (tc.get_type())
	{
	case tc_light_atomic_var_size:
		fn(tc.get_str_mem(), tc.get_strlen_mem(), p);
		return;
	case tc_heavy_atomic_estr:
	case tc_heavy_atomic_pstr_short:
		estr_feed(fn, p, tc.get_str_vmm(), tc.get_strlen_vmm());
		return;
	case tc_heavy_atomic_pstr_long:
		pstr_long_feed2(tc.get_str_vmm(), fn, p);
		return;
	}
}

void print_tuple_cell_dummy(se_ostream& crmout,const tuple_cell& tc)
{
    feed_tuple_cell(writextext_cb, &crmout, tc);
}


CollationHandler *CollationManager::get_collation_handler(const char *uri)
{
    if (strcmp(uri, "http://www.w3.org/2005/xpath-functions/collation/codepoint") == 0)
        return utf8_charset_handler.get_unicode_codepoint_collation();
    else
        return NULL;
}

CollationHandler *CollationManager::get_default_collation_handler()
{
    return utf8_charset_handler.get_unicode_codepoint_collation();
}

