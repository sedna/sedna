/*
 * File:  strings.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "utf8.h"
#include "pstr.h"
#include "pstr_long.h"
#include "casting_operations.h"
#include "d_printf.h"

static CharsetHandler_utf8 utf8_charset_handler;
CharsetHandler *charset_handler = &utf8_charset_handler;

static CollationHandler_utf8 utf8handler;
CollationHandler *collation_handler = &utf8handler;

CollationManager collation_manager;

#define T_STR_MEMBUF_SIZE 100

void str_buf_base::move_to_mem_buf()
{
	if (m_flags & f_text_in_buf)
		return;
	if (m_buf_size <= m_len)
	{
		if (m_buf_size > 0)
			free(m_buf);
		m_buf_size = m_len + 1;
		if (m_buf_size < T_STR_MEMBUF_SIZE)
			m_buf_size = T_STR_MEMBUF_SIZE;
		m_buf = (char*)malloc(m_buf_size);
	}
	switch (m_ttype)
	{
	case text_mem:
		strcpy(m_buf, m_str_ptr.get());
		m_flags |= f_text_in_buf;
		m_ttype = text_mem;
		return;
	case text_estr:
		e_str_copy_to_buffer(m_buf, m_ptr, m_len);
		m_buf[m_len] = 0;
		m_flags |= f_text_in_buf;
		m_ttype = text_mem;
		return;
	case text_doc:
		if (m_len < PSTRMAXSIZE)
			e_str_copy_to_buffer(m_buf, m_ptr, m_len);
		else
			pstr_long_copy_to_buffer(m_buf, m_ptr, m_len);
		m_buf[m_len] = 0;
		m_flags |= f_text_in_buf;
		m_ttype = text_mem;
		return;
	}
	throw USER_EXCEPTION2(SE1003, "Impossible case in str_buf_base::move_to_mem_buf()");
}

//pre: text is not in estr buf
void str_buf_base::move_to_estr()
{
	if (m_flags & f_text_in_buf)
	{
		m_ptr = m_estr.append_mstr(m_buf);
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

void str_buf_base::clear()
{
	m_len = 0;
	if (m_flags & f_text_in_estr_buf)
		clear_estr_buf();
	m_flags = 0;
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

			if (new_len < T_STR_MEMBUF_SIZE)
			{
				if (m_buf_size == 0)
				{
					m_buf_size = T_STR_MEMBUF_SIZE;
					m_buf = (char *)malloc(m_buf_size);
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
	if (new_len < T_STR_MEMBUF_SIZE)
	{
		if (m_buf_size == 0)
		{
			m_buf_size = T_STR_MEMBUF_SIZE;
			ASSERT(m_buf == NULL);
			m_buf = (char *)malloc(m_buf_size);
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
		m_buf[m_len] = 0;
	}
	else
	{
		if (!(m_flags & f_text_in_estr_buf))
			move_to_estr();
		m_estr.append_mstr(str, add_len);
		m_len = new_len;
		m_ttype = text_estr;
		m_flags = f_text_in_estr_buf; //clear f_text_in_buf
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

str_buf_base::~str_buf_base()
{
	if (m_buf_size > 0)
		free(m_buf);
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
		pstr_long_feed(tc.get_str_vmm(), fn, p);
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
        return &utf8handler;
    else
        return NULL;
}

CollationHandler *CollationManager::get_default_collation_handler()
{
    return &utf8handler;
}

