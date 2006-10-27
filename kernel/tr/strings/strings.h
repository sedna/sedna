/*
 * File:  strings.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _STRINGS_H
#define _STRINGS_H

#include "sedna.h"


#include "strings_base.h"
#include "tuple.h"
#include "e_string.h"
#include "micro.h"

#include "pstr_long.h"
#include "e_string_o_iterator.h"
#include "e_string_iterator.h"
#include "char_iterator.h"
///FIXME!!!!!!!!!!!!!!!!
// move text_type here, text_doc => text_pstr


//FIXME - length is pstr_long should not be int
#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, tcell_ptr, params) \
	switch (tcell_ptr->get_type()) \
	{ \
	case tc_light_atomic_var_size: \
	case tc_light_atomic_fix_size: {\
		char *str = tcell_ptr->get_str_mem(); \
		int len = tcell_ptr->get_strlen_mem(); \
		char_iterator start1(str, len, 0); \
		char_iterator end1(str, len, len); \
		func<char_iterator> params; \
		break; }\
	case tc_heavy_atomic_estr: \
	case tc_heavy_atomic_pstr_short: {\
		int len = tcell_ptr->get_strlen_vmm(); \
		xptr data = tcell_ptr->get_str_vmm(); \
\
		e_string_iterator_first start1(len, data); \
		e_string_iterator_first end1(0, data); \
		func<e_string_iterator> params; \
		break; }\
	case tc_heavy_atomic_pstr_long: {\
		int len = tcell_ptr->get_strlen_vmm(); \
		xptr data = tcell_ptr->get_str_vmm(); \
\
		pstr_long_iterator start1(data); \
		pstr_long_iterator end1(data, len); \
		func<pstr_long_iterator> params; \
		break; }\
	}

#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(func, tcell_ptr, p1)             STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, (tcell_ptr), (start1, end1, p1))
#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr_2p(func, tcell_ptr, p1, p2)         STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, (tcell_ptr), (start1, end1, p1, p2))
#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(func, tcell_ptr, p1, p2, p3)     STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, (tcell_ptr), (start1, end1, p1, p2, p3))
#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr_4p(func, tcell_ptr, p1, p2, p3, p4) STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, (tcell_ptr), (start1, end1, p1, p2, p3, p4))


//TODO: rewrite these 2 classes

// FIXME rename it to op_str_buf
class t_str_buf
{
private:
	xptr m_ptr;
	e_str m_estr;
	str_counted_ptr m_str_ptr;
	int m_len;//FIXME (don't use int type)
	char *m_buf;
	int m_buf_size;
	text_type m_ttype;
	static const int f_text_in_buf = 1;
	static const int f_text_in_estr_buf = 2;
	unsigned char m_flags;
	void move_to_mem_buf();
	void move_to_estr();
public:
#define USTRBUF_INIT m_buf_size(0), m_buf(NULL), m_len(0), m_ptr(XNULL), m_flags(0)
	t_str_buf() : USTRBUF_INIT {}
	t_str_buf(const tuple_cell &tc) : USTRBUF_INIT { append(tc); }
	t_str_buf(const char *str) : USTRBUF_INIT { append(str); }
#undef USTRBUF_INIT
	text_type get_type() { return m_ttype; }
	void * get_ptr_to_text() {
		if (m_flags & f_text_in_buf)
			return m_buf;
		else if (m_ttype == text_mem)
			return m_str_ptr.get();
		else
			return &m_ptr;
	}
	int get_size() { return m_len; } //FIXME (don't use int type)
	char *c_str();
	void clear();
	void append(const tuple_cell &tc);
	void append(const char *str);//always copy to inner buffer
	void append(const char *str, int add_len);//always copy to inner buffer
	void set(const tuple_cell &tc) { clear(); append(tc); }
	void set(const char *str) { clear(); append(str); }
	
	~t_str_buf();
	
	t_str_buf(const t_str_buf&) { throw USER_EXCEPTION2(SE1003, "Copy constructor for t_str_buf is not implemented"); }
    t_str_buf& operator=(const t_str_buf&) { throw USER_EXCEPTION2(SE1003, "Assign operator for t_str_buf is not implemented"); }
	virtual t_str_buf& operator<<(const char *s)	{ this->append(s); 
                                                          return *this; }
    virtual t_str_buf& operator<<(char c)		{ this->append(&c, 1); return *this; }
};

class stmt_str_buf_impl
{
private:
	xptr m_ptr;
	e_str m_estr_;
	str_counted_ptr m_str_ptr;
	int m_len;//FIXME (don't use int type)
	char *m_buf;
	int m_buf_size;
	text_type m_ttype;
	static const int f_text_in_buf = 1;
	static const int f_text_in_estr_buf = 2;
	unsigned char m_flags;
	void move_to_mem_buf();
	void move_to_estr();
public:
#define USTRBUF_INIT m_buf_size(0), m_buf(NULL), m_len(0), m_ptr(XNULL), m_flags(0)
	stmt_str_buf_impl() : USTRBUF_INIT {}
	stmt_str_buf_impl(const tuple_cell &tc) : USTRBUF_INIT { append(tc); }
	stmt_str_buf_impl(const char *str) : USTRBUF_INIT { append(str); }
#undef USTRBUF_INIT
	text_type get_type() { return m_ttype; }
	void * get_ptr_to_text() {
		if (m_flags & f_text_in_buf)
			return m_buf;
		else if (m_ttype == text_mem)
			return m_str_ptr.get();
		else
			return &m_ptr;
	}
	int get_size() { return m_len; } //FIXME (don't use int type)
	char *c_str();
	void clear();
	void append(const tuple_cell &tc);
	void append(const char *str);//always copy to inner buffer
	void append(const char *str, int add_len);//always copy to inner buffer
	void set(const tuple_cell &tc) { clear(); append(tc); }
	void set(const char *str) { clear(); append(str); }
	
	tuple_cell get_tuple_cell() { 
	
		if (m_flags & f_text_in_buf)
		{
			tuple_cell tc = tuple_cell::atomic(xs_string, m_buf);
			m_buf = NULL;
			m_buf_size = 0;
			m_flags = 0;
			m_ptr = XNULL;
			m_len = 0;
			return tc;
		}
		else
		{
			tuple_cell tc = (get_type() == text_mem) ?
							tuple_cell::atomic_deep(xs_string, (char*)get_ptr_to_text())
						   :tuple_cell::atomic_estr(xs_string, get_size(), *(xptr*)get_ptr_to_text());
			clear();
			return tc;
		}
	}
	
	~stmt_str_buf_impl();
	
	stmt_str_buf_impl(const stmt_str_buf_impl&) { throw USER_EXCEPTION2(SE1003, "Copy constructor for stmt_str_buf_impl is not implemented"); }
    stmt_str_buf_impl& operator=(const stmt_str_buf_impl&) { throw USER_EXCEPTION2(SE1003, "Assign operator for stmt_str_buf_impl is not implemented"); }
};

class stmt_str_buf
{
private:
	static stmt_str_buf_impl buf_impl;
	static bool used;
	stmt_str_buf_impl *m_impl;
public:
	stmt_str_buf() { if (used) throw USER_EXCEPTION(SE1003); m_impl = &buf_impl; used = true; }
	~stmt_str_buf() { used = false; }

	void clear() {m_impl->clear();}
	void append(const tuple_cell &tc) {m_impl->append(tc);}
	void append(const char *str) {m_impl->append(str);}
	void append(const char *str, int add_len) {m_impl->append(str, add_len);}

	tuple_cell get_tuple_cell() {return m_impl->get_tuple_cell(); } //also clears

	stmt_str_buf(const stmt_str_buf&) { throw USER_EXCEPTION2(SE1003, "Copy constructor for stmt_str_buf is not implemented"); }
    stmt_str_buf& operator=(const stmt_str_buf&) { throw USER_EXCEPTION2(SE1003, "Assign operator for stmt_str_buf is not implemented"); }

	stmt_str_buf& operator<<(const char *s)	{ this->append(s); 
                                                          return *this; }
    stmt_str_buf& operator<<(char c)		{ this->append(&c, 1); return *this; }
};


class CharCounter
{
public:
	//parse sequence of bytes
	//returns full chars count, +1 if parsing ended in the middle of a character
	// (i.e. the number of first bytes of chars)
	virtual int count_chars(const char *str, int len) = 0;
};

class CharsetHandler
{
public:
	//FIXME: length souldn't be int
	virtual int length (tuple_cell *tc) = 0;
	virtual void transtale (tuple &t, tuple_cell *arg, tuple_cell *map_str, tuple_cell *trans_str) = 0;
	virtual CharCounter* new_char_counter() = 0;
	virtual void free_char_counter(CharCounter *) = 0;
};

class CollationHandler
{
public:
	virtual void replace (tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3, tuple_cell *t4) = 0;
	virtual void matches (tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3) = 0;
	virtual bool matches (const tuple_cell *tc, const char *regex) = 0;
	virtual bool matches (const char *tc, const char *regex) = 0;
};

void feed_tuple_cell(string_consumer_fn fn, void *p, const tuple_cell& tc);

void print_tuple_cell_dummy(se_ostream& crmout,const tuple_cell& tc);
inline void print_tuple_cell(se_ostream& crmout,const tuple_cell& tc)
{
    feed_tuple_cell(writextext_cb, &crmout, tc);
}


extern CharsetHandler	*charset_handler;
extern CollationHandler	*collation_handler;


#endif //_STRINGS_H
