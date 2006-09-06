/*
 * File:  strings.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _STRINGS_H
#define _STRINGS_H

#include "sedna.h"

#include "tuple.h"
#include "e_string.h"
#include "micro.h"
#include "pstr_long.h"
///FIXME!!!!!!!!!!!!!!!!
// move text_type here, text_doc => text_pstr


//FIXME - length is pstr_long should not be int
#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, tcell_ptr, params) \
	switch (tcell_ptr->get_type()) \
	{ \
	case tc_light_atomic: {\
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

#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(func, tcell_ptr, p1, p2, p3) STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, tcell_ptr, (start1, end1, p1, p2, p3) )
#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr_4p(func, tcell_ptr, p1, p2, p3, p4) STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, tcell_ptr, (start1, end1, p1, p2, p3, p4))

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
};
void print_tuple_cell(se_ostream& crmout,const tuple_cell& cell);
extern CharsetHandler	*charset_handler;
extern CollationHandler	*collation_handler;


#endif //_STRINGS_H
