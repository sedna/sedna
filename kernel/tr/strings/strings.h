/*
 * File:  strings.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _STRINGS_H
#define _STRINGS_H

/**

There are several types of strings in sedna, listred below:
   estr      - temporary strings stored in vmm memory (temp blocks)
   pstr      - persistent strings in db, length <= PSTRMAXSIZE
   pstr_long - persistent strings in db, length > PSTRMAXSIZE
   char *    - C strings in the heap

A string in tuple_cell may of any of these types.



Methods of reading strings:

1. Cursors
    Cursors allow to traverse a string from the start block by block. Cursors
  exist for pstr_long and estr strings (other string types are not split to
  blocks). These cursors are called pstr_long_cursor and estr_cursor
  respectivly and have a common ancesor class str_cursor which defines a basic
  set of operations any string cursor should support.
                            str_cursor
                           //        \\
                          //          \\
                 pstr_long_cursor    estr_cursor
    Cursors are byte-oriented. Data in string parts depends on encoding and
  a character may be split into several separate string parts for multibyte
  encodings.

2. Feeding a string
    Feeding a string is analogous to using a cursor. String is traversed by
  portions from the start till the string end. For each portion a callback
  function is called.
    There is an usiversal feed_tuple_cell function which feeds a string
    contained in a tuple_cell to some callback function. There are also feed
    functions for estr and pstr_long strings, called feed_estr and
    feed_pstr_long respectively.
    Feed functions are byte-oriented. Data in string parts depends on encoding and
  a character may be split into several separate string parts for multibyte
  encodings.

3. Bidirectional byte iterators
    For each string type there is an iterator class, which has interface
  similar to STL iterators:
      string type | iterator
      ------------+-------------
      estr        | estr_iterator
      pstr        | estr_iterator
      pstr_long   | pstr_long_iterator
      char*       | char_iterator/char*
    To simplify using of these iterators there is a macro that allows to
  call appropriate function template instance, which has string start and
  string end iterators as parameters:
    #define STRING_ITERATOR_CALL_TEMPLATE_1tcptr_<N>p(func, tcell_ptr, p1, p2, ..., p<N>)
  here func is the name of function template, tcell_ptr is a pointer to a
  tuple_cell,p<i> are user parameters.
    1 <= N <= 4.
    Function template must have one parameter - iterator type.
    Instance functions must have N+2 parameters: start iterator, end iterator
  and N user-defined parameters.
    End iterator may be used to compare it with other iterators only. Modifying
  or dereferencing it results in unpredicted behaviour.
4. unicode_cp_iterator
    unicode_cp_iterator is an abstaract class with allows forward only traverse
  of a string character by character. This is achieved by calling
  get_next_char() function which will return unicode character codes for each
  character in the string until the end of the string is reached. When this
  happens an -1 is returned.
    CharsetHandler has a function which returns unicode_cp_iterator for any
    tuple_cell (get_unicode_cp_iterator).


Writing strings:

  Temporary strings may be created using op_str_buf and stmt_str_buf by
appendings string parts.
  stmt_str_buf also supports output iterator interface (similar to STL)
  There may be no more than one instance of stmt_str_buf at any moment. Thus
operations that use stmt_str_buf must call all functions which may use it
outside of blocks which contain stmt_str_buf variables and make sure than no
other instances of stmt_str_buf exist when these functions are called.
  Strings created by stmt_str_buf are valid until the end of the statement.
  Strings created by op_str_buf are valid until construction of another string
by this class has begun (i.e. until op_str_buf::clear() is called)


**/


#include "common/sedna.h"


#include "tr/strings/strings_base.h"
#include "tr/executor/base/tuple.h"
#include "tr/strings/e_string.h"

#include "tr/pstr/pstr_long.h"
#include "tr/strings/e_string_iterator.h"
#include "tr/strings/char_iterator.h"

#include "text_data.h"

//FIXME: int conversions in char_iterator constructors
#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, tcell_ptr, params) \
	switch (tcell_ptr->get_type()) \
	{ \
	case tc_light_atomic_var_size: \
	case tc_light_atomic_fix_size: {\
		unsigned char *__str = (unsigned char *)tcell_ptr->get_str_mem(); \
		str_off_t __len = tcell_ptr->get_strlen_mem(); \
		char_iterator __start1(__str, (int)__len, 0); \
		char_iterator __end1(__str, (int)__len, (int)__len); \
		func params; \
		break; }\
	case tc_heavy_atomic_estr: \
	case tc_heavy_atomic_pstr_short: {\
		str_off_t __len = tcell_ptr->get_strlen_vmm(); \
		xptr __data = tcell_ptr->get_str_vmm(); \
\
		estr_iterator __start1(__len, __data); \
		estr_iterator __end1(0, __data); \
		func params; \
		break; }\
	case tc_heavy_atomic_pstr_long: {\
		pstr_long_off_t __len = tcell_ptr->get_strlen_vmm(); \
		xptr __data = tcell_ptr->get_str_vmm(); \
\
		pstr_long_iterator __start1(__data); \
		pstr_long_iterator __end1(__data, __len); \
		func params; \
		break; }\
	default: \
		throw USER_EXCEPTION2(SE1003, "Non string type in string iterator call template");\
	}

#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(func, tcell_ptr, p1)             STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, (tcell_ptr), (__start1, __end1, p1))
#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr_2p(func, tcell_ptr, p1, p2)         STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, (tcell_ptr), (__start1, __end1, p1, p2))
#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(func, tcell_ptr, p1, p2, p3)     STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, (tcell_ptr), (__start1, __end1, p1, p2, p3))
#define STRING_ITERATOR_CALL_TEMPLATE_1tcptr_4p(func, tcell_ptr, p1, p2, p3, p4) STRING_ITERATOR_CALL_TEMPLATE_1tcptr(func, (tcell_ptr), (__start1, __end1, p1, p2, p3, p4))

class memstr_cursor : public str_cursor
{
private:
	char *m_buf;
	size_t m_bytes_left;
public:
	memstr_cursor(char *buf, size_t size) : m_buf(buf), m_bytes_left(size) {}
    /// Block oriented copy. buf must have size not less than a page size
	virtual size_t copy_blk(char *buf) {
		const size_t size = s_min(m_bytes_left, PAGE_SIZE);
		if (size > 0)
		{
			memcpy(buf, m_buf, size);
			m_buf += size;
			m_bytes_left -= size;
		}
		return size;
	}
	/// Gets a pointer to string part in the current block and moves cursor to the next block
	/// (same as copy_blk, but without copy)
	/// returns the length of the string part
	/// or 0 if end of string reached (*ptr is not modified in this case)
    /// The function calls CHECKP on the given string, so the pointer is
    /// valid until next call to CHECKP
	virtual size_t get_blk(char **ptr) {
		const size_t size = s_min(m_bytes_left, PAGE_SIZE);
		if (size > 0)
		{
			*ptr = m_buf;
			m_buf += size;
			m_bytes_left -= size;
		}
		return size;
	}
};

static str_cursor * get_text_cursor(const text_source_t text);

class str_buf_base
{
private:
	xptr m_ptr;
	bool m_mem_only;
	unsigned char m_flags;
	int m_buf_size;
	char *m_buf;
	str_off_t m_len;
	estr m_estr;
	str_counted_ptr m_str_ptr;
	text_source_t::type_t m_ttype;
	static const int f_text_in_buf = 1;
	static const int f_text_in_estr_buf = 2;
	//string must be in estr buf, f_text_in_estr_buf flag is NOT cleared
	void clear_estr_buf() { m_estr.truncate(m_ptr); m_ptr = XNULL; }
	void move_to_mem_buf();
	void move_to_estr();
protected:
	str_buf_base() : m_ptr(XNULL), m_mem_only(false), m_flags(0), m_buf_size(0), m_buf(NULL), m_len(0)  {}
	str_buf_base(bool _mem_only_) : m_ptr(XNULL), m_mem_only(_mem_only_), m_flags(0), m_buf_size(0), m_buf(NULL), m_len(0) {}
	//gets tuple_cell, it's data in estr buffer won't be overwrited
	tuple_cell get_tuple_cell() {
		if (m_flags & f_text_in_buf)
		{
			tuple_cell tc = tuple_cell::atomic(xs_string, m_buf);
			m_buf = NULL;
			m_buf_size = 0;
			if (m_flags & f_text_in_estr_buf)
				clear_estr_buf();
			m_flags = 0;
			m_ptr = XNULL;
			m_len = 0;
			m_ttype = text_source_t::text_mem;
			return tc;
		}
		else
		{
			if (get_type() == text_source_t::text_mem)
			{
				tuple_cell tc = tuple_cell::atomic_deep(xs_string, get_str_mem());
				clear();
				return tc;
			}
			else
			{
				tuple_cell tc;
				if (m_flags & f_text_in_estr_buf || m_ttype == text_source_t::text_estr)
					tc = tuple_cell::atomic_estr(xs_string, get_size(), m_ptr);
				else
				{
					U_ASSERT(m_ttype == text_source_t::text_pstr || m_ttype == text_source_t::text_pstrlong);
					tc = tuple_cell::atomic_pstr(xs_string, get_size(), m_ptr);
				}
				m_ptr = XNULL;
				m_flags = 0;
				clear();
				return tc;
			}
		}
	}
public:
	bool mem_only() { return m_mem_only; }
	text_source_t::type_t get_type() const { return m_ttype; }
	//return pointer of string in the memory
	//when this function is called, string must be in the memory
	const char *get_str_mem() const
	{
		U_ASSERT(get_type() == text_source_t::text_mem);
		if (m_flags & f_text_in_buf)
			return m_buf;
		else if (m_len == 0)
			return "";
		else
			return m_str_ptr.get();
	}

	void fill_text_source(text_source_t *ts) const
	{
		ts->type = get_type();
		ts->_text_size = m_len; // WARNING: check carefully!

		if (ts->type == text_source_t::text_mem)
		{
			ts->u.cstr = get_str_mem();
			return;
		}
		else
		{
			ts->u.data = m_ptr;
			return;
		}
	}

	///gets cursor to the text stored in the buffer
	///caller is responsible for deleting cursor with free_cursor method
	///buffer can't be modified until cursor is freed

	inline str_cursor *get_cursor() const {
        text_source_t ts;
        this->fill_text_source(&ts);
        return get_text_cursor(ts);
    }

	void free_cursor(str_cursor *cur) const {
		delete cur;
	}
	str_off_t get_size() { return m_len; } //FIXME (don't use int type)
	void clear();
	void reset() { clear(); m_ptr = XNULL; m_estr.clear();}
	void append(const char *str, int add_len);//always copy to inner buffer
	void append(const char *str);//always copy to inner buffer
	void append(const tuple_cell &tc);
	const char *c_str();
	///get string as char*, valid only for mem_only buffers, also clears buffer
	///returned string must later be freed using delete[] by the caller
	char *get_str();
    static void free_str(char *str)
    {
        delete[] str;
    }

	~str_buf_base();

	str_buf_base& operator<<(const char *s) { append(s);  return *this; }
    str_buf_base& operator<<(char c)        { append(&c, 1); return *this; }
};

class op_str_buf : public str_buf_base
{
public:
    op_str_buf() : str_buf_base() {}
    op_str_buf(const tuple_cell &tc) : str_buf_base() { append(tc); }
    op_str_buf(const char *str) : str_buf_base() { append(str); }

    void set(const tuple_cell &tc) { clear(); append(tc); }
    void set(const char *str) { clear(); append(str); }

//  op_str_buf(const op_str_buf&) { throw USER_EXCEPTION2(SE1003, "Copy constructor for op_str_buf is not implemented"); }
    op_str_buf& operator=(const op_str_buf&) { throw USER_EXCEPTION2(SE1003, "Assign operator for op_str_buf is not implemented"); }
};

// string buffer for use in operations
// unlike op_stmt_buf it never overwrites strings created earlier
class stmt_str_buf_impl: public str_buf_base
{
public:
    stmt_str_buf_impl() : str_buf_base() {}
    stmt_str_buf_impl(int mem_only) : str_buf_base(true) {}
    stmt_str_buf_impl(const tuple_cell &tc) : str_buf_base() { append(tc); }
    stmt_str_buf_impl(const char *str) : str_buf_base() { append(str); }
    void set(const tuple_cell &tc) { clear(); append(tc); }
    void set(const char *str) { clear(); append(str); }

    tuple_cell get_tuple_cell() { return str_buf_base::get_tuple_cell(); }

//  stmt_str_buf_impl(const stmt_str_buf_impl&) { throw USER_EXCEPTION2(SE1003, "Copy constructor for stmt_str_buf_impl is not implemented"); }
    stmt_str_buf_impl& operator=(const stmt_str_buf_impl&) { throw USER_EXCEPTION2(SE1003, "Assign operator for stmt_str_buf_impl is not implemented"); }
};

// structure that can be used to construct strings
class stmt_str_buf
{
private:
	static stmt_str_buf_impl buf_impl;
	static bool used;
	stmt_str_buf_impl *m_impl;
public:
	static void reset() { U_ASSERT(!used); buf_impl.reset(); }

	stmt_str_buf() { if (used) throw USER_EXCEPTION(SE1003); m_impl = &buf_impl; used = true; U_ASSERT(!m_impl->mem_only()); }
	stmt_str_buf(int mem_only) { m_impl = se_new stmt_str_buf_impl(mem_only); U_ASSERT(m_impl->mem_only()); }
	~stmt_str_buf() { m_impl->clear(); if (m_impl->mem_only()) delete m_impl; else used = false; }

	void clear() {m_impl->clear();}
	void append(const tuple_cell &tc) {m_impl->append(tc);}
	void append(const char *str) {m_impl->append(str);}
	void append(const char *str, int add_len) {m_impl->append(str, add_len);}

	tuple_cell get_tuple_cell() {return m_impl->get_tuple_cell(); } //also clears
	///get string as char*, valid only for mem_only buffers, also clears buffer
	///returned string must later be freed using delete[] by the caller
	char *get_str() { return m_impl->get_str(); }

	stmt_str_buf(const stmt_str_buf&) { throw USER_EXCEPTION2(SE1003, "Copy constructor for stmt_str_buf is not implemented"); }
    stmt_str_buf& operator=(const stmt_str_buf&) { throw USER_EXCEPTION2(SE1003, "Assign operator for stmt_str_buf is not implemented"); }

	stmt_str_buf& operator<<(const char *s)	{ this->append(s);
                                                          return *this; }
    stmt_str_buf& operator<<(char c)		{ this->append(&c, 1); return *this; }
	stmt_str_buf& operator++() { return *this; }
	stmt_str_buf& operator++(int) { return *this; }
	stmt_str_buf& operator*() { return *this; }
	stmt_str_buf& operator=(char c) { this->append(&c, 1); return *this; }
};


class CharCounter
{
public:
	//parse sequence of bytes
	//returns full chars count, +1 if parsing ended in the middle of a character
	// (i.e. the number of first bytes of chars)
	virtual int count_chars(const char *str, str_off_t len) = 0;
};

class unicode_cp_iterator
{
public:
	static const int EOS = -1;
	//returns EOS if reached end of string
	virtual int get_next_char() = 0;
};

class CollationHandler;
class TokenizerResult
{
public:
    virtual ~TokenizerResult() {};
	virtual void get_next_result(xqp_tuple& t)=0;
};


class CharsetHandler
{
protected:
    CollationHandler *m_ucp_collation_handler;
    CharsetHandler(CollationHandler *ucp_collation_handler) : m_ucp_collation_handler(ucp_collation_handler) {}
public:
	//FIXME: length souldn't be int
	virtual str_off_t length (tuple_cell *tc) = 0;
	virtual void transtale (xqp_tuple &t, tuple_cell *arg, tuple_cell *map_str, tuple_cell *trans_str) = 0;
	virtual CharCounter* new_char_counter() = 0;
	virtual void free_char_counter(CharCounter *) = 0;
	virtual tuple_cell toupper(const tuple_cell *tc) = 0;
	virtual tuple_cell tolower(const tuple_cell *tc) = 0;
    virtual tuple_cell substring(const tuple_cell *tc, int64_t start_pos, int64_t length) = 0;
	//returns pointer to unicode_cp_iterator, it must then be released using delete operator.
	virtual unicode_cp_iterator *get_unicode_cp_iterator(const tuple_cell *tc) = 0;
	virtual unicode_cp_iterator *get_unicode_cp_iterator(const char * tc) = 0;

	virtual void replace (xqp_tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3, tuple_cell *t4) = 0;
	virtual void matches (xqp_tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3) = 0;
	virtual TokenizerResult* tokenize ( tuple_cell *t1, tuple_cell *t2, tuple_cell *t3) = 0;
	virtual bool matches (const tuple_cell *tc, const char *regex) = 0;
	virtual bool matches (const char *tc, const char *regex) = 0;

    inline CollationHandler *get_unicode_codepoint_collation()
    {
        return m_ucp_collation_handler;
    }
};

class CollationHandler
{
public:
	// compares 2 strings, return -1, 0 or 1
	// if function takes str_cursor argument, it's value is undefined after call
	virtual int compare(str_cursor *a, str_cursor *b) = 0;
	virtual int compare(str_cursor *a, const char *b) = 0;
	virtual int compare(const char *a, const char *b) = 0;

	// returns 'true' <-> given tuple_cell starts with 'prefix'
	virtual bool starts_with(const tuple_cell *tc, const tuple_cell *prefix) = 0;
	// returns 'true' <-> given tuple_cell ends with 'suffix'
	virtual bool ends_with(const tuple_cell *tc, const tuple_cell *suffix) = 0;

	// returns position of the first occurrence of 'subs' in 'src'
	// '-1' means that string does not contain given substring.
    virtual int contains(const tuple_cell *src, const tuple_cell  *subs) = 0;
};

extern CharsetHandler	*charset_handler;

void feed_tuple_cell(string_consumer_fn fn, void *p, const tuple_cell& tc);

void print_tuple_cell_dummy(se_ostream& crmout,const tuple_cell& tc);
inline void print_tuple_cell(se_ostream& crmout,const tuple_cell& tc)
{
    feed_tuple_cell(writextext_cb, &crmout, tc);
}

struct text_source_t text_source_tuple_cell(const tuple_cell& tc);

static inline
struct text_source_t text_source_node(const xptr node) {
    return CommonTextNode(node).getTextSource();
};

static inline
strsize_t get_text_size(struct text_source_t t) {
    if (t.type == text_source_t::text_pstrlong) {
        return pstr_long_bytelength2(t.u.data);
    } else {
        return t._text_size;
    }
}

static inline
bool empty_text(struct text_source_t t) {
    return (t.type != text_source_t::text_pstrlong) && (t._text_size == 0);
};

struct text_membuf_t {
  private:
    size_t size;
    char * cstr;

  public:
    text_membuf_t() : size(0), cstr(NULL) {};
    text_membuf_t(struct text_source_t ts) : size(0), cstr(NULL) {
        strsize_t sz = get_text_size(ts);

        if (sz > PAGE_SIZE) {
            throw USER_EXCEPTION2(SE1003, "Too long string to be copied to in-memory buffer");
        }

        size = (size_t) sz;
        cstr = (char *) malloc(size + 1);

        switch (ts.type) {
          case text_source_t::text_mem : {
            strncpy(cstr, ts.u.cstr, size);
          } break;
          case text_source_t::text_pstr : {
            xptr ptr = ts.u.data;
            CHECKP(ptr);
            memcpy(cstr, (char*) XADDR(ptr), (size_t) size);
          } break;
          case text_source_t::text_estr : {
            estr_copy_to_buffer(cstr, ts.u.data, get_text_size(ts));
          } break;
          default : throw USER_EXCEPTION2(SE1003, "Too long string to be copied to in-memory buffer");
        }
    };
    ~text_membuf_t() { if (cstr != NULL) { free(cstr); } };

    text_source_t getTextSource() const { return text_source_mem(cstr, size); };
    const char * getCstr() const { return cstr; }
    size_t getSize() const { return size; }
};

inline static
struct text_source_t text_source_strbuf(str_buf_base * buf) {
    struct text_source_t result = {};
    buf->fill_text_source(&result);

    return result;
}

inline static
str_cursor * get_text_cursor(const text_source_t text) {
    str_cursor * result = NULL;
    switch (text.type) {
    case text_source_t::text_mem: {
        result = new memstr_cursor((char *) text.u.cstr, (size_t) text._text_size);
    } break;
    case text_source_t::text_pstr: {
        result = new pstr_cursor(text.u.data, text._text_size);
    } break;
    case text_source_t::text_estr: {
        result = new estr_cursor(text.u.data, text._text_size);
    } break;
    case text_source_t::text_pstrlong: {
        result = new pstr_long_cursor(text.u.data);
    } break;
    }
    return result;
}

class TextBufferReader {
private:
    str_cursor * cursor;
    char in_buffer[PAGE_SIZE];
public:
    char * buffer;
    int size;

/* It turned out, that huge malloc is slow in this place, so we optimize it, moving it to stack from heap  */
    TextBufferReader(const text_source_t text) : cursor(get_text_cursor(text)), buffer(in_buffer) {};
    ~TextBufferReader() { delete cursor; };
/*
    TextBufferReader(const text_source_t text) : cursor(get_text_cursor(text)), buffer((char*) malloc(PAGE_SIZE)) {};
    ~TextBufferReader() { free(cursor); free(buffer); };
*/
    bool read() {
        /* ASSERTION: Returned value is bounded by PAGE_SIZE at most, so can be safely casted */
        return ((size = (int) cursor->copy_blk(buffer)) != 0);
    }
};


#endif /*_STRINGS_H */
