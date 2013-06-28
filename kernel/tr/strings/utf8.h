/*
 * File:  utf8.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _UTF8_H
#define _UTF8_H

#include "common/sedna.h"
#include "tr/strings/strings.h"

class CollationHandler_utf8 : public CollationHandler
{
public:
	// compares 2 strings, return -1, 0 or 1
	// if function takes str_cursor argument, it's value is undefined after call
	int compare(str_cursor *a, str_cursor *b);
	int compare(str_cursor *a, const char *b);
	int compare(const char *a, const char *b);

    bool starts_with(const tuple_cell *tc, const tuple_cell* prefix);
    bool ends_with(const tuple_cell *tc, const tuple_cell* suffix);
    int contains(const tuple_cell *src, const tuple_cell  *subs);
};
//FIXME: xs_uri.cpp uses pcre directry!, if charsets other than utf8 are to be supported this must be fixed.
class CharsetHandler_utf8 : public CharsetHandler
{
private:
    CollationHandler_utf8 m_ch;
public:
    CharsetHandler_utf8() : CharsetHandler(&m_ch) {}
	virtual str_off_t length (tuple_cell *tc);
	virtual void transtale (xqp_tuple &t, tuple_cell *arg, tuple_cell *map_str, tuple_cell *trans_str);
	virtual CharCounter* new_char_counter();
	virtual void free_char_counter(CharCounter *);
	virtual tuple_cell toupper(const tuple_cell *tc);
	virtual tuple_cell tolower(const tuple_cell *tc);
    virtual tuple_cell substring(const tuple_cell *tc, int64_t start_pos, int64_t length);
    virtual unicode_cp_iterator *get_unicode_cp_iterator(const tuple_cell *tc);
    virtual unicode_cp_iterator *get_unicode_cp_iterator(const char *str);

	//all tuple cells must be strings
	virtual void replace (xqp_tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3, tuple_cell *t4);
	virtual void matches (xqp_tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3);
	virtual TokenizerResult* tokenize ( tuple_cell *t1, tuple_cell *t2, tuple_cell *t3);
	virtual bool matches (const tuple_cell *t1, const char *regex);
	virtual bool matches (const char *t1, const char *regex);

	//put utf8 char to buf
	//buf_p is position in buffer where to write, and is updated according to number of the bytes written
	//return true if ok or false if not enough space in buffer
	//if ch >= (1 << 21), returns true and doesnt write anything to buffer
	//FIXME: what should it return and do if (ch < 0)?
	//FIXME: move to cpp
	static inline bool utf8_putch(int ch, char *buf, int *buf_p, const int buf_size)
	{
		if (ch < (1 << 7)) {
			if (*buf_p+1 > buf_size) return false;
			buf[(*buf_p)++] = ch;
		} else if (ch < (1 << 11)) {
			if (*buf_p+2 > buf_size) return false;
			buf[(*buf_p)++] = ((ch >> 6) | 0xc0);
			buf[(*buf_p)++] = ((ch & 0x3f) | 0x80);
		} else if (ch < (1 << 16)) {
			if (*buf_p+3 > buf_size) return false;
			buf[(*buf_p)++] = ((ch >> 12) | 0xe0);
			buf[(*buf_p)++] = (((ch >> 6) & 0x3f) | 0x80);
			buf[(*buf_p)++] = ((ch & 0x3f) | 0x80);
		} else if (ch < (1 << 21)) {
			if (*buf_p+4 > buf_size) return false;
			buf[(*buf_p)++] = ((ch >> 18) | 0xf0);
			buf[(*buf_p)++] = (((ch >> 12) & 0x3f) | 0x80);
			buf[(*buf_p)++] = (((ch >> 6) & 0x3f) | 0x80);
			buf[(*buf_p)++] = ((ch & 0x3f) | 0x80);
		}
		return true;
	}
};



template <class BaseIterator, typename UChar32 = int>
class utf8_iterator : public std::iterator<std::bidirectional_iterator_tag, UChar32> 
{
private:
	typename std::iterator<std::bidirectional_iterator_tag, UChar32>::value_type cur_c;
	bool has_c;
	BaseIterator it;
public:	

	utf8_iterator() : it() {}
	utf8_iterator(BaseIterator _it_) : it(_it_) {}
	utf8_iterator(const utf8_iterator &_i_) { *this = _i_; }
	~utf8_iterator() {}

	inline BaseIterator base_iterator()
	{
		return it;
	}

	inline typename std::iterator<std::bidirectional_iterator_tag, UChar32>::value_type operator*()
	{
		unsigned char ch=*(it);
		typename std::iterator<std::bidirectional_iterator_tag, UChar32>::value_type r;

		if (ch < 128)
		{
			r = ch;
		}
		else if (ch < 224) //FIXME: ch mustbe >= 192
		{
			r = ch - 192; r <<= 6;
			BaseIterator tmp(it);
			++(tmp); ch = *(tmp); r += ch - 128;
		}
		else if (ch < 240)
		{
			r = ch - 224; r <<= 6;
			BaseIterator tmp(it);
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128;
		}
		else if (ch < 248)
		{
			r = ch - 240; r <<= 6;
			BaseIterator tmp(it);
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128;
		}
		else if (ch < 252)
		{
			r = ch - 248; r <<= 6;
			BaseIterator tmp(it);
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128;
		}
		else // ch mustbe < 254
		{
			r = ch - 252; r <<= 6;
			BaseIterator tmp(it);
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128; r <<= 6;
			++(tmp); ch = *(tmp); r += ch - 128;
		}
		return r;
	}

	utf8_iterator& operator ++()
	{
		unsigned char ch=*(it);

		++(it);
		if (ch >= 192)
		{
			++(it);
			if (ch >= 224)
			{
				++(it);
				if (ch >= 240)
				{
					++(it);
					if (ch >= 248)
					{
						++(it);
						if (ch >= 252)
							++(it);
					}
				}
			}
		}
		return *this;
	}

	utf8_iterator& operator --()
	{
		--(it);
		unsigned char ch=*(it);
		while (ch >= 128 && ch < 192)
		{
			--(it);
			ch=*(it);
		}
		return *this;
	}

	utf8_iterator& operator =(const utf8_iterator& _i_)
	{
		this->it = _i_.it;
		return (*this);
	}

	inline bool operator ==(const utf8_iterator& it2) const
	{
		return this->it == it2.it;
	}
	inline bool operator !=(const utf8_iterator& it2) const
	{
		return this->it != it2.it;
	}
	inline bool operator > (const utf8_iterator<BaseIterator>& it2) const
	{
		return this->it > it2.it;
	}
	inline bool operator >=(const utf8_iterator<BaseIterator>& it2) const
	{
		return this->it >= it2.it;
	}
	inline bool operator < (const utf8_iterator<BaseIterator>& it2) const
	{
		return this->it < it2.it;
	}
	inline bool operator <=(const utf8_iterator<BaseIterator>& it2) const
	{
		return this->it <= it2.it;
	}

};
template <>
class utf8_iterator<char *, int> : public std::iterator<std::bidirectional_iterator_tag, int> 
{
private:
	unsigned char* m_begin;
	int size;
	int shift;
public:
	utf8_iterator(char* _begin_, int _size_,int _shift_) :
	  m_begin((unsigned char*)_begin_) ,size(_size_) ,shift(_shift_)
	  {
	  }
	  utf8_iterator() : m_begin(NULL) ,size(0) ,shift(0) {}

	  bool at_end() {
		  return (shift >= size);
	  }

	  int operator*() const { 
		  unsigned char ch = *(m_begin+shift);
		  int r;

		  if (ch < 128)
		  {
			  r = ch;
		  }
		  else if (ch < 224) // ch mustbe >= 192
		  {
			  r = ch - 192; r <<= 6;
			  r += *(m_begin+shift+1) - 128;
		  }
		  else if (ch < 240)
		  {
			  r = (ch-224); r <<= 6;
			  r += *(m_begin+shift+1) - 128; r <<= 6;
			  r += *(m_begin+shift+2) - 128;
		  }
		  else if (ch < 248)
		  {
			  r = (ch-240); r <<= 6;
			  r += *(m_begin+shift+1) - 128; r <<= 6;
			  r += *(m_begin+shift+2) - 128; r <<= 6;
			  r += *(m_begin+shift+3) - 128;
		  }
		  else if (ch < 252)
		  {
			  r = (ch-248); r <<= 6;
			  r += *(m_begin+shift+1) - 128; r <<= 6;
			  r += *(m_begin+shift+2) - 128; r <<= 6;
			  r += *(m_begin+shift+3) - 128; r <<= 6;
			  r += *(m_begin+shift+4) - 128;
		  }
		  else // ch mustbe < 254
		  {
			  r = (ch-252); r <<= 6;
			  r += *(m_begin+shift+1) - 128; r <<= 6;
			  r += *(m_begin+shift+2) - 128; r <<= 6;
			  r += *(m_begin+shift+3) - 128; r <<= 6;
			  r += *(m_begin+shift+4) - 128; r <<= 6;
			  r += *(m_begin+shift+5) - 128;
		  }
		  return r;
	  }

	  //char operator[](int i) { return s->get(i); }
	  inline utf8_iterator<char *, int>& operator ++() {
		  unsigned char ch = *(m_begin+shift);
		  shift++;
		  if (ch >= 192)
		  {
			  shift++;
			  if (ch >= 224)
			  {
				  shift++;
				  if (ch >= 240)
				  {
					  shift++;
					  if (ch >= 248)
					  {
						  shift++;
						  if (ch >= 252)
							  shift++;
					  }
				  }
			  }
		  }
		  return *this;
	  }
	  inline utf8_iterator<char *, int>  operator ++(int) { int old_shift = shift; ++(*this); return utf8_iterator<char *, int>((char*)m_begin,size,old_shift); }
	  inline utf8_iterator<char *, int>& operator --(){
		  shift--;
		  while (m_begin[shift] >= 128 && m_begin[shift] < 192)
			  shift--;

		  return *this;
	  }
	  inline utf8_iterator<char *, int>  operator --(int) { int old_shift = shift; --(*this); return utf8_iterator<char *, int>((char*)m_begin,size,old_shift); }

	  inline bool operator ==(const utf8_iterator<char *, int>& it2) const
	  {
		  return (this->m_begin==it2.m_begin)&&(this->size==it2.size)&&(this->shift==it2.shift);
	  }
	  inline bool operator !=(const utf8_iterator<char *, int>& it2) const
	  {
		  return !((*this)==it2);
	  }
	  inline bool operator > (const utf8_iterator<char *, int>& it2) const
	  {
		  return ((uintptr_t)(this->m_begin+this->shift)>(uintptr_t)(it2.m_begin+it2.shift));
	  }
	  inline bool operator >=(const utf8_iterator<char *, int>& it2) const
	  {
		  return ((uintptr_t)(this->m_begin+this->shift)>=(uintptr_t)(it2.m_begin+it2.shift));
	  }
	  inline bool operator < (const utf8_iterator<char *, int>& it2) const
	  {
		  return ((uintptr_t)(this->m_begin+this->shift)<(uintptr_t)(it2.m_begin+it2.shift));
	  }
	  inline bool operator <=(const utf8_iterator<char *, int>& it2) const
	  {
		  return ((uintptr_t)(this->m_begin+this->shift)<=(uintptr_t)(it2.m_begin+it2.shift));
	  }
};

//FIXME: remove this and use 'utf8_iterator<char *, int>' explicitly?
typedef utf8_iterator<char *, int> char_iterator_utf8;

template <class uchar_o_iterator>
class utf8_o_iterator 
{
	uchar_o_iterator *it;
public:
	inline utf8_o_iterator(const utf8_o_iterator& x) : it(x.it)
	{
	}
	inline utf8_o_iterator& operator=(const utf8_o_iterator& x)
	{
		it=x.it;
		return *this;
	}
	inline utf8_o_iterator& operator=(const int t)
	{
		if (t < (1 << 7)) {
			**it = t; ++*it;
		} else if (t < (1 << 11)) {
			**it = ((t >> 6) | 0xc0); ++*it;
			**it = ((t & 0x3f) | 0x80); ++*it;
		} else if (t < (1 << 16)) {
			**it = ((t >> 12) | 0xe0); ++*it;
			**it = (((t >> 6) & 0x3f) | 0x80); ++*it;
			**it = ((t & 0x3f) | 0x80); ++*it;
		} else if (t < (1 << 21)) {
			**it = ((t >> 18) | 0xf0);++*it;
			**it = (((t >> 12) & 0x3f) | 0x80);++*it;
			**it = (((t >> 6) & 0x3f) | 0x80);++*it;
			**it = ((t & 0x3f) | 0x80); ++*it;
		}
		return *this;
	}
	inline utf8_o_iterator& operator*()
	{
		return *this;
	}
	inline utf8_o_iterator& operator++()
	{
		return *this;
	}
	inline utf8_o_iterator operator++(int)
	{
		return *this;
	}
	inline utf8_o_iterator(uchar_o_iterator &_it_) : it(&_it_) {}
};



int utf8_parse_char(const char *str, int *byte_len = NULL);
const char *utf8_encode_char(int c);

char* utf8_valid(const char *string, size_t length);



class CollationManager
{
private:
    CharsetHandler_utf8 utf8_charset_handler;
public:
    CollationManager() { charset_handler =  &utf8_charset_handler; }
    // returns NULL if there is no collation handler for such uri
	CollationHandler *get_collation_handler(const char *uri);
	CollationHandler *get_default_collation_handler();
};

#define UTF8_EOF -1

//read char from buffer *buf, *buf and *len are changed accordingly
//does not work if buf contains incomplete chars
inline int utf8_getch(const char **buf, int *len)
{
	unsigned char ch = *(unsigned char *)*buf;
	int r;

	if (*len < 1)
		return UTF8_EOF;

	//FIXME - check len

	++(*buf);
	if (ch < 128)
	{
			r = ch;
			*len -= 1;
	}
	else if (ch < 224) //FIXME: ch mustbe >= 192
	{
		r = ch - 192; r <<= 6;
		ch = *(unsigned char *)*buf; r += ch - 128; ++(*buf);
		*len -= 2;
	}
	else if (ch < 240)
	{
		r = ch - 224; r <<= 6;
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; ++(*buf);
		*len -= 3;
	}
	else if (ch < 248)
	{
		r = ch - 240; r <<= 6;
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; ++(*buf);
		*len -= 4;
	}
	else if (ch < 252)
	{
		r = ch - 248; r <<= 6;
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; ++(*buf);
		*len -= 5;
	}
	else // ch mustbe < 254
	{
		r = ch - 252; r <<= 6;
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; r <<= 6; ++(*buf);
		ch = *(unsigned char *)*buf; r += ch - 128; ++(*buf);
		*len -= 6;
	}
	return r;
}


#endif
