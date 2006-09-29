/*
 * File:  utf8.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "strings.h"

class CharsetHandler_utf8 : public CharsetHandler
{
public:
	virtual int length (tuple_cell *tc);
	virtual void transtale (tuple &t, tuple_cell *arg, tuple_cell *map_str, tuple_cell *trans_str);
	virtual CharCounter* new_char_counter();
	virtual void free_char_counter(CharCounter *);
};

class CollationHandler_utf8 : public CollationHandler
{
public:
	//all tuple cells must be strings
	virtual void replace (tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3, tuple_cell *t4);
	virtual void matches (tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3);
	virtual bool matches (const tuple_cell *t1, const char *regex);
};


class char_iterator_utf8
{
private:

	inline friend bool operator ==(const char_iterator_utf8& it1, const char_iterator_utf8& it2)
	{
		return (it1.begin==it2.begin)&&(it1.size==it2.size)&&(it1.shift==it2.shift);
	}
	inline friend bool operator !=(const char_iterator_utf8& it1, const char_iterator_utf8& it2)
	{
		return !(it1==it2);
	}
	inline friend bool operator > (const char_iterator_utf8& it1, const char_iterator_utf8& it2)
	{
		return ((int)(it1.begin+it1.shift)>(int)(it2.begin+it2.shift));
	}
	inline friend bool operator >=(const char_iterator_utf8& it1, const char_iterator_utf8& it2)
	{
		return ((int)(it1.begin+it1.shift)>=(int)(it2.begin+it2.shift));
	}
	inline friend bool operator < (const char_iterator_utf8& it1, const char_iterator_utf8& it2)
	{
		return ((int)(it1.begin+it1.shift)<(int)(it2.begin+it2.shift));
	}
	inline friend bool operator <=(const char_iterator_utf8& it1, const char_iterator_utf8& it2)
	{
		return ((int)(it1.begin+it1.shift)<=(int)(it2.begin+it2.shift));
	}
	unsigned char* begin;
	int size;
	int shift;
public:
	char_iterator_utf8(char* _begin_, int _size_,int _shift_) :
	  begin((unsigned char*)_begin_) ,size(_size_) ,shift(_shift_)
	  {

	  }
	  char_iterator_utf8() : begin(NULL) ,size(0) ,shift(0) {}

	  bool at_end() {
		  return (shift >= size);
	  }

	  int operator*() const { 
		  unsigned char ch = *(begin+shift);
		  int r;

		  if (ch < 128)
		  {
			  r = ch;
		  }
		  else if (ch < 224) // ch mustbe >= 192
		  {
			  r = ch - 192; r <<= 6;
			  r += *(begin+shift+1) - 128;
		  }
		  else if (ch < 240)
		  {
			  r = (ch-224); r <<= 6;
			  r += *(begin+shift+1) - 128; r <<= 6;
			  r += *(begin+shift+2) - 128;
		  }
		  else if (ch < 248)
		  {
			  r = (ch-240); r <<= 6;
			  r += *(begin+shift+1) - 128; r <<= 6;
			  r += *(begin+shift+2) - 128; r <<= 6;
			  r += *(begin+shift+3) - 128;
		  }
		  else if (ch < 252)
		  {
			  r = (ch-248); r <<= 6;
			  r += *(begin+shift+1) - 128; r <<= 6;
			  r += *(begin+shift+2) - 128; r <<= 6;
			  r += *(begin+shift+3) - 128; r <<= 6;
			  r += *(begin+shift+4) - 128;
		  }
		  else // ch mustbe < 254
		  {
			  r = (ch-252); r <<= 6;
			  r += *(begin+shift+1) - 128; r <<= 6;
			  r += *(begin+shift+2) - 128; r <<= 6;
			  r += *(begin+shift+3) - 128; r <<= 6;
			  r += *(begin+shift+4) - 128; r <<= 6;
			  r += *(begin+shift+5) - 128;
		  }
		  return r;
	  }

	  //char operator[](int i) { return s->get(i); }
	  inline char_iterator_utf8& operator ++() {
		  unsigned char ch = *(begin+shift);
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
	  inline char_iterator_utf8  operator ++(int) { int old_shift = shift; ++(*this); return char_iterator_utf8((char*)begin,size,old_shift); }
	  inline char_iterator_utf8& operator --(){
		  shift--;
		  while (begin[shift] >= 128 && begin[shift] < 192)
			  shift--;

		  return *this;
	  }
	  inline char_iterator_utf8  operator --(int) { int old_shift = shift; --(*this); return char_iterator_utf8((char*)begin,size,old_shift); }
};


const char *utf8_encode_char(int c);

