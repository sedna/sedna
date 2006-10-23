/*
 * File:  utf8.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "utf8.h"

//neeeded for pcre includes
#define PCRE_STATIC
#define SUPPORT_UTF8
#define SUPPORT_UCP

#include "pcre.h"
#include "char_iterator.h"
#include "e_string.h"
#include "e_string_iterator.h"
#include "e_string_o_iterator.h"
#include <sstream>

//////////////////////////////////////////////////////////////////////////
// Charset Handler
//////////////////////////////////////////////////////////////////////////

int CharsetHandler_utf8::length (tuple_cell *tc)
{
	switch (tc->get_type())
	{
	case tc_heavy_atomic_estr:
	case tc_heavy_atomic_pstr_short:
	{
		int len = 0;
		int bytes_left = tc->get_strlen_vmm();

		xptr cur_block_xptr;
		unsigned char *p;
		unsigned char *end;
		
		xptr data = tc->get_str_vmm();

		cur_block_xptr = BLOCKXPTR(data);
		p =(unsigned char*) XADDR(data);
		end = (unsigned char*) XADDR(cur_block_xptr) + PAGE_SIZE;
		if (end-p > bytes_left)
			end = p + bytes_left;

		while (1)
		{
			bytes_left -= end-p;
			while (p < end)
			{
				if ((*p < 0x80) || (*p > 0xBF))
					++len;
				//len +=  ((*p & 0xC0) != 0x80);
				++p;
			}
			if (bytes_left <= 0)
				break;
			//else:
			cur_block_xptr = E_STR_BLK_HDR(cur_block_xptr)->nblk;
			CHECKP(cur_block_xptr);
			p = (unsigned char*)XADDR(cur_block_xptr)+ sizeof(e_str_blk_hdr);
			end = (unsigned char*) XADDR(cur_block_xptr) + PAGE_SIZE;
			if (end-p > bytes_left)
				end = p + bytes_left;
		}
		
		return len;
	}
	case tc_light_atomic_fix_size: 
	case tc_light_atomic_var_size: 
	{
		unsigned char *p = (unsigned char *)tc->get_str_mem();
		unsigned char *end = p + tc->get_strlen_mem();
		int len = 0;
		while (p < end)
		{
			if ((*p < 0x80) || (*p > 0xBF))
				++len;
			++p;
		}
		return len;
	}
	case tc_heavy_atomic_pstr_long:
		return pstr_long_length(tc->get_str_vmm());
	}
}

struct mapent {
	int src;
	int dst;
	int ordr; //FIXME - get rid of it
};
static int mapent_cmp(const void *a, const void *b)
{
	return 
		(((mapent*)a)->src == ((mapent*)b)->src)
		  ? (((mapent*)a)->ordr - ((mapent*)b)->ordr)
		  : (((mapent*)a)->src - ((mapent*)b)->src);
}
//FIXME - 'int size'
static inline mapent *me_bsearch(mapent *map_arr, int size, int c)
{
	int l = 0;
	int r = size-1;
	int m;
	if (map_arr[0].src == c)
		return &map_arr[0];
	//l.src < c <= r.src
	while (r-l > 1)
	{
		m = (l+r) >> 1;
		const int v = map_arr[m].src;
		if (v < c)
			l = m;
		else
			r = m;
	}
	if (map_arr[r].src == c)
		return &map_arr[r];
	else
		return NULL;
}
void CharsetHandler_utf8::transtale (tuple &t, tuple_cell *arg, tuple_cell *map_str, tuple_cell *trans_str)
{
	int map_len = length(map_str);
	mapent *map_arr;
	//TODO - assert map_str & trans_str are light atomic
	char_iterator_utf8 map_it(map_str->get_str_mem(), map_str->get_strlen_mem(), 0);
	char_iterator_utf8 trans_it(trans_str->get_str_mem(), trans_str->get_strlen_mem(), 0);
	map_arr = (mapent *)malloc(sizeof(mapent) * map_len);
	//FIXME: int i/map_len is ok?
	for (int i = 0; i < map_len; i++)
	{
		//TODO - assert map_it is not at end yet

		map_arr[i].src = *map_it;
		map_arr[i].ordr = i;
		++map_it;
		if (trans_it.at_end())
			map_arr[i].dst = -1;
		else
		{
			map_arr[i].dst = *trans_it;
			++trans_it;
		}
	}
	//TODO - assert map_it.at_end()
	qsort(map_arr, map_len, sizeof(mapent), mapent_cmp);
	if (arg->is_light_atomic())
	{
		std::ostringstream st(std::ios::out | std::ios::binary);
		std::ostream_iterator<char> oi_raw(st);
		utf8_o_iterator<std::ostream_iterator<char> > oi(oi_raw);
		char_iterator_utf8 arg_it(arg->get_str_mem(), arg->get_strlen_mem(), 0);
		while (!arg_it.at_end())
		{
			int c = *arg_it;
			++arg_it;

			//FIXME - not very good to pass &c here
			mapent *m = me_bsearch(map_arr, map_len, c);

			if (m == NULL)
			{
				*oi = c;
				++oi;
			}
			else
				if (m->dst != -1)
				{
					*oi = m->dst;
					++oi;
				}

		}
		t.copy(tuple_cell::atomic_deep(xs_string,st.str().c_str()));
	}
	else
	{
		int arg_len = arg->get_strlen_vmm();
		xptr arg_data = arg->get_str_vmm();
		e_string_iterator_first start(arg_len, arg_data);
		e_string_iterator_first end(0, arg_data);
		utf8_iterator<e_string_iterator> arg_it(start);

		e_string_o_iterator<unsigned char> es_it;
		utf8_o_iterator<e_string_o_iterator<unsigned char> > oi(es_it);
		xptr start_pos = es_it.pos;

		while (arg_it.base_iterator() < end)
		{
			int c = *arg_it;
			++arg_it;

			//FIXME - not very good to pass &c here
			mapent *m = me_bsearch(map_arr, map_len, c);

			if (m == NULL)
			{
				*oi = c;
				++oi;
			}
			else
				if (m->dst != -1)
				{
					*oi = m->dst;
					++oi;
				}
		}

		//FIXME: int
		int len=get_length_of_last_str(start_pos);
		if (len > 0)
			len--;
		t.copy(tuple_cell::atomic_estr(xs_string,len,start_pos));
	}
	free(map_arr);
}


class CharCounter_utf8 : public CharCounter
{
public:
	CharCounter_utf8() {}

	virtual int count_chars(const char *str, int len);
};

int CharCounter_utf8::count_chars(const char *str, int len)
{
	int cnt = 0;
	while (--len >= 0)
	{
		if ((*str & 0xC0) != 0x80)
			++cnt;
		++str;
	}
	return cnt;
}

CharCounter* CharsetHandler_utf8::new_char_counter()
{
	return new CharCounter_utf8();
}

void CharsetHandler_utf8::free_char_counter(CharCounter *char_counter)
{
	delete char_counter;
}

//////////////////////////////////////////////////////////////////////////
// Collation Handler
//////////////////////////////////////////////////////////////////////////

static int get_pcre_flags(tuple_cell *t)
{
	int flags = 0;
	if (t == NULL)
		return flags;
	if (t->is_eos())
		return flags;

	char* str=t->get_str_mem();
	int str_len = t->get_strlen_mem();

	//FIXME - this part may depend on charset!
	// but pcre can only be used for one-byte encodings (with ASCII in 1st half?)
	// and utf8, so this should be ok here
	for (int i = 0; i < str_len; i++)
	{
		switch (str[i])
		{
			case 's': //dot-all mode
				flags |= PCRE_DOTALL;
				break;
			case 'm':
				flags |= PCRE_MULTILINE;
				break;
			case 'i':
				flags |= PCRE_CASELESS;
				break;
			case 'x':
				flags |= PCRE_EXTENDED;
				break;
			default:
				throw USER_EXCEPTION(FORX0001);
		}
	}

	return flags;
}

//TODO!!!!: use some sort of output buffer!
template <class Iterator>
static inline void utf8_replace(const Iterator &start, const Iterator &end, tuple &t, const PcrePattern &re, tuple_cell *t3, tuple_cell *t4)
{
	int match_flags = PCRE_NO_UTF8_CHECK;

	PcreMatcher<Iterator> matcher(re);
	e_string_o_iterator<unsigned char> es_it;
	xptr start_pos = es_it.pos;

	matcher.replaceAll(es_it, start, end, start, t3->get_str_mem(), match_flags);

	//FIXME: int
	int len=get_length_of_last_str(start_pos);
	if (len > 0)
		len--;
	t.copy(tuple_cell::atomic_estr(xs_string,len,start_pos));
}

void CollationHandler_utf8::replace (tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3, tuple_cell *t4)
{
	try
	{
	PcrePattern re(t2->get_str_mem(), PCRE_UTF8 | PCRE_NO_UTF8_CHECK | get_pcre_flags(t4));
	STRING_ITERATOR_CALL_TEMPLATE_1tcptr_4p(utf8_replace, t1, t, re, t3, t4);
	}
	catch (const PcreCompileException &e)
	{
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
	catch (const PcreEmptyStringMatchedException &e)
	{
		throw USER_EXCEPTION2(FORX0003, e.what());
	}
	catch (const PcreException &e)
	{
		//FIXME FORX0002 - is not ok here
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
	catch (const std::exception &e)
	{
		//FIXME FORX0002 - is not ok here
		//FIXME: is this barch possible??
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
}

template<class Iterator>
static inline void utf8_matches (const Iterator &start, const Iterator &end, tuple &t, const PcrePattern &re, tuple_cell *t3)
{
	int match_flags = PCRE_NO_UTF8_CHECK;
	PcreMatcher<Iterator> matcher(re);

	t.copy(tuple_cell::atomic(matcher.matches(start, end, start, match_flags)));
}

void CollationHandler_utf8::matches (tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3)
{
	try
	{
	PcrePattern re(t2->get_str_mem(), PCRE_UTF8 | PCRE_NO_UTF8_CHECK | get_pcre_flags(t3));
	STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(utf8_matches, t1, t, re, t3);
	}
	catch (const PcreCompileException &e)
	{
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
	catch (const std::exception &e)
	{
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
}

template<class Iterator>
static inline void utf8_matches_bool (const Iterator &start, const Iterator &end, const PcrePattern &re, bool *res)
{
	int match_flags = PCRE_NO_UTF8_CHECK;
	PcreMatcher<Iterator> matcher(re);

	*res = matcher.matches(start, end, start, match_flags);
}

bool CollationHandler_utf8::matches (const tuple_cell *tc, const char *regex)
{
	try
	{
		PcrePattern re(regex, PCRE_UTF8 | PCRE_NO_UTF8_CHECK);
		bool res;
		STRING_ITERATOR_CALL_TEMPLATE_1tcptr_2p(utf8_matches_bool, tc, re, &res);
		return res;
	}
	catch (const PcreCompileException &e)
	{
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
	catch (const std::exception &e)
	{
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
}

int utf8_parse_char(const char *str, int *byte_len)
{
	//FIXME: this works bad with incorrent utf-8 strings
	int r;

	if (((unsigned char *)str)[0] < 128)
	{
		r = ((unsigned char *)str)[0];
		if (byte_len)
			*byte_len = 1;
	}
	else if (((unsigned char *)str)[0] < 224) // ch mustbe >= 192
	{
		r = ((unsigned char *)str)[0] - 192; r <<= 6;
		r += ((unsigned char *)str)[1] - 128;
		if (byte_len)
			*byte_len = 2;
	}
	else if (((unsigned char *)str)[0] < 240)
	{
		r = (((unsigned char *)str)[0]-224); r <<= 6;
		r += ((unsigned char *)str)[1] - 128; r <<= 6;
		r += ((unsigned char *)str)[2] - 128;
		if (byte_len)
			*byte_len = 3;
	}
	else if (((unsigned char *)str)[0] < 248)
	{
		r = (((unsigned char *)str)[0]-240); r <<= 6;
		r += ((unsigned char *)str)[1] - 128; r <<= 6;
		r += ((unsigned char *)str)[2] - 128; r <<= 6;
		r += ((unsigned char *)str)[3] - 128;
		if (byte_len)
			*byte_len = 4;
	}
	else if (((unsigned char *)str)[0] < 252)
	{
		r = (((unsigned char *)str)[0]-248); r <<= 6;
		r += ((unsigned char *)str)[1] - 128; r <<= 6;
		r += ((unsigned char *)str)[2] - 128; r <<= 6;
		r += ((unsigned char *)str)[3] - 128; r <<= 6;
		r += ((unsigned char *)str)[4] - 128;
		if (byte_len)
			*byte_len = 5;
	}
	else // ch mustbe < 254
	{
		r = (((unsigned char *)str)[0]-252); r <<= 6;
		r += ((unsigned char *)str)[1] - 128; r <<= 6;
		r += ((unsigned char *)str)[2] - 128; r <<= 6;
		r += ((unsigned char *)str)[3] - 128; r <<= 6;
		r += ((unsigned char *)str)[4] - 128; r <<= 6;
		r += ((unsigned char *)str)[5] - 128;
		if (byte_len)
			*byte_len = 6;
	}
	return r;
}


const char *utf8_encode_char(int c)
{
	static char rbuf[5];
	if (c < (1 << 7)) {
		rbuf[0] = c;
		rbuf[1] = 0;
	} else if (c < (1 << 11)) {
		rbuf[0] = ((c >> 6) | 0xc0);
		rbuf[1] = ((c & 0x3f) | 0x80);
		rbuf[2] = 0;
	} else if (c < (1 << 16)) {
		rbuf[0] = ((c >> 12) | 0xe0);
		rbuf[1] = (((c >> 6) & 0x3f) | 0x80);
		rbuf[2] = ((c & 0x3f) | 0x80);
		rbuf[3] = 0;
	} else if (c < (1 << 21)) {
		rbuf[0] = ((c >> 18) | 0xf0);
		rbuf[1] = (((c >> 12) & 0x3f) | 0x80);
		rbuf[2] = (((c >> 6) & 0x3f) | 0x80);
		rbuf[3] = ((c & 0x3f) | 0x80);
		rbuf[4] = 0;
	}
	return rbuf;
}

