/*
 * File:  utf8.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/strings/utf8.h"

//neeeded for pcre includes
#define PCRE_STATIC
#define SUPPORT_UTF8
#define SUPPORT_UCP

#include "pcre/pcre.h"
#include "tr/strings/char_iterator.h"
#include "tr/strings/e_string.h"
#include "tr/strings/e_string_iterator.h"
#include <sstream>
#include "tr/executor/base/PPBase.h"
#include "tr/executor/xqops/PPSubsMatch.h"


//////////////////////////////////////////////////////////////////////////
// Charset Handler
//////////////////////////////////////////////////////////////////////////


str_off_t CharsetHandler_utf8::length (tuple_cell *tc)
{
	switch (tc->get_type())
	{
	case tc_heavy_atomic_estr:
	case tc_heavy_atomic_pstr_short:
	{
		str_off_t len = 0;
		str_off_t bytes_left = tc->get_strlen_vmm();

		xptr cur_block_xptr;
		unsigned char *p;
		unsigned char *end;

		xptr data = tc->get_str_vmm();
		CHECKP(data);

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
    default:
        throw USER_EXCEPTION2(SE1003, "impossible case in CharsetHandler_utf8::length");
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

	if(map_arr == NULL) return NULL;

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

template <class Iterator>
static inline void utf8_translate_proc(const Iterator &start, const Iterator &end, tuple &t, mapent *map_arr, int map_len)
{
	utf8_iterator<Iterator> arg_it(start);

	stmt_str_buf out_it;
	utf8_o_iterator<stmt_str_buf> oi(out_it);
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

	t.copy(out_it.get_tuple_cell());
}

void CharsetHandler_utf8::transtale (tuple &t, tuple_cell *arg, tuple_cell *map_str, tuple_cell *trans_str)
{
	const str_off_t _map_len = length(map_str);
	U_ASSERT(_map_len >= 0 && _map_len < INT_MAX);
	int map_len = (int)_map_len;
	mapent *map_arr = NULL;
	//TODO - assert map_str & trans_str are light atomic
	char_iterator_utf8 map_it(map_str->get_str_mem(), map_str->get_strlen_mem(), 0);
	char_iterator_utf8 trans_it(trans_str->get_str_mem(), trans_str->get_strlen_mem(), 0);
	//NOTE - in FreeBSD malloc(0) returns valid pointer by default
	if(map_len)
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
		stmt_str_buf out_it;
		utf8_o_iterator<stmt_str_buf > oi(out_it);
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
		t.copy(out_it.get_tuple_cell());
	}
	else
	{
		STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(utf8_translate_proc, arg, t, map_arr, map_len);
	}
	free(map_arr);
}


class CharCounter_utf8 : public CharCounter
{
public:
	CharCounter_utf8() {}

	virtual int count_chars(const char *str, str_off_t len);
};

int CharCounter_utf8::count_chars(const char *str, str_off_t len)
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
	return se_new CharCounter_utf8();
}

void CharsetHandler_utf8::free_char_counter(CharCounter *char_counter)
{
	delete char_counter;
}

//FIXME1: do not include this
//FIXME2: it is already included in pcre_matcher_base.h
//#include "ucp.c"

template <class Iterator>
static inline void utf8_toupper(const Iterator &start, const Iterator &end, stmt_str_buf *sb)
{
	utf8_o_iterator<stmt_str_buf> outp(*sb);
	utf8_iterator<Iterator> it(start);

	while (it.base_iterator() < end)
	{
		const int c = *it;
		int c_type = 0, c_case = 0;
		++it;
		ucp_findchar(c, &c_type, &c_case);
		if (c_case != 0 && c_type == ucp_Ll)
			*outp++ = c_case;
		else
			*outp++ = c;
	}
}


tuple_cell CharsetHandler_utf8::toupper(const tuple_cell *tc)
{
	stmt_str_buf sb;

	STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(utf8_toupper, tc, &sb);

	return sb.get_tuple_cell();
}

template <class Iterator>
static inline void utf8_tolower(const Iterator &start, const Iterator &end, stmt_str_buf *sb)
{
	utf8_o_iterator<stmt_str_buf> outp(*sb);
	utf8_iterator<Iterator> it(start);

	while (it.base_iterator() < end)
	{
		const int c = *it;
		int c_type = 0, c_case = 0;
		++it;
		ucp_findchar(c, &c_type, &c_case);
		if (c_case != 0 && c_type == ucp_Lu)
			*outp++ = c_case;
		else
			*outp++ = c;
	}
}


tuple_cell CharsetHandler_utf8::tolower(const tuple_cell *tc)
{
	stmt_str_buf sb;

	STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(utf8_tolower, tc, &sb);

	return sb.get_tuple_cell();
}

template <class Iterator>
static inline void utf8_substring(const Iterator &start, const Iterator &end, stmt_str_buf *sb, int64_t start_pos, int64_t length)
{
	utf8_o_iterator<stmt_str_buf> outp(*sb);
	utf8_iterator<Iterator> it(start);

    while (it.base_iterator() < end && start_pos > 0)
    {
        start_pos--;
        ++it;
    }

	while (it.base_iterator() < end && length > 0)
	{
		const int c = *it;
		++it;
	    *outp++ = c;
        length--;
	}
}
tuple_cell CharsetHandler_utf8::substring(const tuple_cell *tc, int64_t start_pos, int64_t length)
{
	stmt_str_buf sb;

	STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(utf8_substring, tc, &sb, start_pos, length);

	return sb.get_tuple_cell();
}

template <class Iterator>
class utf8_unicode_cp_iterator : public unicode_cp_iterator
{
private:
	utf8_iterator<Iterator> it;
	Iterator end;
public:
	utf8_unicode_cp_iterator(Iterator _start_, Iterator _end_) : it(_start_), end(_end_) {}
	virtual int get_next_char();
};
template <class Iterator>
int utf8_unicode_cp_iterator<Iterator>::get_next_char()
{
	if (it < end)
	{
		int res = *it;
		++it;
		return res;
	}
	else
		return EOS;
}
template <class Iterator>
static inline void utf8_get_unicode_cp_iterator(const Iterator &start, const Iterator &end, unicode_cp_iterator **res)
{
	*res = se_new utf8_unicode_cp_iterator<Iterator>(start, end);
}

unicode_cp_iterator *CharsetHandler_utf8::get_unicode_cp_iterator(const tuple_cell *tc)
{
	unicode_cp_iterator *res;
	STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(utf8_get_unicode_cp_iterator, tc, &res);
	return res;
}

unicode_cp_iterator *CharsetHandler_utf8::get_unicode_cp_iterator(const char *str)
{
	unicode_cp_iterator *res;
	utf8_get_unicode_cp_iterator((const unsigned char *)str, (const unsigned char *)str+strlen(str), &res);
	return res;
}


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

	PcreMatcher<Iterator, typename Iterator::off_t> matcher(re);
	stmt_str_buf out_it;

	matcher.replaceAll(out_it, start, end, start, t3->get_str_mem(), match_flags);

	t.copy(out_it.get_tuple_cell());
}

void CharsetHandler_utf8::replace (tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3, tuple_cell *t4)
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
	catch (const PcreBadFormatException &e)
	{
		throw USER_EXCEPTION2(FORX0004, e.what());
	}
	catch (const PcreException &e)
	{
		//FIXME FORX0002 - is not ok here
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
}

template<class Iterator>
static inline void utf8_matches (const Iterator &start, const Iterator &end, tuple &t, const PcrePattern &re, tuple_cell *t3)
{
	int match_flags = PCRE_NO_UTF8_CHECK;
	PcreMatcher<Iterator, typename Iterator::off_t> matcher(re);

	t.copy(tuple_cell::atomic(matcher.matches(start, end, start, match_flags)));
}

void CharsetHandler_utf8::matches (tuple &t, tuple_cell *t1, tuple_cell *t2, tuple_cell *t3)
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
	catch (const PcreException &e)
	{
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
}


template <class Iterator>
class utf8_tokenize_result : public TokenizerResult
{
private:
	tuple_cell str_tc; //need to keep it so that counted ptr in original tuple_cell won't reach 0 until utf8_tokenize_result is destroyed
	Iterator start, end, pos;
	bool ret_empty;
	PcrePattern re;
public:
	utf8_tokenize_result(Iterator _start_, Iterator _end_, tuple_cell *t2, tuple_cell *t3, tuple_cell *_str_tc_) : str_tc(*_str_tc_), 
                                                                                                                   start(_start_), 
                                                                                                                   end(_end_), 
                                                                                                                   pos(_start_), 
                                                                                                                   ret_empty(false),
                                                                                                                   re(t2->get_str_mem(), PCRE_UTF8 | PCRE_NO_UTF8_CHECK | get_pcre_flags(t3))

	{
		PcreMatcher<const unsigned char *, ptrdiff_t>m(re);
		const unsigned char * x = (const unsigned char *)"";
		if (m.matches(x, x+1, x, PCRE_NO_UTF8_CHECK))
			throw USER_EXCEPTION(FORX0003);
	}
	virtual void get_next_result(tuple& t);
};
template <class Iterator>
void utf8_tokenize_result<Iterator>::get_next_result(tuple& t)
{
	if (pos >= end)
	{
		if (ret_empty)
		{
			t.copy(EMPTY_STRING_TC);
			ret_empty = false;
		}
		else
			t.set_eos();
		return;
	}

	int match_flags = PCRE_NO_UTF8_CHECK;
	PcreMatcher<Iterator, typename Iterator::off_t> matcher(re);

	Iterator ms, me;
	if (matcher.matches(start, end, pos, match_flags))
	{
		ms = matcher.start(0);
		me = matcher.end(0);
		if (ms == me)
			throw USER_EXCEPTION(FORX0003);
		if (me >= end)
			ret_empty = true;
	}
	else
	{
		ms = end;
		me = end;
	}
	stmt_str_buf b;
	while (pos < ms)
	{
		b << *pos;
		++pos;
	}
	pos = me;
	t.copy(b.get_tuple_cell());
}
template <class Iterator>
static inline void utf8_tokenize(const Iterator &start, const Iterator &end, tuple_cell *t2, tuple_cell *t3, tuple_cell *str_tc, TokenizerResult **res)
{
	*res = se_new utf8_tokenize_result<Iterator>(start, end, t2, t3, str_tc);
}

TokenizerResult* CharsetHandler_utf8::tokenize ( tuple_cell *t1, tuple_cell *t2, tuple_cell *t3)
{
	try
	{
	TokenizerResult *res = NULL;
	STRING_ITERATOR_CALL_TEMPLATE_1tcptr_4p(utf8_tokenize, t1, t2, t3, t1, &res);
	return res;
	}
	catch (const PcreCompileException &e)
	{
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
	catch (const PcreException &e)
	{
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
}


template<class Iterator>
static inline void utf8_matches_bool (const Iterator &start, const Iterator &end, const PcrePattern &re, bool *res)
{
	int match_flags = PCRE_NO_UTF8_CHECK;
	PcreMatcher<Iterator, typename Iterator::off_t> matcher(re);

	*res = matcher.matches(start, end, start, match_flags);
}

static inline void utf8_matches_bool_c (const char *str, const PcrePattern &re, bool *res)
{
	int match_flags = PCRE_NO_UTF8_CHECK;
	PcreMatcher<const unsigned char*, ptrdiff_t> matcher(re);

	*res = matcher.matches((const unsigned char*)str, (const unsigned char*)str+strlen(str), (const unsigned char*)str, match_flags);
}

bool CharsetHandler_utf8::matches (const tuple_cell *tc, const char *regex)
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
	catch (const PcreException &e)
	{
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
}

bool CharsetHandler_utf8::matches (const char *tc, const char *regex)
{
	try
	{
		PcrePattern re(regex, PCRE_UTF8 | PCRE_NO_UTF8_CHECK);
		bool res;
		utf8_matches_bool_c(tc, re, &res);
		return res;
	}
	catch (const PcreCompileException &e)
	{
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
	catch (const PcreException &e)
	{
		throw USER_EXCEPTION2(FORX0002, e.what());
	}
}


//////////////////////////////////////////////////////////////////////////
// Collation Handler
//////////////////////////////////////////////////////////////////////////

int CollationHandler_utf8::compare(str_cursor *cur1, str_cursor *cur2)
{
    char *str1_ptr = NULL, *str2_ptr = NULL;
    int cmp_res = 0;

    str2_ptr = tr_globals::e_string_buf;
    int str2_part_len = cur2->copy_blk(str2_ptr);
    int str1_part_len = cur1->get_blk(&str1_ptr);
	xptr str1xptr = str1_ptr == NULL ? XNULL : addr2xptr(str1_ptr);

    while (true)
    {
        const int real_count = s_min(str1_part_len, str2_part_len);

        if (str1_part_len == 0 && str2_part_len == 0) return 0;
        if (str1_part_len == 0) return -1;
        if (str2_part_len == 0) return 1;

        cmp_res = memcmp(str1_ptr, str2_ptr, real_count);

        if (cmp_res != 0) return sign(cmp_res);

        str1_ptr += real_count;
        str1_part_len -= real_count;
        str2_ptr += real_count;
        str2_part_len -= real_count;

		if (str1_part_len == 0 && str2_part_len == 0)
		{
            str2_ptr = tr_globals::e_string_buf;
            str2_part_len = cur2->copy_blk(str2_ptr);
			str1_part_len = cur1->get_blk(&str1_ptr);
			str1xptr = ADDR2XPTR(str1_ptr);
			continue;
		}
		if (str1_part_len == 0)
		{
			str1_part_len = cur1->get_blk(&str1_ptr);
			str1xptr = ADDR2XPTR(str1_ptr);
			continue;
		}
		if (str2_part_len == 0)
		{
            str2_ptr = tr_globals::e_string_buf;
            str2_part_len = cur2->copy_blk(str2_ptr);
			CHECKP(str1xptr);
			continue;
		}

		throw USER_EXCEPTION2(SE1003, "Impossible case in CollationHandler_utf8::compare");
    }
}
int CollationHandler_utf8::compare(str_cursor *cur, const char *str2)
{
    char *str1_ptr = NULL;
    int str1_part_len = 0;
    int str2len = strlen(str2);
    int cmp_res = 0;

    while (true)
    {
        str1_part_len = cur->get_blk(&str1_ptr);
        if (str1_part_len == 0 && str2len == 0) return 0;
        if (str1_part_len == 0) return -1;
        if (str2len == 0) return 1;

        const int real_count = s_min(str2len, str1_part_len);

        cmp_res = memcmp(str1_ptr, str2, real_count);

        if (cmp_res != 0) return sign(cmp_res);

        if (real_count == str1_part_len)
        {
            str2len -= real_count;
            str2 += real_count;
        }
        else
        {
            return 1;
        }
    }
}

int CollationHandler_utf8::compare(const char *str1, const char *str2)
{
    return sign(strcmp(str1, str2));
}



template <class Iterator>
static inline void utf8_starts_with(const Iterator &start, const Iterator &end, const char* prefix, int pref_len, bool* result)
{
    (*result) = true;
    utf8_iterator<Iterator> src_it(start);
    char_iterator_utf8 pref_it((char*)prefix, pref_len, 0);

    while (src_it.base_iterator() < end && !pref_it.at_end())
    {
        if ( (*src_it) != (*pref_it) )
        {
            (*result) = false;
            break;
        }
        ++src_it;
        ++pref_it;
    }
}

template <class Iterator>
static inline void utf8_ends_with(const Iterator &start, const Iterator &end, int64_t src_len, const char* suffix, str_off_t suf_len, bool* result)
{
    (*result) = true;
    utf8_iterator<Iterator> src_it(start);
    char_iterator_utf8 suf_it((char*)suffix, strlen(suffix), 0);

    for(int64_t i = src_len - suf_len; i > 0; i--) ++src_it;

    while (src_it.base_iterator() < end && !suf_it.at_end())
    {
        if ( (*src_it) != (*suf_it) )
        {
            (*result) = false;
            break;
        }
        ++src_it;
        ++suf_it;
    }
}

bool CollationHandler_utf8::starts_with(const tuple_cell *tc, const tuple_cell *prefix)
{
    tuple_cell pref = tuple_cell::make_sure_light_atomic(*prefix);

	//ptrdiff_t is ok since we use make_sure_light_atomic
	ptrdiff_t pref_len = (ptrdiff_t)pref.get_strlen();

    if(tc->get_strlen() < pref_len) return false;

    bool result;
    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(utf8_starts_with, tc, pref.get_str_mem(), pref_len, &result);
    return result;
}

bool CollationHandler_utf8::ends_with(const tuple_cell *tc, const tuple_cell *suffix)
{
    tuple_cell suf = tuple_cell::make_sure_light_atomic(*suffix);

    int64_t src_len = charset_handler->length((tuple_cell*)tc);
    int64_t suf_len = charset_handler->length((tuple_cell*)suffix);

    if(src_len < suf_len) return false;

    bool result;
    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_4p(utf8_ends_with, tc, src_len, suf.get_str_mem(), suf_len, &result);
    return result;
}


template <class Iterator1, class Iterator2>
static inline void utf8_contains_part2(Iterator2 &start1, const Iterator2 &end1, int *res, Iterator1 &start, const str_off_t len)
{
	str_off_t len1 = end1 - start1;
	(*res) = PPSubsMatch::contains<Iterator1, Iterator2>(start, start1, len, len1);
}

template <class Iterator>
static inline void utf8_contains(Iterator &start, const Iterator &end, const tuple_cell *subs, int *res)
{
    str_off_t len = end - start;
	STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(utf8_contains_part2, subs, res, start, len);
}

int CollationHandler_utf8::contains(const tuple_cell *src, const tuple_cell *subs)
{
    int result;
    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_2p(utf8_contains, src, subs, &result);
    return result;
}

/////////////////////////////////////////////////////////////////////////

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


// utf8_valid
//  Returns:       NULL   if the string is a valid UTF-8 string
//				   otherwise; the value is the offset of the bad byte

// utf8_table4[] is defined in pcre headers

char*
utf8_valid(const char *string, size_t length)
{
	register const char * p;

	for (p = string; length-- > 0; ++p)
	{
		register unsigned int ab;
		register int c = (unsigned char)*p;
		if (c < 128) continue;
		if ((c & 0xc0) != 0xc0) return (char*)p;
		ab = utf8_table4[c & 0x3f];  /* Number of additional bytes */
		if (length < ab) return (char*)p;
		length -= ab;

		/* Check top bits in the second byte */
		if ((*(++p) & 0xc0) != 0x80) return (char*)p;

		/* Check for overlong sequences for each different length */
		switch (ab)
		{
			/* Check for xx00 000x */
		case 1:
			if ((c & 0x3e) == 0) return (char*)p;
			continue;   /* We know there aren't any more bytes to check */

			/* Check for 1110 0000, xx0x xxxx */
		case 2:
			if (c == 0xe0 && (*p & 0x20) == 0) return (char*)p;
			break;

			/* Check for 1111 0000, xx00 xxxx */
		case 3:
			if (c == 0xf0 && (*p & 0x30) == 0) return (char*)p;
			break;

			/* Check for 1111 1000, xx00 0xxx */
		case 4:
			if (c == 0xf8 && (*p & 0x38) == 0) return (char*)p;
			break;

			/* Check for leading 0xfe or 0xff, and then for 1111 1100, xx00 00xx */
		case 5:
			if (c == 0xfe || c == 0xff ||
				(c == 0xfc && (*p & 0x3c) == 0)) return (char*)p;
			break;
		}

		/* Check for valid bytes after the 2nd, if any; all must start 10 */
		while (--ab > 0)
		{
			if ((*(++p) & 0xc0) != 0x80) return (char*)p;
		}
	}

	return NULL;
}
