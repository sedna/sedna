/*
 * File:  str_matcher.cpp
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include "tr/crmutils/str_matcher.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tr/strings/strings.h"
#include "tr/strings/utf8.h"


trie_node_t *StrMatcher::make_node(trie_node_t *parent, char ch)
{
	trie_node_t *tn = (trie_node_t *)malloc(sizeof(trie_node_t));
	memset(tn->next, 0, sizeof(tn->next));
	tn->res_ofs.offs = -1;
	tn->res_len = 0;
	tn->pc = 0;
	if (parent == NULL)
		tn->len = 0;
	else
		tn->len = parent->len + 1;
	tn->ch = ch;
	tn->parent = parent;
	return tn;
}

void StrMatcher::delete_trie(trie_node_t *root)
{
	trie_node_t * last = NULL;
	for (int i = 0; i<256; i++)
		if (root->next[i] != NULL)
		{
			if (root->next[i] != last && root->next[i] != s3) //FIXME: dirty hack for replace_surr
			{
				last = root->next[i];
				delete_trie(root->next[i]);
			}
			root->next[i] = NULL;
		}

	if (root != s3) //FIXME: one more part of this dirty dirty hack
		free(root);
}

trie_node_t *StrMatcher::get_node(trie_node_t *start, const char *str, int set_pc)
{
	start->pc = (int)start->pc | (int)set_pc;
	if (*str == 0)
		return start;
	if (start->next[(unsigned char)*str] == NULL || start->next[(unsigned char)*str] == s3) //FIXME: dirty hack is here as well
		start->next[(unsigned char)*str] = make_node(start, *str);
	return get_node(start->next[(unsigned char)*str], str+1, set_pc);
}

void StrMatcher::add_string_to_buf(const char *str, int *ofs, int *len)
{
	*ofs = strings_buf_used;
	*len = 0;
	while (*str)
	{
		if (strings_buf_used >= strings_buf_len)
		{
			strings_buf_len *= 2;
			strings_buf = (char*)realloc(strings_buf, strings_buf_len);
		}
		strings_buf[strings_buf_used] = *str;
		str++;
		strings_buf_used++;
	}
	(*len) = strings_buf_used - *ofs;
}

void StrMatcher::add_str (const char * str, const char * map_str, int pc)
{
	trie_node *node = get_node(root, str, pc);
	add_string_to_buf(map_str, &node->res_ofs.offs, &node->res_len);
}


typedef void (*replace_cb)(const char *occur, int occur_len, write_func_t write_cb, void *p);
static void print_unicode_escape(const char *occur, int occur_len, write_func_t write_cb, void *p)
{
	char buf[15];
	int c = sprintf(buf, "&#%d;", utf8_parse_char(occur));
	write_cb(p, buf, c);
}

void StrMatcher::add_unicode_escape_range (int start_symbol, int end_symbol, int pc)
{
	for (int symbol = start_symbol; symbol <= end_symbol; symbol++)
	{
		const char *str = utf8_encode_char(symbol);
		trie_node *node = get_node(root, str, pc);
		node->res_ofs.ptr = (uintptr_t)print_unicode_escape;
		node->res_len = -1;
	}
}

void StrMatcher::reset()
{
	strings_buf_used = 0;
	buf_used = 0;
	
	if (root != NULL)
		delete_trie(root);
	if (s3 != NULL) //FIXME: that is also a part of some dirty hack
	{
		trie_node_t *_s3 = s3;
		delete_trie(_s3);
		free(s3);
		s3 = NULL;
	}

	
	root = make_node(NULL, 0);
	state = root;
	if (replace_surr)
	{
		//240-244
		trie_node_t *s0 = make_node(root, 0);
		trie_node_t *s1 = make_node(s0, 0);
		trie_node_t *s2 = make_node(s1, 0);
		s3 = make_node(s2, 0);
		int i;

		s0->pc = s1->pc = s2->pc = s3->pc = -1;

		for (i = 240; i <= 244; i++)
			root->next[i] = s0;
		for (i = 0; i <= 255; i++)
			s0->next[i] = s1;
		for (i = 0; i <= 255; i++)
			s1->next[i] = s2;
		for (i = 0; i <= 255; i++)
			s2->next[i] = s3;

		s3->res_ofs.ptr = (uintptr_t)print_unicode_escape;
		s3->res_len = -1;
	}
}

trie_node_t *StrMatcher::get_ls_node(trie_node_t *node)
{
	//TODO
	return root;
}
 
#define _MIN_(a,b) ((a) < (b) ? (a) : (b))
inline void printpart(int from, int to, const char *str, int len, 
	write_func_t write_cb, void *p, char *buf, int buf_used)
{
	if (from < 0)
	{
		write_cb(p, buf + buf_used + from, _MIN_(buf_used, 1+to-from));
		if (to >= 0)
			write_cb(p, str, 1+to);
	}
	else
	{
		//FIXME: assert(from >= 0)
		//FIXME: assert(to >= from)
		write_cb(p, str+from, 1+to-from);
	}
}
inline void copypart(char *dst, int from, int to, const char *str, int len, 
					  char *buf, int buf_used)
{
	if (from < 0)
	{
		memcpy(dst, buf + buf_used + from, _MIN_(buf_used, 1+to-from));
		if (to >= 0)
			memcpy(dst + _MIN_(buf_used, 1+to-from), str, 1+to);
	}
	else
	{
		//FIXME: assert(from >= 0)
		//FIXME: assert(to >= from)
		memcpy(dst, str+from, 1+to-from);
	}
}

inline void put_prefix(char *where, trie_node_t *node)
{
	while (node->len > 0)
	{
		where[node->len - 1] = node->ch;
		node = node->parent;
	}
}

/*
	I think a quick pretest here could be a performance gain. 
	Up to 4 bytes can be tested simultaneously using int32 arithmetic, mmx intstructions
	allows a simultaneous 8 bytes test and sse2 has capacity for 16 bytes.
	See these entries 
	http://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord
	http://graphics.stanford.edu/~seander/bithacks.html#HasBetweenInWord
	- Common escaped characters (<>&"\) are in ascii range and can be tested using ZeroInWord method.
    - A test for characters not in ascii range can be implemented by comparing "head byte"
	  of UTF-8 encoded character (those that have 2 high bits set) against a respective value
	  using ZeroInWord method (rather inaccurate after all).
    - A test for unicode escape range can be done with HasBetweenInWord method on the "head byte".
	ZNV
*/ 
int StrMatcher::parse(const char *str, str_off_t len, write_func_t write_cb, void *p, int pc)
{
	U_ASSERT(len >= 0 && len <= SIZE_MAX);
	//TODO: implement (write_cb == 0) case
	int k = -buf_used;
	for (int i = 0; i < len; i++)
	{
		while (state != root && 
			(state->next[(unsigned char)str[i]] == NULL || (state->next[(unsigned char)str[i]]->pc & pc) == 0))
		{
			state = get_ls_node(state);
		}
		if (state->next[(unsigned char)str[i]] != NULL && ((state->next[(unsigned char)str[i]]->pc & pc) != 0))
		{
			state = state->next[(unsigned char)str[i]];
			state->ch = (unsigned char)str[i]; //FIXME: this is a dirty hack for replace_surr, and was never tested
			if (state->res_ofs.offs != -1)
			{
				if (write_cb==NULL) return 1;
				printpart(k, i - state->len, str, len, write_cb, p, buf, buf_used);
				if (state->res_len == -1)
				{
					char *str_part = (char*)malloc(state->len + 1);
					copypart(str_part, i + 1 - state->len, i, str, len, buf, buf_used);
					str_part[state->len] = 0;
					((replace_cb)state->res_ofs.ptr)(str_part, state->len, write_cb, p);
					free(str_part);
				}
				else
					write_cb(p, &strings_buf[state->res_ofs.offs], state->res_len);
				k = i+1;
				state = root;
			}
		}
		//FIXME: assert state == root in else
	}
	if (write_cb==NULL) return 0;
	if (k < len - state->len)
	{
		printpart(k, len - 1 - state->len, str, len, write_cb, p, buf, buf_used);
		k = len - state->len; //FIXME: do this only when asserts enabled
	}
	//FIXME: assert k == len - state->len
	buf_used = state->len;
	if (buf_used > buf_len)
	{
		while (buf_used > buf_len)
			buf_len *= 2;
		buf = (char*)realloc(buf, buf_len);
	}
	put_prefix(buf, state);
	
	return 0; //TODO: return number of replaces
}

struct feed_to_matcher_state
{
	int res_counter;
	StrMatcher *sm_ptr;
	write_func_t write_cb;
	void *p;
	int pc;
};
typedef struct feed_to_matcher_state feed_to_matcher_state_t;

static void feed_to_matcher(const char *str, int len, void *p)
{
	feed_to_matcher_state_t* s = (feed_to_matcher_state_t*)p;
	if (s->write_cb != NULL || s->res_counter == 0)
		s->res_counter += s->sm_ptr->parse(str, len, s->write_cb, s->p, s->pc);
}

int StrMatcher::parse_tc(const tuple_cell *tc, write_func_t write_cb, void *p, int pc)
{
	feed_to_matcher_state_t s = {0, this, write_cb, p, pc};
	feed_tuple_cell(feed_to_matcher, &s, *tc);
	return s.res_counter;
}

void StrMatcher::flush(write_func_t write_cb, void *p)
{
	if (buf_used > 0)
	{
		write_cb(p, buf, buf_used);
		buf_used = 0;
	}
}


StrMatcher::StrMatcher()
{
	strings_buf_len = 32;
	strings_buf = (char*)malloc(strings_buf_len);
	strings_buf_used = 0;
	buf_len = 16;
	buf = (char*)malloc(buf_len);
	buf_used = 0;
	
	replace_surr = true;

	root = NULL;
	s3 = NULL;
	reset();
}

StrMatcher::~StrMatcher()
{
	delete_trie(root);
	if (s3 != NULL) //FIXME: that is also a part of some dirty hack
	{
		trie_node_t *_s3 = s3;
		delete_trie(_s3);
		free(s3);
		s3 = NULL;
	}
	free(strings_buf);
	free(buf);
}
