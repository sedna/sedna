#include "str_matcher.h"
#include <stdlib.h>
#include <string.h>


trie_node_t *StrMatcher::make_node()
{
	trie_node_t *tn = (trie_node_t *)malloc(sizeof(trie_node_t));
	memset(tn->next, 0, sizeof(tn->next));
	tn->res_ofs = -1;
	tn->res_len = 0;
	return tn;
}

trie_node_t *StrMatcher::get_node(trie_node_t *start, const char *str)
{
	if (*str == 0)
		return start;
	if (start->next[(unsigned char)*str] == NULL)
		start->next[(unsigned char)*str] = make_node();
	return get_node(start->next[(unsigned char)*str], str+1);
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

void StrMatcher::add_str (const char * str, const char * map_str, pat_class pc)
{
	trie_node *node = get_node(root, str);
	add_string_to_buf(str, &node->res_ofs, &node->res_len);
}

void StrMatcher::clear_state()
{
	buf_used = 0;
	state = root;
}
int StrMatcher::match_next_symbol(char symb, pat_class pc)
{
	if (state->next[(unsigned char)symb] == NULL)
		return 0;
	else
	{
		state = state->next[(unsigned char)symb];
		if (buf_used >= buf_len)
		{
			buf_len *= 2;
			buf = (char*)realloc(buf, buf_len);
		}
		buf[buf_used] = symb;
		buf_used++;
		if (state->res_ofs == -1)
			return -1;
		else
		{
			last_match = &strings_buf[state->res_ofs];
			last_match_len = state->res_len;
			return 1;
		}
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
	last_match = NULL;
	last_match_len = 0;
	
	root = make_node();
	state = root;
}
