#include "str_matcher.h"
#include <stdlib.h>
#include <string.h>


trie_node_t *StrMatcher::make_node()
{
	trie_node_t *tn = (trie_node_t *)malloc(sizeof(trie_node_t));
	memset(tn->next, 0, sizeof(tn->next));
	tn->res_ofs = -1;
	tn->res_len = 0;
	tn->pc = 0;
	return tn;
}

trie_node_t *StrMatcher::get_node(trie_node_t *start, const char *str, int set_pc)
{
	start->pc = (int)start->pc | (int)set_pc;
	if (*str == 0)
		return start;
	if (start->next[(unsigned char)*str] == NULL)
		start->next[(unsigned char)*str] = make_node();
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
	add_string_to_buf(str, &node->res_ofs, &node->res_len);
}

void StrMatcher::reset()
{
	strings_buf_used = 0;
	buf_used = 0;
	last_match = NULL;
	last_match_len = 0;
	
	delete_trie(root);
	
	root = make_node();
	state = root;
}

trie_node_t *StrMatcher::get_ls_node(trie_node_t *node)
{
	//TODO
	return root;
}


int StrMatcher::parse(const char *str, int len, write_func_t write_cb, void *p, int pc)
{
	int k = 0;
	for (int i = 0; i < len; i++)
	{
		while (state != root && 
			(state->next[(unsigned char)str[i]] == NULL || (state->next[(unsigned char)str[i]]->pc & pc) == 0))
		{
			state = get_ls_node(state);
		}
		if (state->next[(unsigned char)str[i]] != NULL)
		{
			state = state->next[(unsigned char)str[i]];
			if (state->res_ofs != -1)
			{
				//TODO
			}
		}
	}
	if (k < len)
	{
		//TODO: print buf + k..len-1 without last state chars, leave them in buf
	}
	
	return 0;
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
	last_match = NULL;
	last_match_len = 0;
	
	root = make_node();
	state = root;
}
