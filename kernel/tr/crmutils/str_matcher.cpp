#include "str_matcher.h"
#include <stdlib.h>
#include <string.h>


trie_node_t *StrMatcher::make_node(trie_node_t *parent, char ch)
{
	trie_node_t *tn = (trie_node_t *)malloc(sizeof(trie_node_t));
	memset(tn->next, 0, sizeof(tn->next));
	tn->res_ofs = -1;
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
	//TODO
}

trie_node_t *StrMatcher::get_node(trie_node_t *start, const char *str, int set_pc)
{
	start->pc = (int)start->pc | (int)set_pc;
	if (*str == 0)
		return start;
	if (start->next[(unsigned char)*str] == NULL)
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
	add_string_to_buf(map_str, &node->res_ofs, &node->res_len);
}

void StrMatcher::reset()
{
	strings_buf_used = 0;
	buf_used = 0;
	
	delete_trie(root);
	
	root = make_node(NULL, 0);
	state = root;
}

trie_node_t *StrMatcher::get_ls_node(trie_node_t *node)
{
	//TODO
	return root;
}
 
#define _MIN_(a,b) (a) < (b) ? (a) : (b)
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

inline void put_prefix(char *where, trie_node_t *node)
{
	while (node->len > 0)
	{
		where[node->len - 1] = node->ch;
		node = node->parent;
	}
}

int StrMatcher::parse(const char *str, int len, write_func_t write_cb, void *p, int pc)
{
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
			if (state->res_ofs != -1)
			{
				printpart(k, i - state->len, str, len, write_cb, p, buf, buf_used);
				write_cb(p, &strings_buf[state->res_ofs], state->res_len);
				k = i+1;
				state = root;
			}
		}
		//FIXME: assert state == root in else
	}
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
	
	root = make_node(NULL, 0);
	state = root;
}

StrMatcher::~StrMatcher()
{
	delete_trie(root);
	free(strings_buf);
	free(buf);
}
