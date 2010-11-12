/*
 * File:  str_matcher.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */


#ifndef _STR_MATCHER_H
#define _STR_MATCHER_H

#include "tr/strings/strings_base.h"

struct tuple_cell;

struct trie_node
{
    /* 
     * since res_ofs is used to store pointer to a function (ZOMG!!!) I'm
     * promoting it to a union. But all this mess should be fixed ASAP (AK).
     */
    union
    {
        int offs;
        uintptr_t ptr;
    } res_ofs;
	int res_len;
	int pc;
	int len;
	struct trie_node *parent;
	unsigned char ch;
	struct trie_node *next[256];
};

typedef struct trie_node trie_node_t;

typedef void (*write_func_t)(void *param, const char *str, int len);

class StrMatcher 
{
private:
	trie_node_t *s3; //dirty hack
	char *strings_buf;
	int strings_buf_len;
	int strings_buf_used;
	char *buf;
	int buf_len;
	int buf_used;
	trie_node_t *root;
	trie_node_t *state;
	trie_node_t *make_node(trie_node_t *p, char ch);
	void delete_trie(trie_node_t *root);
	trie_node_t *get_node(trie_node_t *start, const char *str, int set_pc);
	trie_node_t *get_ls_node(trie_node_t *node);
	void add_string_to_buf(const char *str, int *ofs, int *len);
	bool replace_surr; 
public:
	void add_str (const char * str, const char * map_str, int pc = -1);
	void add_unicode_escape_range (int start_symbol, int end_symbol, int pc = -1);
	void reset();

	// if write_cb == NULL, returs 1 if something matched
	// if write_cb != NULL, parses and returns number of replaces
	int parse(const char *str, str_off_t len, write_func_t write_cb, void *p, int pc = -1);
	int parse_tc(const tuple_cell *tc, write_func_t write_cb, void *p, int pc);
	void flush(write_func_t write_cb, void *p);
	
	StrMatcher();
	~StrMatcher();
};

#endif /* _STR_MATCHER_H */
