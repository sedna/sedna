#ifndef _STR_MATCHER_H
#define _STR_MATCHER_H


enum pat_class
{
	pat_attribure	= 1,
	pat_element		= 2
};

struct trie_node
{
	int res_ofs;
	int res_len;
	struct trie_node *next[256];
};
typedef struct trie_node trie_node_t;


class StrMatcher 
{
private:
	char *strings_buf;
	int strings_buf_len;
	int strings_buf_used;
	char *buf;
	int buf_len;
	int buf_used;
	trie_node_t *root;
	trie_node_t *state;
	trie_node_t *make_node();
	void delete_trie(trie_node_t *root);
	trie_node_t *get_node(trie_node_t *start, const char *str);
	void add_string_to_buf(const char *str, int *ofs, int *len);
	char *last_match;
	int last_match_len;
public:
	void add_str (const char * str, const char * map_str, pat_class pc = (pat_class)-1);
	void clear_state();
	int match_next_symbol(char symb, pat_class pc);//0-no matches 1-full match -1 -prefix match
	const char * get_buf() {return buf; }
	unsigned int get_buf_len() {return buf_used;}
	const char* get_last_match() {return last_match; }
	int get_last_match_len() {return last_match_len; }
	StrMatcher();
};
    
#endif