#ifndef _STR_MATCHER_H
#define _STR_MATCHER_H


enum pat_class
{
	pat_attribute	= 1,
	pat_element		= 2
};

struct trie_node
{
	int res_ofs;
	int res_len;
	int pc;
	struct trie_node *next[256];
};
typedef struct trie_node trie_node_t;

typedef void (*write_func_t)(void *param, const char *str, int len);

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
	trie_node_t *get_node(trie_node_t *start, const char *str, int set_pc);
	void add_string_to_buf(const char *str, int *ofs, int *len);
	char *last_match;
	int last_match_len;
public:
	void add_str (const char * str, const char * map_str, pat_class pc = (pat_class)-1);
	void clear_state();
	void reset();

	// if f == NULL, returs 1 if something matched
	// if f != NULL, parses and returns number of replaces
	int parse(const char *str, int len, write_func_t f, void *p, pat_class pc = (pat_class)-1);
	void flush(write_func_t f, void *p);
	
	StrMatcher();
};
    
#endif