#ifndef _STR_MATCHER_H
#define _STR_MATCHER_H
class StrMatcher 
{

public:
	void add_str (const char * str, const char * map_str);
	void clear_state();
	int match_next_symbol(char symb);//0-no matches 1-full match -1 -prefix match
	const char * get_buf();
	unsigned int get_buf_len();
	const char* get_last_match();
	StrMatcher();
};
    
#endif