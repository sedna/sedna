/*
 * File:  test.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _TEST_H
#define _TEST_H

void helper_stuff_string(char* str, shft & str_size, shft max_increase);
void helper_populate_page(char* pg, bool is_leaf, shft key_size, shft key_num, shft max_chunk_size);
shft helper_count_key_with_load_data_volume(char* pg, shft start_key_idx, shft end_key_idx);
void helper_print_page(char* pg, bool header, bool tab_key, bool key_contents, bool  tab_bigptr, bool tab_chnk, bool chunks);
void helper_spool_page(char* pg);
void helper_read_page(char* pg);
void helper_print_spool();
#endif
