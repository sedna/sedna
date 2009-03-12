/*
 * File:  ft_cache.h
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_CACHE_H
#define _FT_CACHE_H

#include "common/xptr.h"
#include "tr/executor/base/tuple.h"
#include "tr/executor/base/sorted_sequence.h"
#include "tr/ft/string_map.h" //FIXME: remove (need allocator), don't use MallocAllocator

#define FTC_ALLOCATOR MallocAllocator
#define FTC_ALLOCATOR_IS_MALLOC_ALLOCATOR

typedef FTC_ALLOCATOR::ptr_t ftc_doc_t;
typedef FTC_ALLOCATOR::ptr_t ftc_index_t;

//ft_index_sem must be accuired!
ftc_index_t ftc_get_index(const char *name, xptr btree_root);
void ftc_set_ss(ftc_index_t idx, sorted_sequence *ss);

//returned doc may become invalid after any operation with index
ftc_doc_t ftc_add_new_doc(ftc_index_t idx, xptr acc);

void ftc_add_word(ftc_index_t index, ftc_doc_t &ft_doc, const char *word, int word_ind, bool no_flush=false);

void ftc_flush();

#define FTC_PTR      FTC_ALLOCATOR::ptr_t
#define FTC_NULL     FTC_ALLOCATOR::null_ptr()
#define FTC_MAP      string_map<FTC_ALLOCATOR::ptr_t, FTC_ALLOCATOR>
#define FTC_WORDMAP  string_map<ftc_word_data, FTC_ALLOCATOR>
#define FTC_OCCURMAP string_map<ftc_occur_data, FTC_ALLOCATOR>

struct ftc_occur_data
{
	ftc_doc_t doc;
	FTC_PTR first;
	FTC_PTR last;
};

//FIXME: fix .h files dependencies
#include "tr/idx/btree/btree.h"

class ftc_scan_result
{
private:
	ftc_index_t ftc_idx;

	FTC_OCCURMAP *om;
	FTC_OCCURMAP::pers_sset_entry *ome;

	bt_cursor_tmpl<ft_idx_btree_element> bcur;
	ft_idx_btree_element ce;
public:
	ftc_scan_result(ftc_index_t idx) : ftc_idx(idx) {}
	void scan_word(const char *word);
	void get_next_result(tuple &t);
};


#endif
