/*
 * File:  ft_cache.h
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_CACHE_H
#define _FT_CACHE_H

#include "common/xptr.h"
#include "tr/executor/base/tuple.h"
#include "tr/executor/base/sorted_sequence.h"
#include "tr/ft/ft_storage.h"
#include "tr/ft/ft_norm.h"
#include "tr/ft/string_map.h" //FIXME: remove (need allocator), don't use MallocAllocator

#define FTC_ALLOCATOR MallocAllocator
#define FTC_ALLOCATOR_IS_MALLOC_ALLOCATOR

typedef FTC_ALLOCATOR::ptr_t ftc_doc_t;
typedef FTC_ALLOCATOR::ptr_t ftc_index_t;

//ft_index_sem must be accuired!
ftc_index_t ftc_get_index(const char *name, struct FtsData *fts_data);

//returned doc may become invalid after any operation with index
ftc_doc_t ftc_add_new_doc(ftc_index_t idx, xptr acc);
//get document in cache (adds document to cache if it's not there)
ftc_doc_t ftc_get_doc(ftc_index_t idx, xptr acc);

FtStemmer *ftc_get_stemmer(ftc_index_t idx);

//delete document from index
void ftc_del_doc(ftc_index_t index, const xptr acc);
void ftc_upd_word(ftc_index_t index, ftc_doc_t &ft_doc, const char *word, int word_ind, bool no_flush=false);
void ftc_finalize_doc(ftc_index_t index, ftc_doc_t ft_doc, int doc_len);

void ftc_flush();

#define FTC_PTR      FTC_ALLOCATOR::ptr_t
#define FTC_NULL     FTC_ALLOCATOR::null_ptr()
//TODO: move these to cpp
#define FTC_MAP      string_map<FTC_ALLOCATOR::ptr_t, FTC_ALLOCATOR>
#define FTC_WORDMAP  string_map<ftc_word_data, FTC_ALLOCATOR>
#define FTC_OCCURMAP string_map<ftc_occur_data, FTC_ALLOCATOR>
#define FTC_DOCMAP   string_map<ftc_doc_t, FTC_ALLOCATOR>

struct ftc_occur_data
{
	ftc_doc_t doc;
	FTC_PTR first;   //first occur
	FTC_PTR cursor;  //last updated occur
};
struct ftc_word_data
{
	FTC_PTR occur_map;
};

//FIXME: fix .h files dependencies
#include "tr/idx/btree/btree.h"


struct ftc_word_occur;
class FtCacheScanner : public FtScanner
{
private:
	ftc_index_t ftc_idx;

	FTC_OCCURMAP *om;
	FTC_OCCURMAP::pers_sset_entry *ome;
	ftc_word_occur *cur_occur;

	//scan for the next occur, returns true if ome was changed
	//pre: ome != NULL (cur_occur may be NULL)
	//post: FtScanner::cur_acc_i updated accordingly
	bool scan_occurs();
public:
	FtCacheScanner(ftc_index_t idx) : ftc_idx(idx) {}
	void init_word(const char *word);

	int cur_word_ind();
	void next_occur();
	//returns number of skipped occurs
	int skip_acc();
	bool acci_deleted(ft_uint_t acc_i);

	uint64_t get_doc_len(ft_uint_t acc_i);
};

//TODO: remove this class
class ftc_scan_result
{
private:
	ftc_index_t ftc_idx;

	struct FtsScanData fts_sd;
	FtCacheScanner ftc_s;

	inline bool get_next_result_step(uint64_t *res, int *noccurs);
	inline bool get_next_occur_step(ft_uint_t *acc, int *word_ind);
public:
	ftc_scan_result(ftc_index_t idx) : ftc_idx(idx), ftc_s(idx) {}
	ftc_index_t get_ftc_idx() { return this->ftc_idx; }
	void scan_word(const char *word);
	void get_next_result(uint64_t *res, int *noccurs);

	//see FtQueryTerm::get_next_occur description
	void get_next_occur(ft_uint_t *acc_i, int *word_ind);

	uint64_t get_doc_len(ft_uint_t acc_i); //FIXME: move outsize of this class
};

int ftc_get_doc_count(ftc_index_t idx);

class ftc_scan_words_result
{
private:
	ftc_index_t ftc_idx;

	int nscanners;
	FtWordsScanner** scanners;
public:
	ftc_scan_words_result(ftc_index_t idx);
	void get_next_result(tuple &t);
	~ftc_scan_words_result();
};

#endif
