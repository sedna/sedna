/*
 * File:  ft_storage.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_STORAGE_H
#define _FT_STORAGE_H

#include "common/xptr/xptr.h"
#include "tr/btree/btree.h" //FIXME: move to cpp
#include "tr/ft/ft_partition.h"
#include "tr/ft/ft_types.h"

#define FTS_MAX_PARTITIONS 5

struct FtsData
{
	ft_partition_data partitions[FTS_MAX_PARTITIONS];
	int npartitions;
	int naccs;
	xptr doc_stats;

	//options
	ft_stem_type stem_type;
	
	FtsData() : npartitions(0), naccs(0), doc_stats(XNULL), stem_type(ftst_default) {}
};

class FtsUpdater
{
private:
	FtsData *fts_data;
	FtPartitionBuilder pb;
	bool empty;
	void before_upd()
	{
		if (empty)
		{
			empty = false;
			if (fts_data->doc_stats == XNULL)
			{
				fts_data->doc_stats = bt_create(xs_integer);
			}
			pb.create_new();
		}
	}
	bool pers;
public:
	FtsUpdater(bool _pers) : pb(_pers), pers(_pers) {}
	//updates are performed as such:
	// 1. begin_update is called
	// 2. del_document/add_document is called for each deleted/updated/inserted node in ascending xptr order
	// 3. update functions are called (add_word_occur, del_word_occur), arguments (word, acc, word_ind) must be ascending during these calls
	//    acc may be one of passed to del_document, if node was updated
	// 4. end_update is called
	void begin_update(FtsData *_fts_data);
	//end update and modify FtsData at pointer dest, so that it will become identical to FtsData at buffer provided in begin_update
	//returs true if some changes were made
	bool end_update(struct FtsData *dest);

	void del_document(const xptr acc)
	{
		before_upd();
		pb.del_doc(acc);

		bt_key bkey;
		bkey.setnew((int64_t)FT_XPTR_TO_UINT(acc));

		bt_cursor_tmpl<uint64_t> cur = bt_find_tmpl<uint64_t>(fts_data->doc_stats, bkey);
		if (cur.bt_next_obj() != null_object<uint64_t>::get())
		{
			bt_delete_tmpl<uint64_t>(fts_data->doc_stats, bkey);
			fts_data->naccs--;
		}
	}
	void add_document(const xptr acc, int doc_len)
	{
		before_upd();
		bt_key bkey;
		bkey.setnew((int64_t)FT_XPTR_TO_UINT(acc));

		bt_cursor_tmpl<uint64_t> cur = bt_find_tmpl<uint64_t>(fts_data->doc_stats, bkey);
		if (cur.bt_next_obj() != null_object<uint64_t>::get())
		{
			bt_delete_tmpl<uint64_t>(fts_data->doc_stats, bkey);
			fts_data->naccs--;
		}
		U_ASSERT(doc_len > 0);
		bt_insert_tmpl<uint64_t>(fts_data->doc_stats, bkey, (uint64_t)doc_len);
		fts_data->naccs++;
	}
	void add_word_occur(const char *word, const xptr acc, const int word_ind)
	{
		before_upd();
		pb.add_word_occur(word, acc, word_ind+1);
	}
};

struct FtsScanData
{
	FtPartitionScanner scanner;
	xptr doc_stats;
	ft_acc_uint_t cur_acc_i;
	ftp_ind_t cur_ind;

	//start scan
	void init(const struct FtsData *fts_data, const char *word)
	{
		scanner.init(fts_data->partitions, fts_data->npartitions, word);
		doc_stats = fts_data->doc_stats;
		if (!scanner.get_next_occur(&cur_acc_i, &cur_ind))
		{
			cur_acc_i = FT_ACC_UINT_NULL;
			cur_ind = 0;
		}
	}

	bool at_end()
	{
		return cur_acc_i == FT_ACC_UINT_NULL;
	}

	ft_acc_uint_t get_cur_acc_i()
	{
		return cur_acc_i;
	}

	int cur_word_ind()
	{
		return cur_ind-1;
	}

	//move to next occur
	void next_occur()
	{
		if (!scanner.get_next_occur(&cur_acc_i, &cur_ind))
		{
			cur_acc_i = FT_ACC_UINT_NULL;
			cur_ind = 0;
		}
	}

	//returns number of skipped occurs
	int skip_node()
	{
		const ft_acc_uint_t old_acc_i = cur_acc_i;
		int noccurs = 0;
		while (cur_acc_i != FT_ACC_UINT_NULL && old_acc_i == cur_acc_i)
		{
			next_occur();
			noccurs++;
		}
		return noccurs;
	}

	uint64_t get_doc_len(ft_acc_uint_t acc_i)
	{
		bt_key bkey;
		bkey.setnew((int64_t)acc_i); //FIXME

		bt_cursor_tmpl<uint64_t> cur = bt_find_tmpl<uint64_t>(doc_stats, bkey);
		return cur.bt_next_obj(); //FIXME: depends on the fact that null object for uint64_t is 0 (in btstruct.h)
	}
};

void fts_create(struct FtsData *data);

#endif
