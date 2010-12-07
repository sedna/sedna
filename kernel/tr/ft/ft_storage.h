/*
 * File:  ft_storage.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_STORAGE_H
#define _FT_STORAGE_H

#include "common/xptr.h"
#include "tr/idx/btree/btree.h" //FIXME: move to cpp
#include "tr/ft/ft_partition.h"
#include "tr/ft/ft_types.h"

#define FTS_MAX_PARTITIONS 5

struct FtsData
{
	ft_partition_data partitions[FTS_MAX_PARTITIONS];
	int npartitions;
	
	FtsData() : npartitions(0) {}
};

class FtsUpdater
{
private:
	FtsData *fts_data;
	FtPartitionBuilder pb;
public:
	//updates are performed as such:
	// 1. begin_update is called
	// 2. del_document is called for each deleted/updated node in ascending xptr order
	// 3. update functions are called (add_word_occur, del_word_occur), arguments (word, acc, word_ind) must be ascending during these calls
	//    acc may be one of passed to del_document, if node was updated
	// 4. end_update is called
	void begin_update(FtsData *_fts_data);
	//end update and modify FtsData at pointer dest, so that it will become identical to FtsData at buffer provided in begin_update
	void end_update(struct FtsData *dest);

	void del_document(const xptr acc)
	{
		pb.del_doc(acc);
	}
	void add_word_occur(const char *word, const xptr acc, const int word_ind)
	{
		pb.add_word_occur(word, acc, word_ind+1);
	}
	/*
	void del_word_occur(const char *word, const xptr acc, const int word_ind)
	{
		//legacy code, TODO: remove if not needed anymore
		U_ASSERT(false);
		bt_key bkey;
		bkey.setnew(word);
		bt_delete_tmpl<ft_idx_btree_element>(fts_data->btree_root, bkey, ft_idx_btree_element(acc, word_ind));
	}*/
};

struct FtsScanData
{
	FtPartitionScanner scanner;
	ft_uint_t cur_acc_i;
	ftp_ind_t cur_ind;

	//start scan
	void init(const struct FtsData *fts_data, const char *word)
	{
		scanner.init(fts_data->partitions, fts_data->npartitions, word);
		if (!scanner.get_next_occur(&cur_acc_i, &cur_ind))
		{
			cur_acc_i = FT_UINT_NULL;
			cur_ind = 0;
		}
	}

	bool at_end()
	{
		return cur_acc_i == FT_UINT_NULL;
	}

	ft_uint_t get_cur_acc_i()
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
			cur_acc_i = FT_UINT_NULL;
			cur_ind = 0;
		}
	}

	void skip_node()
	{
		const ft_uint_t old_acc_i = cur_acc_i;
		while (cur_acc_i != FT_UINT_NULL && old_acc_i == cur_acc_i)
			next_occur();
	}
};

void fts_create(struct FtsData *data);

#endif
