/*
 * File:  ft_storage.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _FT_STORAGE_H
#define _FT_STORAGE_H

#include "common/xptr.h"
#include "tr/idx/btree/btree.h" //FIXME: move to cpp

struct FtsData
{
	xptr btree_root;
	
	FtsData() : btree_root(XNULL) {}

	//updates are performed as such:
	// 1. begin_update is called
	// 2. update functions are called, arguments (word, acc, word_ind) must be ascending during these calls
	// 3.end_update is called
	void begin_update() {}
	void add_word_occur(const char *word, const xptr acc, const int word_ind)
	{
		bt_key bkey;
		bkey.setnew(word);
		bt_insert_tmpl<ft_idx_btree_element>(btree_root, bkey, ft_idx_btree_element(acc, word_ind));
	}
	void del_word_occur(const char *word, const xptr acc, const int word_ind)
	{
		bt_key bkey;
		bkey.setnew(word);
		bt_delete_tmpl<ft_idx_btree_element>(btree_root, bkey, ft_idx_btree_element(acc, word_ind));
	}
	//end update, and modify FtsData at pointer dest, so that it will become identical to *this
	void end_update(struct FtsData *dest)
	{
		*dest = *this;
	}
};

struct FtsScanData
{
	bt_cursor_tmpl<ft_idx_btree_element> bcur;
	ft_idx_btree_element ce;

	//start scan
	void init(const struct FtsData *fts_data, const char *word)
	{
		bt_key bkey;
		bkey.setnew(word);
		bcur = bt_find_tmpl<ft_idx_btree_element>(fts_data->btree_root, bkey);
		ce = bcur.bt_next_obj();
	}

	bool at_end()
	{
		return ce.node == XNULL;
	}

	xptr cur_node()
	{
		return ce.node;
	}

	int cur_word_ind()
	{
		return ce.word_ind;
	}

	//move to next occur
	void next_occur()
	{
		ce = bcur.bt_next_obj();
	}

	void skip_node(const xptr p)
	{
		while (ce.node == p)
			ce = bcur.bt_next_obj();
	}

};


void fts_create(struct FtsData *data);

#endif
