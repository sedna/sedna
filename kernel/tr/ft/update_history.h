/*
 * File:  update_history.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef UPDATE_HISTORY_H
#define UPDATE_HISTORY_H

#include "common/xptr.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/ft/sequence_sorter.h"

//used to update full-text indexes - db operations add updated nodes to update_history and
//then this class returns sequences of inserted, deleted and updated nodes to be updated in the index
class update_history
{
private:
	sequence_sorter ssr;
	static const int ssr_n = 3;
	static tc_sort_type *ssr_types[ssr_n];
	static const int ssr_inds[ssr_n];

public:
	update_history();

	enum update_type {ut_insert, ut_update, ut_delete, ut_invalid};
	//node must contain pointer to indirection
	void add_node(const update_type ut, const xptr node);
	void add_insert_node(const xptr node);
	void add_update_node(const xptr node);
	void add_delete_node(const xptr node);

	void get_update_sequences(xptr_sequence **inserted, xptr_sequence **updated, xptr_sequence **deleted);
	void free_update_sequences(xptr_sequence *inserted, xptr_sequence *updated, xptr_sequence *deleted){
		delete inserted;
		delete updated;
		delete deleted;
	}
};

#endif /* UPDATE_HISTORY_H */
