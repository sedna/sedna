/*
 * File:  update_history.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/ft/update_history.h"
#include "tr/ft/sequence_sorter.h"


tc_sort_type *update_history::ssr_types[ssr_n] = {&st_xptr::inst, &st_pos::inst, &st_uint64::inst};
const int update_history::ssr_inds[ssr_n] = {0, 0, 1};

update_history::update_history() : ssr()
{
	ssr.create_sorted_sequence(ssr_n, ssr_types, ssr_inds);
}

inline void update_history::add_node(const update_type ut, const xptr node)
{
	tuple t(2);
	t.cells[0] = tuple_cell::unsafenode(node);
	t.cells[1] = tuple_cell::atomic((int64_t)ut);
	this->ssr.add(t);
}

void update_history::add_insert_node(const xptr node)
{
	add_node(ut_insert, node);
}

void update_history::add_update_node(const xptr node)
{
	add_node(ut_update, node);
}

void update_history::add_delete_node(const xptr node)
{
	add_node(ut_delete, node);
}

void update_history::get_update_sequences(xptr_sequence **inserted, xptr_sequence **updated, xptr_sequence **deleted)
{
	sorted_sequence *ss = ssr.get_sorted_sequence();

	ss->lazy_sort();

	*inserted = new xptr_sequence();
	*updated = new xptr_sequence();
	*deleted = new xptr_sequence();

	tuple t(3);
	xptr cur_node = XNULL;
	update_type first_ut, last_ut;
	while (true)
	{
		xptr node;
		update_type ut;

		ss->next(t);
		if (t.is_eos())
		{
			node = XNULL; //add exta XNULL update at the end to process last update
			ut = ut_insert; //just to get rid of use of non-initialized variable later
		}
		else
		{
			node = t.cells[0].get_xptr();
			ut = (update_type)t.cells[2].get_xs_integer();
		}

		if (cur_node != node)
		{
			if (cur_node != XNULL)
			{
				if (first_ut == ut_insert)
				{//node created now
					switch (last_ut)
					{
					case ut_insert:
					case ut_update:
						(*inserted)->add(cur_node);
						break;
					case ut_delete:
						break; //no need to index
					default:
						U_ASSERT(false);
					}
				}
				else
				{//node existed before
					switch (last_ut)
					{
					case ut_insert:
					case ut_update:
						(*updated)->add(cur_node);
						break;
					case ut_delete:
						(*deleted)->add(cur_node);
						break; //no need to index
					default:
						U_ASSERT(false);
					}
				}
			}
			cur_node = node;
			first_ut = ut;
		}
		if (t.is_eos())
			break;
		last_ut = ut;
	}

	delete ss;
}
