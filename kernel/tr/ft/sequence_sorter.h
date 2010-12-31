/*
 * File:  sequence_sorter.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEQUENCE_SORTER_H
#define SEQUENCE_SORTER_H

#include "tr/executor/base/sorted_sequence.h"

class sequence_sorter
{
public:
	//currenly all types have fixed serialized size and serializations can be compared byte-wise
	//if this changes most of the functions passed to sorted_sequence must be changed
	enum sort_type { st_xptr, st_uint64, st_uint64_desc, st_pos };

	struct ss_data
	{
		int n;
		const sort_type *sort_types;
		const int *sort_inds;
		uint64_t pos;
		int serialized_size;
		char *buf; //buffer of size serialized_size*2
	};

	sequence_sorter();
	~sequence_sorter();

	//contents of tuples in the returned sequence depend on sort_types and sort_inds, not tuples from initial sequence
	//returned sequence  should then be freed using delete
	sorted_sequence *create_sorted_sequence(int n, const sort_type *sort_types, const int *sort_inds);
	sorted_sequence *get_sorted_sequence() { return ss; }
	void add(tuple &t);
private:
	struct ss_data data;
	sorted_sequence *ss;
};

#endif /* SEQUENCE_SORTER_H */
