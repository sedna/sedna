/*
 * File:  sequence_sorter.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEQUENCE_SORTER_H
#define SEQUENCE_SORTER_H

#include "tr/executor/base/sorted_sequence.h"


class tc_sort_type;
struct ss_data
{
	int n;
	tc_sort_type **sort_types;
	const int *sort_inds;
	uint64_t pos;
	char *buf;
	size_t buf_size;

	void set_buf(size_t sz)
	{
		if (this->buf_size < sz)
		{
			this->buf_size = sz; //FIXME
			this->buf = (char*)realloc(this->buf, this->buf_size);
		}
	}
};

class tc_sort_type
{
public:
	//all functions excep compare return size of serialized data
	virtual size_t get_tc_size(tuple_cell &tc) = 0;
	virtual size_t serialize_tc(tuple_cell &tc, char *buf, ss_data *data) = 0;
	virtual size_t deserialize_tc(tuple_cell &tc, xptr *v, ss_data *data) = 0;
	//compare and move pointers by size of serialized data
	virtual int compare(xptr *v1, xptr *v2, ss_data *data) = 0;
};

class st_xptr : public tc_sort_type
{
public:
	static st_xptr inst;
	virtual size_t get_tc_size(tuple_cell &tc);
	virtual size_t serialize_tc(tuple_cell &tc, char *buf, ss_data *data);
	virtual size_t deserialize_tc(tuple_cell &tc, xptr *v, ss_data *data);
	virtual int compare(xptr *v1, xptr *v2, ss_data *data);
};
class st_uint64 : public tc_sort_type
{
public:
	static st_uint64 inst;
	virtual size_t get_tc_size(tuple_cell &tc);
	virtual size_t serialize_tc(tuple_cell &tc, char *buf, ss_data *data);
	virtual size_t deserialize_tc(tuple_cell &tc, xptr *v, ss_data *data);
	virtual int compare(xptr *v1, xptr *v2, ss_data *data);
};
class st_uint64_desc : public tc_sort_type
{
public:
	static st_uint64_desc inst;
	virtual size_t get_tc_size(tuple_cell &tc);
	virtual size_t serialize_tc(tuple_cell &tc, char *buf, ss_data *data);
	virtual size_t deserialize_tc(tuple_cell &tc, xptr *v, ss_data *data);
	virtual int compare(xptr *v1, xptr *v2, ss_data *data);
};
class st_pos : public tc_sort_type
{
public:
	static st_pos inst;
	virtual size_t get_tc_size(tuple_cell &tc);
	virtual size_t serialize_tc(tuple_cell &tc, char *buf, ss_data *data);
	virtual size_t deserialize_tc(tuple_cell &tc, xptr *v, ss_data *data);
	virtual int compare(xptr *v1, xptr *v2, ss_data *data);
};
class st_xptr_do : public tc_sort_type
{
public:
	static st_xptr_do inst;
	virtual size_t get_tc_size(tuple_cell &tc);
	virtual size_t serialize_tc(tuple_cell &tc, char *buf, ss_data *data);
	virtual size_t deserialize_tc(tuple_cell &tc, xptr *v, ss_data *data);
	virtual int compare(xptr *v1, xptr *v2, ss_data *data);
};

class sequence_sorter
{
public:
	sequence_sorter();
	~sequence_sorter();

	//contents of tuples in the returned sequence depend on sort_types and sort_inds, not tuples from initial sequence
	//returned sequence  should then be freed using delete
	sorted_sequence *create_sorted_sequence(int n, tc_sort_type **sort_types, const int *sort_inds);
	sorted_sequence *get_sorted_sequence() { return ss; }
	void add(tuple &t);
private:
	struct ss_data data;
	sorted_sequence *ss;
};

#endif /* SEQUENCE_SORTER_H */
