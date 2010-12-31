/*
 * File:  sequence_sorter.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/ft/sequence_sorter.h"

static inline void ss_read(const xptr v, char *buf, int len)
{
    CHECKP(v);
    int sz = GET_FREE_SPACE(v);

	if (sz < len)
	{
		memcpy(buf, XADDR(v), sz);
		const xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(v)))->nblk+sizeof(seq_blk_hdr);
		CHECKP(nblk);
		memcpy(buf+sz, XADDR(nblk), len-sz);
	}
	else
		memcpy(buf, XADDR(v), len);
}

static int ss_compare (xptr v1, xptr v2, const void *data)
{
	if (v1 == v2)
		return 0;

	struct sequence_sorter::ss_data * const ss_data = (struct sequence_sorter::ss_data *)(data);
	ss_read(v1, ss_data->buf, ss_data->serialized_size);
	ss_read(v2, ss_data->buf+ss_data->serialized_size, ss_data->serialized_size);
	return memcmp(ss_data->buf, ss_data->buf+ss_data->serialized_size, ss_data->serialized_size);
}

//returns size of serialized data
static inline int ss_get_tc_serialized_size(sequence_sorter::sort_type st)
{
	switch (st)
	{
	case sequence_sorter::st_xptr:
		return sizeof(uint64_t);
	case sequence_sorter::st_uint64:
	case sequence_sorter::st_uint64_desc:
	case sequence_sorter::st_pos:
		return sizeof(uint64_t);
	default:
		U_ASSERT(false);
        return 0;
	}
}

static inline void put_xuint(char *buf, uint64_t x)
{
#ifdef BIG_ENDIAN_ORDER
		memcpy(buf, &x, sizeof(x));
#else
		for (int i = 0; i < sizeof(x); i++)
			buf[sizeof(x)-1-i] = ((char*)&x)[i];
#endif
}

//returns size of serialized data
static inline int ss_serialize_tc(tuple_cell &tc, sequence_sorter::sort_type st, char *buf, struct sequence_sorter::ss_data *data)
{
	switch (st)
	{
	case (sequence_sorter::st_xptr):
		{
		const uint64_t p = tc.get_xptr().to_uint64();
		memcpy(buf, &p, sizeof(p));
		return sizeof(p);
		}
	case (sequence_sorter::st_uint64):
		{
		uint64_t x = (uint64_t)tc.get_xs_integer();
		put_xuint(buf, x);
		return sizeof(x);
		}
	case (sequence_sorter::st_uint64_desc):
		{
		uint64_t x = (uint64_t)tc.get_xs_integer();
		put_xuint(buf, ~x);
		return sizeof(x);
		}
	case (sequence_sorter::st_pos):
		put_xuint(buf, (uint64_t)data->pos);
		return sizeof(uint64_t);
	default:
		U_ASSERT(false);
		return 0;
	}
}
//returns size of serialized data
static inline int ss_deserialize_tc(tuple_cell &tc, sequence_sorter::sort_type st, char *buf)
{
	switch (st)
	{
	case (sequence_sorter::st_xptr):
		{
		uint64_t p;
		memcpy(&p, buf, sizeof(p));
		tc = tuple_cell::unsafenode(uint64_to_xptr(p));
		return sizeof(p);
		}
	case (sequence_sorter::st_uint64):
	case (sequence_sorter::st_pos):
		{
		uint64_t x;
#ifdef BIG_ENDIAN_ORDER
		memcpy(&x, buf, sizeof(x));
#else
		for (int i = 0; i < sizeof(x); i++)
			((char*)&x)[i] = buf[sizeof(x)-1-i];
#endif
		tc = tuple_cell::atomic((int64_t)x);
		return sizeof(x);
		}
	case (sequence_sorter::st_uint64_desc):
		{
		uint64_t x;
#ifdef BIG_ENDIAN_ORDER
		memcpy(&x, buf, sizeof(x));
#else
		for (int i = 0; i < sizeof(x); i++)
			((char*)&x)[i] = buf[sizeof(x)-1-i];
#endif
		x = ~x;
		tc = tuple_cell::atomic((int64_t)x);
		return sizeof(x);
		}
	default:
		U_ASSERT(false);
		return 0;
	}
}

static int ss_get_size (tuple& t, const void *data)
{
	return ((struct sequence_sorter::ss_data *)(data))->serialized_size;
}

//buf must be of size returned by ss_get_size
static void ss_serialize_to_buf(tuple& t, char *buf, struct sequence_sorter::ss_data *data)
{
	char *p = buf;

	for (int i = 0; i < data->n; i++)
		p += ss_serialize_tc(t.cells[data->sort_inds[i]], data->sort_types[i], p, data);

	U_ASSERT(p-buf == data->serialized_size);
}

static void ss_serialize (tuple& t, xptr v1, const void *_data)
{
	struct sequence_sorter::ss_data * const data = (struct sequence_sorter::ss_data*)_data;
	ss_serialize_to_buf(t, data->buf, data);

	CHECKP(v1);
    VMM_SIGNAL_MODIFICATION(v1);
	memcpy(XADDR(v1), data->buf, data->serialized_size);
}

static void ss_serialize_2_blks (tuple& t, xptr& v1, shft size1, xptr& v2, const void *_data)
{
	struct sequence_sorter::ss_data * const data = (struct sequence_sorter::ss_data*)_data;
	ss_serialize_to_buf(t, data->buf, data);

	U_ASSERT(size1 < data->serialized_size);

	CHECKP(v1);
    VMM_SIGNAL_MODIFICATION(v1);
	memcpy(XADDR(v1), data->buf, size1);

	CHECKP(v2);
    VMM_SIGNAL_MODIFICATION(v2);
	memcpy(XADDR(v2), data->buf+size1, data->serialized_size - size1);
}

static void ss_deserialize_buf(tuple& t, struct sequence_sorter::ss_data *data)
{
	char *p = data->buf;
	U_ASSERT(t.cells_number == data->n);
	for (int i = 0; i < data->n; i++)
		p += ss_deserialize_tc(t.cells[i], data->sort_types[i], p);
	U_ASSERT((p-data->buf) == data->serialized_size);
}

static void ss_deserialize (tuple& t, xptr& v1, const void *_data)
{
	struct sequence_sorter::ss_data * const data = (struct sequence_sorter::ss_data*)_data;
	ss_read(v1, data->buf, data->serialized_size);
	ss_deserialize_buf(t, data);
}

static void ss_deserialize_2_blks (tuple& t, xptr& v1, shft size1, xptr& v2, const void *_data)
{
	struct sequence_sorter::ss_data * const data = (struct sequence_sorter::ss_data*)_data;
	U_ASSERT(size1 < data->serialized_size);
	ss_read(v1, data->buf, size1);
	ss_read(v2, data->buf+size1, data->serialized_size-size1);
	ss_deserialize_buf(t, data);
}


sequence_sorter::sequence_sorter() : ss(NULL)
{
	data.buf = NULL;
}
sequence_sorter::~sequence_sorter()
{
	if (data.buf)
	{
		free(data.buf);
		data.buf = NULL;
	}
}
sorted_sequence *sequence_sorter::create_sorted_sequence(int n, const sort_type *sort_types, const int *sort_inds)
{
	U_ASSERT(ss == NULL);

	data.n = n;
	data.sort_types = sort_types;
	data.sort_inds = sort_inds;
	data.serialized_size = 0;
	for (int i = 0; i < data.n; i++)
		data.serialized_size += ss_get_tc_serialized_size(data.sort_types[i]);
	if (data.buf)
		free(data.buf);
	data.buf = (char*)malloc(data.serialized_size*2);

	ss = se_new sorted_sequence(ss_compare, ss_get_size, ss_serialize,
		ss_serialize_2_blks, ss_deserialize, ss_deserialize_2_blks, &data);

	return ss;
}

void sequence_sorter::add(tuple &t)
{
	U_ASSERT(ss != NULL);
	ss->add(t);
	++data.pos;
}
