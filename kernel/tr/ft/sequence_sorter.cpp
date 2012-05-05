/*
 * File:  sequence_sorter.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/ft/sequence_sorter.h"

static inline void put_xuint(char *buf, uint64_t x)
{
#ifdef BIG_ENDIAN_ORDER
		memcpy(buf, &x, sizeof(x));
#else
		for (size_t i = 0; i < sizeof(x); i++)
			buf[sizeof(x)-1-i] = ((char*)&x)[i];
#endif
}

static inline xptr ss_read(const xptr v, char *buf, int len)
{
    CHECKP(v);
    int sz = GET_FREE_SPACE(v);

    //if sz == len, v+len actually refers to next block, so we nee to use first branch to return correct value
	if (sz <= len)
	{
		memcpy(buf, XADDR(v), sz);
		const xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(v)))->nblk+sizeof(seq_blk_hdr);
        if ((len-sz) > 0)
		{
			CHECKP(nblk);
			memcpy(buf+sz, XADDR(nblk), len-sz);
		}
		return nblk + (len-sz);
	}
	else
	{
		memcpy(buf, XADDR(v), len);
		return v + len;
	}
}

st_xptr st_xptr::inst;
size_t st_xptr::get_tc_size(tuple_cell &tc)
{
	return sizeof(uint64_t);
}
size_t st_xptr::serialize_tc(tuple_cell &tc, char *buf, ss_data *data)
{
	const uint64_t p = tc.get_xptr().to_uint64();
	memcpy(buf, &p, sizeof(p));
	return sizeof(p);
}
size_t st_xptr::deserialize_tc(tuple_cell &tc, xptr *v, ss_data *data)
{
	uint64_t p;
	*v = ss_read(*v, (char*)&p, sizeof(p));
	tc = tuple_cell::unsafenode(uint64_to_xptr(p));
	return sizeof(p);
}
int st_xptr::compare(xptr *v1, xptr *v2, ss_data *data)
{
	const size_t sz = sizeof(uint64_t);
	data->set_buf(2 * sz);
	*v1 = ss_read(*v1, data->buf, sz);
	*v2 = ss_read(*v2, data->buf + sz, sz);
	return memcmp(data->buf, data->buf + sz, sz);
}


st_uint64 st_uint64::inst;
size_t st_uint64::get_tc_size(tuple_cell &tc)
{
	return sizeof(uint64_t);
}
size_t st_uint64::serialize_tc(tuple_cell &tc, char *buf, ss_data *data)
{
	uint64_t x = (uint64_t)tc.get_xs_integer();
	put_xuint(buf, x);
	return sizeof(x);
}
size_t st_uint64::deserialize_tc(tuple_cell &tc, xptr *v, ss_data *data)
{
	uint64_t x;
#ifdef BIG_ENDIAN_ORDER
	*v = ss_read(*v, &x, sizeof(x));
#else
	data->set_buf(sizeof(x));
	*v = ss_read(*v, data->buf, sizeof(x));
	for (size_t i = 0; i < sizeof(x); i++)
		((char*)&x)[i] = data->buf[sizeof(x)-1-i];
#endif
	tc = tuple_cell::atomic((int64_t)x);
	return sizeof(x);
}
int st_uint64::compare(xptr *v1, xptr *v2, ss_data *data)
{
	const size_t sz = sizeof(uint64_t);
	data->set_buf(2 * sz);
	*v1 = ss_read(*v1, data->buf, sz);
	*v2 = ss_read(*v2, data->buf + sz, sz);
	return memcmp(data->buf, data->buf + sz, sz);
}


st_uint64_desc st_uint64_desc::inst;
size_t st_uint64_desc::get_tc_size(tuple_cell &tc)
{
	return sizeof(uint64_t);
}
size_t st_uint64_desc::serialize_tc(tuple_cell &tc, char *buf, ss_data *data)
{
	uint64_t x = (uint64_t)tc.get_xs_integer();
	put_xuint(buf, ~x);
	return sizeof(x);
}
size_t st_uint64_desc::deserialize_tc(tuple_cell &tc, xptr *v, ss_data *data)
{
	uint64_t x;
#ifdef BIG_ENDIAN_ORDER
	*v = ss_read(*v, &x, sizeof(x));
#else
	data->set_buf(sizeof(x));
	*v = ss_read(*v, data->buf, sizeof(x));
	for (size_t i = 0; i < sizeof(x); i++)
		((char*)&x)[i] = data->buf[sizeof(x)-1-i];
#endif
	x = ~x;
	tc = tuple_cell::atomic((int64_t)x);
	return sizeof(x);
}
int st_uint64_desc::compare(xptr *v1, xptr *v2, ss_data *data)
{
	const size_t sz = sizeof(uint64_t);
	data->set_buf(2 * sz);
	*v1 = ss_read(*v1, data->buf, sz);
	*v2 = ss_read(*v2, data->buf + sz, sz);
	return memcmp(data->buf, data->buf + sz, sz);
}



st_pos st_pos::inst;
size_t st_pos::get_tc_size(tuple_cell &tc)
{
	return sizeof(uint64_t);
}
size_t st_pos::serialize_tc(tuple_cell &tc, char *buf, ss_data *data)
{
	put_xuint(buf, (uint64_t)data->pos);
	return sizeof(uint64_t);
}
size_t st_pos::deserialize_tc(tuple_cell &tc, xptr *v, ss_data *data)
{
	uint64_t x;
#ifdef BIG_ENDIAN_ORDER
	*v = ss_read(*v, &x, sizeof(x));
#else
	data->set_buf(sizeof(x));
	*v = ss_read(*v, data->buf, sizeof(x));
	for (size_t i = 0; i < sizeof(x); i++)
		((char*)&x)[i] = data->buf[sizeof(x)-1-i];
#endif
	tc = tuple_cell::atomic((int64_t)x);
	return sizeof(x);
}
int st_pos::compare(xptr *v1, xptr *v2, ss_data *data)
{
	const size_t sz = sizeof(uint64_t);
	data->set_buf(2 * sz);
	*v1 = ss_read(*v1, data->buf, sz);
	*v2 = ss_read(*v2, data->buf + sz, sz);
	return memcmp(data->buf, data->buf + sz, sz);
}


st_xptr_do st_xptr_do::inst;
size_t st_xptr_do::get_tc_size(tuple_cell &tc)
{
    xptr node = tc.get_node();
    CHECKP(node);
    shft sz = nid_get_size((t_nid *) XADDR(nodeGetNIDPtr(node)));

    U_ASSERT(sz <= PSTRMAXSIZE);

    sz += (sizeof(xptr) + sizeof(shft));
    return (sz > DATA_BLK_SIZE) ? 2*sizeof(xptr)+sizeof(shft) : sz;
}
size_t st_xptr_do::serialize_tc(tuple_cell &tc, char *buf, ss_data *data)
{
    xptr node=tc.get_node();
    CHECKP(node);

    shft sz;
    xptr addr;

    nid_parse(nodeGetNIDPtr(node), &addr, &sz);

    *((xptr*)buf) = node;
    *((shft*)(buf + sizeof(xptr))) = sz;

    /*
     * Seems that this case is impossible now since
     * PSTRMAXSIZE + sizeof(xptr) + sizeof(shft) < DATA_BLK_SIZE and
     * MAX_NIDE_SIZE (i.e. sz value) <= PSTRMAXSIZE
     */
    if ((sz + (sizeof(xptr) + sizeof(shft))) > DATA_BLK_SIZE)
    {
        *((xptr*)(buf + sizeof(xptr) + sizeof(shft))) = addr;
		return 2*sizeof(xptr)+sizeof(shft);
    }
    else
    {
		CHECKP(addr);
		memcpy(buf + sizeof(xptr) + sizeof(shft), XADDR(addr), sz);
		return sizeof(xptr) + sizeof(shft) + sz;
    }
}
size_t st_xptr_do::deserialize_tc(tuple_cell &tc, xptr *v, ss_data *data)
{
	xptr res;
	*v = ss_read(*v, (char*)&res, sizeof(res));
	tc = tuple_cell::node(res);

	shft sz;
	*v = ss_read(*v, (char*)&sz, sizeof(sz));
    if ((sz + (sizeof(xptr) + sizeof(shft))) > DATA_BLK_SIZE)
    {
		xptr tmp;
        *v = ss_read(*v, (char*)&tmp, sizeof(tmp));
		return 2*sizeof(xptr)+sizeof(shft);
    }
    else
	{
		data->set_buf(sz);
		*v = ss_read(*v, data->buf, sz); //FIXME: data is ignored here, no need to copy
		return sizeof(xptr) + sizeof(shft) + sz;
    }
}

inline void copy_nid(char *buf, xptr *v, size_t sz)
{
   if ((sz + (sizeof(xptr) + sizeof(shft))) > DATA_BLK_SIZE)
    {
		xptr addr;
		*v = ss_read(*v, (char*)&addr, sizeof(addr));
		CHECKP(addr);
		memcpy(buf, XADDR(addr), sz);
    }
    else
    {
		*v = ss_read(*v, buf, sz);
    }
}
int st_xptr_do::compare(xptr *v1, xptr *v2, ss_data *data)
{
	xptr tmp;
	*v1 = ss_read(*v1, (char*)&tmp, sizeof(tmp));
	*v2 = ss_read(*v2, (char*)&tmp, sizeof(tmp));
	shft _sz1, _sz2;
	*v1 = ss_read(*v1, (char*)&_sz1, sizeof(_sz1));
	*v2 = ss_read(*v2, (char*)&_sz2, sizeof(_sz2));
	size_t sz1 = _sz1, sz2 = _sz2;

	data->set_buf(sz1 + sz2);
	copy_nid(data->buf, v1, sz1);
	copy_nid(data->buf + sz1, v2, sz2);

	if (sz1 > sz2)
	{
		int r = memcmp(data->buf, data->buf+sz1, sz2);
		if (r == 0)
			return 1;
		else
			return r;
	}
	else if (sz1 < sz2)
	{
		int r = memcmp(data->buf, data->buf+sz1, sz1);
		if (r == 0)
			return -1;
		else
			return r;
	}
	else
		return memcmp(data->buf, data->buf+sz1, sz2);
}


static int ss_compare (xptr v1, xptr v2, const void *_data)
{
	if (v1 == v2)
		return 0;

	ss_data *data = (ss_data*)_data;
	for (int i = 0; i < data->n; i++)
	{
		int r = data->sort_types[i]->compare(&v1, &v2, data);
		if (r != 0)
			return r;
	}
	return 0;
}

static int ss_get_size (tuple& t, const void * _data)
{
	ss_data *data = (ss_data*)_data;
	size_t serialized_size = 0;
	for (int i = 0; i < data->n; i++)
		serialized_size += data->sort_types[i]->get_tc_size(t.cells[data->sort_inds[i]]);

	U_ASSERT(serialized_size < INT_MAX);
	return (int)serialized_size;
}

static void ss_serialize (tuple& t, xptr v1, const void *_data)
{
	ss_data *data = (ss_data*)_data;
	int sz = ss_get_size(t, _data);
	data->set_buf(sz);

	char *p = data->buf;
	for (int i = 0; i < data->n; i++)
		p += data->sort_types[i]->serialize_tc(t.cells[data->sort_inds[i]], p, data);
	U_ASSERT(p-data->buf == sz);

	CHECKP(v1);
    VMM_SIGNAL_MODIFICATION(v1);
	memcpy(XADDR(v1), data->buf, sz);
}

static void ss_serialize_2_blks (tuple& t, xptr& v1, shft size1, xptr& v2, const void *_data)
{
	ss_data *data = (ss_data*)_data;
	int sz = ss_get_size(t, _data);
	data->set_buf(sz);

	char *p = data->buf;
	for (int i = 0; i < data->n; i++)
		p += data->sort_types[i]->serialize_tc(t.cells[data->sort_inds[i]], p, data);
	U_ASSERT(p-data->buf == sz);

	U_ASSERT(size1 < sz);

	CHECKP(v1);
    VMM_SIGNAL_MODIFICATION(v1);
	memcpy(XADDR(v1), data->buf, size1);

	CHECKP(v2);
    VMM_SIGNAL_MODIFICATION(v2);
	memcpy(XADDR(v2), data->buf+size1, sz - size1);
}

static void ss_deserialize (tuple& t, xptr& v1, const void *_data)
{
	ss_data *data = (ss_data*)_data;
	U_ASSERT(t.size() == data->n);
	for (int i = 0; i < data->n; i++)
		data->sort_types[i]->deserialize_tc(t.cells[i], &v1, data);
}

static void ss_deserialize_2_blks (tuple& t, xptr& v1, shft size1, xptr& v2, const void *_data)
{
	//FIXME
	ss_deserialize(t, v1, _data);
}


sequence_sorter::sequence_sorter() : ss(NULL)
{
	data.buf = NULL;
	data.buf_size = 0;
}
sequence_sorter::~sequence_sorter()
{
	if (data.buf)
	{
		free(data.buf);
		data.buf = NULL;
	}
}
sorted_sequence *sequence_sorter::create_sorted_sequence(int n, tc_sort_type **sort_types, const int *sort_inds)
{
	U_ASSERT(ss == NULL);

	data.n = n;
	data.sort_types = sort_types;
	data.sort_inds = sort_inds;

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
