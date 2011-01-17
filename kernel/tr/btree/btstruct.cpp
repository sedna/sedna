/*
 * File:  btstruct.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/vmm/vmm.h"

#include "tr/btree/btstruct.h"
#include "tr/btree/btintern.h"

btree_blk_hdr a;

///////////////////////////////////////////////////////////////////////////////
/// bt_key implementation
///////////////////////////////////////////////////////////////////////////////

void bt_key::init(const bt_key& k)
{
    type = k.type;
    switch (type)
    {
        case xs_anyType	: return;
        case xs_integer	: v.i_v = k.v.i_v; break;
        case xs_float	: v.f_v = k.v.f_v; break;
        case xs_double	: v.d_v = k.v.d_v; break;
        case xs_string	: v.s_v = se_new char[strlen(k.v.s_v) + 1];
                          strcpy(v.s_v, k.v.s_v);
			  break;
	case xs_date:
	case xs_time:
	case xs_dateTime:
			v.dt_v = k.v.dt_v; break;
	case xs_duration:
	case xs_yearMonthDuration:
	case xs_dayTimeDuration:
			v.dur_v = k.v.dur_v; break;
        default			: throw USER_EXCEPTION2(SE1008, "Unsupported type of index");
    }
}

void bt_key::init(char* pg, shft key_idx)
{
    type = BT_KEY_TYPE(pg);
    shft* key_tab_slot = (shft*)(BT_KEY_TAB_AT(pg, key_idx));

    switch (type)
    {
        case xs_integer	: v.i_v = *(int64_t*)   key_tab_slot; break;
        case xs_float	: v.f_v = *(float*) key_tab_slot; break;
        case xs_double	: v.d_v = *(double*)key_tab_slot; break;
        case xs_string	: {
                              shft size = *(key_tab_slot + 1);
                              v.s_v = se_new char[size + 1];
                              v.s_v[size] = '\0';
                              memcpy(v.s_v, pg + *key_tab_slot, size);
                              break;
			}
	case xs_date:
	case xs_time:
	case xs_dateTime: v.dt_v = *(xs_packed_datetime*) key_tab_slot; break;

	case xs_yearMonthDuration:
	case xs_dayTimeDuration:	v.dur_v = *(xs_packed_duration*) key_tab_slot; break;

        default			: throw USER_EXCEPTION2(SE1008, "Unsupported type of index");
    }
}

size_t bt_key::get_size() const
{
    switch (type)
    {
        case xs_integer	: return sizeof(int64_t);
        case xs_float	: return sizeof(float);
        case xs_double	: return sizeof(double);
        case xs_string	: return strlen(v.s_v);
	case xs_date:
	case xs_time:
	case xs_dateTime: return sizeof(xs_packed_datetime);
	case xs_yearMonthDuration:
	case xs_dayTimeDuration: return sizeof(xs_packed_duration);

	default			: throw USER_EXCEPTION2(SE1008, "Unsupported type of index");
    }
}

void bt_key::setnew(int64_t nv)
{
    free();
    type = xs_integer;
    v.i_v = nv;
}

void bt_key::setnew(float nv)
{
    free();
    type = xs_float;
    v.f_v = nv;
}

void bt_key::setnew(double nv)
{
    free();
    type = xs_double;
    v.d_v = nv;
}

void bt_key::setnew(const char* nv)
{
    free();
    type = xs_string;
    v.s_v = se_new char[strlen(nv) + 1];
    strcpy(v.s_v, nv);
}

void bt_key::setnew_dateTime(const xs_packed_datetime& dt, xmlscm_type t)
{
    free();
    type = t;
    v.dt_v = dt;
}

void bt_key::setnew_duration(const xs_packed_duration& dur, xmlscm_type t)
{
    free();
    type = t;
    v.dur_v = dur;
}

bool operator==(const bt_key& k1, const bt_key& k2)
{
    return (bt_cmp_key(k1, k2) == 0);
}

bool operator!=(const bt_key& k1, const bt_key& k2)
{
    return (bt_cmp_key(k1, k2) != 0);
}

bool operator> (const bt_key& k1, const bt_key& k2)
{
    return (bt_cmp_key(k1, k2) >  0);
}

bool operator>=(const bt_key& k1, const bt_key& k2)
{
    return (bt_cmp_key(k1, k2) >= 0);
}

bool operator< (const bt_key& k1, const bt_key& k2)
{
    return (bt_cmp_key(k1, k2) <  0);
}

bool operator<=(const bt_key& k1, const bt_key& k2)
{
    return (bt_cmp_key(k1, k2) <= 0);
}




///////////////////////////////////////////////////////////////////////////////
/// bt_cursor implementation
///////////////////////////////////////////////////////////////////////////////

template<typename object>
bt_cursor_tmpl<object>::bt_cursor_tmpl()
{
    cur_page = XNULL;
    key_idx = 0;
    obj_idx = 0;
    chnk_size = 0;
    obj_count = 0;
}

/* note, page must be uploaded in memory */
template<typename object>
bt_cursor_tmpl<object>::bt_cursor_tmpl(char* pg, shft the_key_idx)
{
    xptr pg_xptr = ADDR2XPTR(pg);

#ifdef PERMIT_CLUSTERS
    /* in case of cluster cursors may be constructed only from the head of the cluster */
    if (BT_IS_CLUS(pg) && !(BT_IS_CLUS_HEAD(pg)) && !(BT_IS_CLUS_TAIL(pg)))
        throw USER_EXCEPTION2(SE1008, "Trying to construct cursor from non-head cluster page");
#endif

    /* calculate current chunk size */
    shft* chnk_tab_slot = (shft*)BT_CHNK_TAB_AT(pg, the_key_idx);
    chnk_size = *(chnk_tab_slot + 1);
    cur_page = pg_xptr;
    key_idx = the_key_idx;
    obj_count = 0;
    obj_idx = 0;

    while (key_idx >= BT_KEY_NUM(pg)) {
        cur_page = BT_NEXT(pg);

        if (cur_page == XNULL) { return; }

        CHECKP(cur_page);
        pg = (char*)XADDR(cur_page);

        key_idx = 0;
    }

	key.setnew(pg, the_key_idx);
}

template<typename object>
void  bt_cursor_tmpl<object>::bt_set_next_obj(object obj)
{
	//object  result;
    char*   pg;
    char*   chnk;

    if (cur_page == XNULL)
        /* XNULL current page - we must be out of the list of keys in our btree;
           this is the case when we moved through the sequence of keys using bt_next_key()
           behind the right-most key */
        throw USER_EXCEPTION2(SE1008, "XPTR not found");

    CHECKP(cur_page);

    if (obj_idx == chnk_size)
    {
#ifdef PERMIT_CLUSTERS
        char* cpage = (char*)XADDR(cur_page);

        if (BT_IS_CLUS(cpage) && !BT_IS_CLUS_TAIL(cpage))
        {
            /* note: key_idx must be 0, and it should not be changed */
            if (key_idx != 0)
                throw USER_EXCEPTION2(SE1008, "More than one key in non-tail cluster page");

            cur_page = BT_NEXT(cpage);
            if (cur_page == XNULL)
                throw USER_EXCEPTION2(SE1008, "Cluster has no tail page");

            CHECKP(cur_page);

            /* re-initialize cursor fields */
            obj_idx = 0;
            chnk_size = *((shft*)BT_CHNK_TAB_AT((char*)XADDR(cur_page), 0) + 1);
        }
        else throw USER_EXCEPTION2(SE1008, "NYI");
#else
        throw USER_EXCEPTION2(SE1008, "NYI");
#endif
	}

    /* construct result object; note the target page is already in memory */
    pg = (char*)XADDR(cur_page);
	VMM_SIGNAL_MODIFICATION(cur_page);
    chnk = (pg + *(shft*)BT_CHNK_TAB_AT(pg, key_idx));
    *(object*)(chnk + sizeof(object) * obj_idx)=obj;

    obj_idx++;
    obj_count++;
   // return result;
}
template<typename object>
object bt_cursor_tmpl<object>::bt_next_obj()
{
    object  result;
    char*   pg;
    char*   chnk;

    if (cur_page == XNULL)
        /* XNULL current page - we must be out of the list of keys in our btree;
           this is the case when we moved through the sequence of keys using bt_next_key()
           behind the right-most key */
        return NULL_OBJECT;

    CHECKP(cur_page);

    if (obj_idx == chnk_size)
    {
#ifdef PERMIT_CLUSTERS
        char* cpage = (char*)XADDR(cur_page);

        if (BT_IS_CLUS(cpage) && !BT_IS_CLUS_TAIL(cpage))
        {
            /* note: key_idx must be 0, and it should not be changed */
            if (key_idx != 0)
                throw USER_EXCEPTION2(SE1008, "More than one key in non-tail cluster page");

            cur_page = BT_NEXT(cpage);
            if (cur_page == XNULL)
                throw USER_EXCEPTION2(SE1008, "Cluster has no tail page");

            CHECKP(cur_page);

            /* re-initialize cursor fields */
            obj_idx = 0;
            chnk_size = *((shft*)BT_CHNK_TAB_AT((char*)XADDR(cur_page), 0) + 1);
        }
        else return NULL_OBJECT;
#else
        return NULL_OBJECT;
#endif
	}

    /* construct result object; note the target page is already in memory */
    pg = (char*)XADDR(cur_page);
    chnk = (pg + *(shft*)BT_CHNK_TAB_AT(pg, key_idx));
    result = *(object*)(chnk + sizeof(object) * obj_idx);
    obj_idx++;
    obj_count++;
    return result;
}

template<typename object>
bool bt_cursor_tmpl<object>:: bt_next_key()
{
    if (cur_page == XNULL) /* we went behind the right-most key in our btree */
        return false;

    CHECKP(cur_page);

    char* cpage = (char*)XADDR(cur_page);

    /* shift to next key */
#ifdef PERMIT_CLUSTERS
    if (BT_IS_CLUS(cpage))
    {
        cpage = bt_cluster_tail(cpage);
        cur_page = ADDR2XPTR(cpage);
    }
#endif

    /* construct result key - the key cursor is currently positioned at;
       note, the target page is already in memory */
	if (key_idx == BT_KEY_NUM(cpage) - 1)
    {
        cur_page = BT_NEXT(cpage);

        if (cur_page == XNULL) return false;

        CHECKP(cur_page);
        cpage = (char*)XADDR(cur_page);

        key_idx = 0;
    }
    else key_idx++;

    key.setnew(cpage, key_idx);
    chnk_size = *((shft*)BT_CHNK_TAB_AT(cpage, key_idx) + 1);
    obj_idx = 0;
    obj_count = 0;

    return true;
}

template<typename object>
bool bt_cursor_tmpl<object>::is_null() const
{
    return cur_page == XNULL;
}



#define MAKE_IMPLS(t) \
    template bt_cursor_tmpl<t>::bt_cursor_tmpl(); \
    template bt_cursor_tmpl<t>::bt_cursor_tmpl(char* pg, shft the_key_idx); \
    template t bt_cursor_tmpl<t>::bt_next_obj(); \
    template bool bt_cursor_tmpl<t>::bt_next_key(); \
    template bool bt_cursor_tmpl<t>::is_null() const; \
    template void bt_cursor_tmpl<t>::bt_set_next_obj(t obj);

#include "tr/btree/make_impl.h"
