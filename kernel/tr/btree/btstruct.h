/*
 * File:  btstruct.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BTSTRUCT_H
#define _BTSTRUCT_H

#include "common/sedna.h"

#include "tr/structures/nodetypes.h"
#include "tr/btree/btpage.h"
#include "tr/executor/base/XMLDateTime.h"

#include <list>

//typedef xptr object;
//#define NULL_OBJECT	XNULL

template<typename object> class null_object {};

template<> class null_object<xptr>
{
public: static inline xptr get() { return XNULL; }
};
template<> class null_object<int>
{
public: static inline int get() { return 0; }
};
template<> class null_object<uint64_t>
{
public: static inline int get() { return 0; }
};
//this is useable only if object type is defined
#define NULL_OBJECT null_object<object>::get()


struct bt_path_item {
    xptr pg;
    int idx;

    bt_path_item() : pg(XNULL), idx(0) {}
    bt_path_item(const xptr apg, const int aidx) : pg(apg), idx(aidx) {}
};

typedef std::list<bt_path_item> bt_path;

class bt_key {
private:
    union {
        int64_t    i_v;
        float  f_v;
        double d_v;
        char*  s_v;
        xs_packed_duration dur_v;
        xs_packed_datetime dt_v;
    } v;
    xmlscm_type type;

    void free()
    {
        if (type == xs_string)
            delete [] v.s_v;
        v.s_v = NULL;
    }	/* free allocated dynamic memory */

    void init(const bt_key& k);
    void init(char* pg, shft key_idx);

    friend int bt_cmp_key(const bt_key& k1, const bt_key& k2);
    friend int bt_cmp_key(char* pg, const void* tab_el, const bt_key& k2);

public:
    bt_key() : type(xs_anyType) {}
    /* the following constructor allocates key content in dynamic memory;
       it inits content from a btree page and a key index;
     */
	bt_key(char* pg, shft key_idx) { init(pg, key_idx); }
	~bt_key() { free(); }

	bt_key& operator=(const bt_key& k)
    {
        free();
        init(k);
        return *this;
    }

	bt_key(const bt_key& k) { init(k); }

    /* initialize key with content of given key in given page;
     */
	void setnew(char* pg, shft key_idx)
    {
        free();
        init(pg, key_idx);
    }
    void setnew(int64_t nv);
    void setnew(float nv);
    void setnew(double nv);
    void setnew(const char* nv);
    void setnew_dateTime(const xs_packed_datetime& dt, xmlscm_type t);
    void setnew_duration(const xs_packed_duration& dur, xmlscm_type t);

    int64_t get_int() const { U_ASSERT(type == xs_integer); return v.i_v; };
    float get_float() const { U_ASSERT(type == xs_float); return v.f_v; };
    double get_double() const { U_ASSERT(type == xs_double); return v.d_v; };
    char * get_string() const { U_ASSERT(type == xs_string); return v.s_v; };
    xs_packed_datetime get_datetime() const { return v.dt_v; };
    xs_packed_duration get_duration() const { return v.dur_v; };

    void * data () const { return ((type == xs_string )?(void*)(v.s_v):(void*)&v); }

    xmlscm_type get_type() const { return type; }
    size_t get_size() const;
};




bool operator==(const bt_key& k1, const bt_key& k2);
bool operator!=(const bt_key& k1, const bt_key& k2);
bool operator> (const bt_key& k1, const bt_key& k2);
bool operator>=(const bt_key& k1, const bt_key& k2);
bool operator< (const bt_key& k1, const bt_key& k2);
bool operator<=(const bt_key& k1, const bt_key& k2);

/* cursor over the objects/keys (in general may span a number of pages following next keys or
   following next object - page cluster case)
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Note: when traversing over objects that occupy a number of pages inside cluster, in each page of the
   cluster, cursor's key is the 1-st one in the key table, in accordance with page clusters architecture
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */
template<typename object>
class bt_cursor_tmpl
{
private:
    xptr    cur_page;		/* current leaf page (may flow) */
    shft    key_idx;		/* index of cursor's key in key table */
    shft    obj_idx;		/* index of next object in corresponding chunk table; when cursor is created
							   points to the 0-th element */
    shft    chnk_size;		/* size of the current chunk */
    int     obj_count;		/* total count of objects sticking to the cursor's key (joint for clusters);
							   is resetted when going to next/prev key */
    bt_key  key;			/* current key */

public:
    bt_cursor_tmpl();
    bt_cursor_tmpl(char* pg, shft key_idx);
    object  bt_next_obj();/* obtain next object - begining from 0-th */
	void  bt_set_next_obj(object obj);
    bool    bt_next_key();	/* focus on next key; note that initially cursor is already focused
							   at the 0-th key. Returns the key that was previously in the focus */
    bool    is_null() const;/* if there are keys belonging to this cursor */
    const bt_key &get_key() const { return key; }
};

/* the codes used to indicate the searched key is bigger than all keys/objs in corr-ing table (BT_LEFTMOST)
   or smaller than all keys/objs in corr-ing table (BT_LEFTMOST). Note that BT_LEFTMOST is by the way the index
   of first element of the table and thus can be used for indexing insertion position for new key/obj;
   I realy wished to set BT_RIGHTMOST to somewhat negative value (-1) but it's expected to be assigned to
   unsigned variables (shft). Thus, I set it to the biggest value possible for 'shft' type that can never be
   real key/object index in life, as long as we deal with 64K pages */
#define BT_RIGHTMOST	65535
#define	BT_LEFTMOST		0

#endif
