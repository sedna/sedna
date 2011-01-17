/*
 * File:  btree.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BTREE_H
#define _BTREE_H

#include "common/sedna.h"

#include "tr/btree/btstruct.h"
#include "tr/btree/btpage.h"

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Btree interface functions. In all functions 'root' parameter is btree actual root page.
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

xptr			bt_create(xmlscm_type t);						/* create new btree instance */
void			bt_drop(const xptr root);						/* drop existing btree instance (free all vmm memory) */

int bt_walk_nodes(const xptr root); /* Calculate used block count */

template<typename object>
bt_cursor_tmpl<object>		bt_find_tmpl   (const xptr root, const bt_key& key);/* find key */
template<typename object>
bt_cursor_tmpl<object>		bt_find_ge_tmpl(const xptr root, const bt_key& key);/* find key or nearest greater key */
template<typename object>
bt_cursor_tmpl<object>		bt_find_gt_tmpl(const xptr root, const bt_key& key);/* find nearest greater key */
template<typename object>
bt_cursor_tmpl<object>		bt_lm_tmpl     (const xptr root);					/* get left-most key */

template<typename object>
void			bt_insert_tmpl(xptr &root, const bt_key &key, const object &obj, bool with_bt=true);
template<typename object>
void			bt_modify_tmpl(xptr &root, const bt_key &old_key, const bt_key &new_key, const object &obj);
template<typename object>
void			bt_delete_tmpl(xptr &root, const bt_key &key, const object &obj);	/* delete key/obj pair */
template<typename object>
void			bt_delete_tmpl(xptr &root, const bt_key &key);						/* delete key with all it's objects */
void           bt_drop_page(const btree_blk_hdr * pg);


//wrapper for btree with object=xptr
typedef bt_cursor_tmpl<xptr> bt_cursor;

inline bt_cursor bt_find   (const xptr root, const bt_key& key)
{
	return bt_find_tmpl<xptr>(root, key);
}
inline bt_cursor bt_find_ge(const xptr root, const bt_key& key)
{
	return bt_find_ge_tmpl<xptr>(root, key);
}
inline bt_cursor bt_find_gt(const xptr root, const bt_key& key)
{
	return bt_find_gt_tmpl<xptr>(root, key);
}
inline bt_cursor bt_lm     (const xptr root)
{
	return bt_lm_tmpl<xptr>(root);
}

inline void			bt_insert(xptr &root, const bt_key &key, const xptr obj, bool with_bt=true)
{
	bt_insert_tmpl<xptr>(root, key, obj, with_bt);
}
inline void			bt_modify(xptr &root, const bt_key &old_key, const bt_key &new_key, const xptr obj)
{
	bt_modify_tmpl<xptr>(root, old_key, new_key, obj);
}
inline void			bt_delete(xptr &root, const bt_key &key, const xptr obj)
{
	bt_delete_tmpl<xptr>(root, key, obj);
}
inline void			bt_delete(xptr &root, const bt_key &key)
{
	bt_delete_tmpl<xptr>(root, key);
}


#endif
