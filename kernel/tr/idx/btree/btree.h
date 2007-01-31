/*
 * File:  btree.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BTREE_H
#define _BTREE_H

#include "common/sedna.h"

#include "tr/structures/nodes.h"
#include "tr/idx/btree/btstruct.h"
#include "tr/idx/btree/btpage.h"

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   Btree interface functions. In all functions 'root' parameter is btree actual root page.
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

xptr			bt_create(xmlscm_type t);						/* create new btree instance */
void			bt_drop(const xptr &root);						/* drop existing btree instance (free all vmm memory) */

bt_cursor		bt_find   (const xptr &root, const bt_key& key);/* find key */
bt_cursor		bt_find_ge(const xptr &root, const bt_key& key);/* find key or nearest greater key */
bt_cursor		bt_find_gt(const xptr &root, const bt_key& key);/* find nearest greater key */
bt_cursor		bt_lm     (const xptr &root);					/* get left-most key */

void			bt_insert(xptr &root, const bt_key &key, const object &obj);
void			bt_modify(xptr &root, const bt_key &old_key, const bt_key &new_key, const object &obj);
void			bt_delete(xptr &root, const bt_key &key, const object &obj);	/* delete key/obj pair */
void			bt_delete(xptr &root, const bt_key &key);						/* delete key with all it's objects */
void           bt_drop_page(const btree_blk_hdr * pg);
#endif

