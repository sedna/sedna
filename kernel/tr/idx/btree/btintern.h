/*
 * File:  btintern.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BTINTERN_H
#define _BTINTERN_H

#include "common/sedna.h"
#include "tr/idx/btree/btstruct.h"

/** internal btree implementation fucntions */

/* auxiliary functions */
void				bt_page_consistency(char* pg, bt_key* key);
void				bt_page_markup(char* pg, xmlscm_type t);


/* comparison functions */
int					bt_cmp_key(const bt_key& k1, const bt_key& k2);
int					bt_cmp_key(char* pg, const void* tab_el, const bt_key& k2);
int					bt_cmp_obj(object o1, object o2);

/* search functions */
char*				bt_cluster_tail(char* pg);
bool				bt_leaf_find_key(xptr &xpg, bt_key* key, shft & key_idx,bool with_bt);
bool				bt_nleaf_find_key(char* pg, bt_key* key, shft &key_idx,bool with_bt);
bool				bt_leaf_find_obj(xptr &xpg, object obj, shft key_idx, shft &obj_idx);
bool				bt_find_key(xptr &xpg, bt_key* key, shft &key_idx,bool with_bt=true);
/**/
bool				bt_locate_key_bisection(char* pg, const char* ar, shft ar_size, shft ar_el_size, const bt_key &key, shft /*out*/ &key_idx,bool with_bt);
bool				bt_locate_obj_bisection(const object* ar, shft ar_size, const object &obj, shft /*out*/ &obj_idx);


/* insert functions */
xptr				bt_page_split(char* pg, const xptr &rpg, shft & pretender_idx, shft pretender_size, bool want_insertion = true);
void				bt_page_clusterize(xptr &root, char* pg, const xptr &rpg, const object &obj, shft obj_idx, bool with_bt);
shft				bt_find_split_key(char* pg, shft pretender_idx, shft pretender_size, bool & pretender_goes_left);
xptr				bt_leaf_insert(xptr &root, char* pg, shft key_idx, bool create_new_key, const bt_key &key, const object &obj, shft obj_idx,bool with_bt);
void				bt_nleaf_insert(xptr &root, char* pg, const bt_key& key, const xptr &big_ptr,bool with_bt);
void				bt_promote_key(xptr &root, const xptr &pg, const xptr &parent_pg,bool with_bt);
void				bt_leaf_do_insert_key(char* pg, shft key_idx, const bt_key& key, const object &obj);
void				bt_nleaf_do_insert_key(char* pg, shft key_idx, const bt_key& key, const xptr &big_ptr);
void				bt_do_insert_obj(char* pg, shft key_idx, const object &obj, shft obj_idx);
bool				bt_page_fit(char* pg, shft size);


/* delete functions */
void				bt_delete_obj(char* pg, shft key_idx, shft obj_idx);
void				bt_leaf_delete_key(char* pg, shft key_idx);
void bt_nleaf_delete_key(char* pg, shft key_idx);



#endif