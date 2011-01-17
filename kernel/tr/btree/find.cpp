/*
 * File:  find.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/btree/btintern.h"

/* All of the following search functions return boolean value indicating success or failure
   to find specified item. The updateable key_idx/obj_idx parameter in all functions is set
   to the index of the hit item in case of success or to the index of the nearest bigger item
   in case of failure. As the utmost cases this parameters may take values BT_LEFTMOST, BT_RIGHTMOST
   meaning that the searched item is less or higher than all items in appropriate search
   domain of concrete function. The updateable page parameter (where appropriate) can be internaly
   modified to focus to the hit page where the corresponding item was found or where the search process
   completed. The ultimate idx parameters have validness relatively to ultimate page parameter value.
   Note also that BT_LEFTMOST value equals 0 and thus in some places is directly treated as index.
*/

/*
   Find key inside the given leaf page/cluster.
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Clusters are the blocks of consequitive leaf pages, which have head page, tail page and zero or more
   intermediate pages. Non-tail pages of clusters by definition keep single key, which is called 'cluster
   key' and objects sticking to that key. At that if the cluster has a head page and a number of
   intermediate pages, all of them store objects sticking to the cluster key. Only tail page may store
   keys other than cluster key, which follow the cluster key
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Note that if the page is inside of cluster, the whole cluster is searched. Search inside
   the cluster can be initialized only from head page of a cluster, thus the algorithm
   moves only in left-to-right direction over the pages of the cluster. If the key is found
   in page different from initial page, the 'pg' pointer is set to the target page
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  */
bool bt_leaf_find_key(xptr & xpg, bt_key* key, shft & key_idx,bool with_bt) {
CHECKP(xpg);
	char*	pg=(char*)XADDR(xpg);
	int		rc;
	shft	el_size;
	bool	var_key_type;
	//bt_page_consistency(pg, key);
	/* calculate size of key table element */
	if (BT_VARIABLE_KEY_TYPE(key->get_type())) {
		var_key_type = true;
		el_size = 2*sizeof(shft);
	} else {
		var_key_type = false;
		el_size = BT_KEY_SIZE(pg);
	}
#ifdef PERMIT_CLUSTERS
	/* check if this is the cluster */
	if (BT_IS_CLUS(pg)) {
		while (!BT_IS_CLUS_TAIL(pg)) {
			/* the cluster key table consists of zero or one key, by definition */
			if (BT_KEY_NUM(pg)) {
				rc = bt_cmp_key(pg, BT_KEY_TAB(pg), *key);
				if (!rc) {
					key_idx = 0;
					return true;
				} else if (rc < 0) {
					/* the key is above cluster key, go to cluster tail; uploads tail page into memory */
					pg = bt_cluster_tail(pg);
					xpg = ADDR2XPTR(pg);
					break;
				} else {
					/* the key is below cluster key */
					key_idx = BT_LEFTMOST;
					return false;
				}
			} else {
				/* if there is no key in current non-tail cluster page - go to the next page */
				xpg = BT_NEXT(pg);
CHECKP(xpg);
				pg = (char*)XADDR(xpg);
			}
		}
		/* pg is cluster tail page, in general this is not initial page */
		bool rcbool = bt_locate_key_bisection(pg, BT_KEY_TAB(pg), BT_KEY_NUM(pg), el_size, *key, key_idx,with_bt);
		if (key_idx == BT_LEFTMOST)
			/* search in the cluster tail page of the cluster can not result in BT_LEFTMOST */
            throw USER_EXCEPTION2(SE1008, "Search in cluster tail page resulted in BT_LEFTMOST key_idx");
		return rcbool;
	} else
		return bt_locate_key_bisection(pg, BT_KEY_TAB(pg), BT_KEY_NUM(pg), el_size, *key, key_idx,with_bt);
#else
	return bt_locate_key_bisection(pg, BT_KEY_TAB(pg), BT_KEY_NUM(pg), el_size, *key, key_idx,with_bt);
#endif

}

/* Given pointer to cluster page returns cluster tail page; the page is uploaded in memory */
char* bt_cluster_tail(char* pg) {
	char*	next_pg=pg;
	xptr	next_pg_xptr=ADDR2XPTR(pg);
	if (!BT_IS_CLUS(pg))
        throw USER_EXCEPTION2(SE1008, "Non-cluster page given");
	while (!BT_IS_CLUS_TAIL(next_pg)) {
		next_pg_xptr = BT_NEXT(next_pg);
CHECKP(next_pg_xptr);
		next_pg = (char*)XADDR(next_pg_xptr);
		if (!BT_IS_CLUS(next_pg))
            throw USER_EXCEPTION2(SE1008, "Non-cluster page inside cluster");
	}
	return next_pg;
}

/* Find given object among objects of given key (key_idx), starting from given page, following
   adjacent pages in cluster case, while the 'pg' argument dynamically focuses to next searched page.
   In case of cluster the initial 'pg' argument must address head cluster page.
 */
template<typename object>
bool bt_leaf_find_obj_tmpl(xptr &xpg, object obj, shft key_idx, shft &obj_idx) {
CHECKP(xpg);
	char*	pg = (char*)XADDR(xpg);
	bool	rc;
	btree_chnk_hdr c;

	if (key_idx >= BT_KEY_NUM(pg)) throw USER_EXCEPTION2(SE1008, "Bad key index");

	c = *BT_CHNK_ITEM_AT(pg, key_idx);
	rc = bt_locate_obj_bisection_tmpl<object>((object*)(pg + c.c_shft), c.c_size, obj, obj_idx);

//#ifdef PERMIT_CLUSTERS
	while (!rc && BT_IS_CLUS(pg) && !BT_IS_CLUS_TAIL(pg) && (obj_idx == BT_RIGHTMOST)) {
		U_ASSERT(key_idx == 0);
		/* move to next cluster page; in this case key_idx remains the same, namely 1 */
		xpg = BT_NEXT(pg);
		CHECKP(xpg);
		pg = (char*)XADDR(xpg);
		c = *BT_CHNK_ITEM_AT(pg, 0);
		rc = bt_locate_obj_bisection_tmpl<object>((object*)(pg + c.c_shft), c.c_size, obj, obj_idx);
	}
//#endif

	return rc;
}

/* Search key in given non-leaf page*/
bool bt_nleaf_find_key(char* pg, bt_key* key, shft &key_idx,bool with_bt) {
	/* calculate size of key table element */
	//bt_page_consistency(pg, key);
	shft el_size;
	if (BT_VARIABLE_KEY_TYPE(pg))
		el_size = 2*sizeof(shft);
	else
		el_size = BT_KEY_SIZE(pg);
	return bt_locate_key_bisection(pg, BT_KEY_TAB(pg), BT_KEY_NUM(pg), el_size, *key, key_idx,with_bt);
}

/* Search key in given non-leaf page, recursively descending through the tree to the leaf page level.
   Note that the page pointer is changed at every recursive level to point to the currently processed
   page. When reached leaf level, whether the key was found or not, the 'pg' is set to the page where
   the searched key resides or must be allocated.
 */

bool bt_find_key(xptr & xpg, bt_key* key, shft &key_idx, bt_path *path, bool with_bt) {
	CHECKP(xpg);
	char*	pg=(char*)XADDR(xpg);
	bt_path_item pi(xpg, 0);

	/* pg - currently processed page */
	if (!BT_IS_LEAF(pg)) {
		if (bt_nleaf_find_key(pg, key, key_idx, with_bt)) {
			xpg = *(xptr*)BT_BIGPTR_TAB_AT(pg, key_idx);
			pi.idx = key_idx;
		} else if (key_idx == BT_LEFTMOST) {
			if (BT_LMP(pg) == XNULL)
                   throw USER_EXCEPTION2(SE1008, "Cannot follow XNULL left-most pointer");
			xpg = BT_LMP(pg);
			pi.idx = -1;
		} else {
			if (key_idx == BT_RIGHTMOST) {
				if (BT_KEY_NUM(pg) == 0) return false;
				key_idx = BT_KEY_NUM(pg);
			}
			xpg = *(xptr*)BT_BIGPTR_TAB_AT(pg, key_idx - 1);
			pi.idx = key_idx - 1;
		}

		if (path != NULL) path->push_back(pi);

		return bt_find_key(xpg, key, key_idx, path, with_bt);
	} else {
		return bt_leaf_find_key(xpg, key, key_idx, with_bt);
	}
}


#define MAKE_IMPLS(t) template bool bt_leaf_find_obj_tmpl<t>(xptr &xpg, t obj, shft key_idx, shft &obj_idx);
#include "tr/btree/make_impl.h"
