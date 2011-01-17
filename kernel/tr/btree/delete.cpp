/*
 * File:  delete.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/vmm/vmm.h"

#include "tr/btree/btintern.h"

/* temporary buffer used for performing page delete operations */
char delete_buf[BT_PAGE_SIZE];

// How much free space should we have on the page merged from two others
#define BT_LAZY_DELETE_RESERVE 	(BT_PAGE_PAYLOAD / 4)
// How much space must be free for us not to even try to merge pages
#define BT_CRITICAL_FREESPACE	(BT_PAGE_SIZE - (BT_PAGE_PAYLOAD / 2))


void bt_get_leftmost_key(char * pg, bt_key &key) {
	while (!BT_IS_LEAF(pg) && !(BT_LMP(pg) == XNULL)) {
		xptr lmp_pg = BT_LMP(pg);
		CHECKP(lmp_pg);
		pg = (char *) XADDR(lmp_pg);
	}
	key.setnew(pg, 0);
}


bool bt_pages_mergable(xptr p1, xptr p2, int additional_space)
{
	CHECKP(p1);
	size_t s1 = BT_PFS((char *) XADDR(p1));

	CHECKP(p2);
	size_t s2 = BT_PFS((char *) XADDR(p2));

	return (s1 + s2 >= (BT_PAGE_PAYLOAD + additional_space + BT_LAZY_DELETE_RESERVE));
}


/*
 * Merges leaf pages pl and pr (pl MUST be left and pr MUST be right).
 * Works only if pr is not cluster, etc.
 */

bool bt_merge_pages(xptr pl, xptr pr)
{
// in tmp : lkey-rkey-lchnk-rchnk   :   rheap-lheap

	char * tmp_pg = delete_buf;
	char * p;
	bool is_leaf;

	CHECKP(pr);
	p = (char *) XADDR(pr);
	memcpy(tmp_pg, p, BT_PAGE_SIZE);
	vmm_delete_block(pr);

	CHECKP(pl);
	VMM_SIGNAL_MODIFICATION(pl);
	p = (char *) XADDR(pl);

	is_leaf = BT_IS_LEAF(p);

	char* rkey_place  = BT_KEY_TAB_AT(p, BT_KEY_NUM(p));
	shft  rkey_len	  = BT_KEY_TAB_AT(tmp_pg, BT_KEY_NUM(tmp_pg)) - BT_KEY_TAB(tmp_pg);

	char* ldata_place = rkey_place + rkey_len;
	shft  ldata_len	  =
		 is_leaf ?
		(BT_CHNK_TAB_AT(p, BT_KEY_NUM(p)) - BT_CHNK_TAB(p)) :
		(BT_BIGPTR_TAB_AT(p, BT_KEY_NUM(p)) - BT_BIGPTR_TAB(p));

	char* rdata_place = ldata_place + ldata_len;
	shft  rdata_len	  =
		 is_leaf ?
		(BT_CHNK_TAB_AT(tmp_pg, BT_KEY_NUM(tmp_pg)) - BT_CHNK_TAB(tmp_pg)) :
		(BT_BIGPTR_TAB_AT(tmp_pg, BT_KEY_NUM(tmp_pg)) - BT_BIGPTR_TAB(tmp_pg));

	int	r_first_key	= BT_KEY_NUM(p);
	int l_heap_len = BT_PAGE_SIZE - BT_HEAP(p);
	xptr next_page = BT_NEXT(tmp_pg);

	memcpy(rdata_place, BT_KEY_TAB_AT(tmp_pg, BT_KEY_NUM(tmp_pg)), rdata_len);
	memmove(ldata_place, BT_KEY_TAB_AT(p, BT_KEY_NUM(p)), ldata_len);
	memcpy(rkey_place, BT_KEY_TAB(tmp_pg), rkey_len);

	BT_HEAP(p) -= (shft) (BT_PAGE_SIZE - BT_HEAP(tmp_pg));
	U_ASSERT(BT_HEAP(p) < BT_PAGE_SIZE);
	memcpy(p + BT_HEAP(p), tmp_pg + BT_HEAP(tmp_pg), BT_PAGE_SIZE - BT_HEAP(tmp_pg));

	BT_KEY_NUM(p) = BT_KEY_NUM(p) + BT_KEY_NUM(tmp_pg);

	if (BT_VARIABLE_KEY_TYPE(p)) {
		for (int i = r_first_key; i < BT_KEY_NUM(p); i++) {
			BT_KEY_ITEM_AT(p, i)->k_shft  -= l_heap_len;
		}
	}

	if (is_leaf) {
		for (int i = r_first_key; i < BT_KEY_NUM(p); i++) {
			BT_CHNK_ITEM_AT(p, i)->c_shft -= l_heap_len;
		}
	}

	BT_NEXT(p) = BT_NEXT(tmp_pg);

	if (next_page != XNULL) {
		WRITEP(next_page);
		BT_PREV((char *)XADDR(next_page)) = BT_PREV(tmp_pg);
	}

	return true;
}


template<typename object>
xptr bt_try_squeeze_cluster_tmpl(xptr leaf)
{
	CHECKP(leaf);
	char * pg = (char *) XADDR(leaf);
	xptr mpg = XNULL;
	bool rotation;
	char * tmp_pg = delete_buf;
	xptr next_page;

	if (BT_IS_CLUS(pg)) {
		if (!BT_IS_CLUS_HEAD(pg) && (BT_PFS(pg) > BT_CRITICAL_FREESPACE) && bt_pages_mergable(leaf, BT_PREV(pg), 0)) {
			CHECKP(leaf);
			mpg = BT_PREV(pg);
			rotation = true;
		}
		CHECKP(leaf);
		if ((mpg == XNULL) && !BT_IS_CLUS_TAIL(pg) && (BT_PFS(pg) > BT_CRITICAL_FREESPACE) && bt_pages_mergable(leaf, BT_NEXT(pg), 0)) {
			CHECKP(leaf);
			mpg = BT_NEXT(pg);
			rotation = false;
		}
	}

	if (mpg != XNULL) {
		xptr lpg, rpg;
		char * p;
		int heap_modification_point, heap_insertion_size;
		bool drop_cluster;

		if (rotation) {
			lpg = mpg;
			rpg = leaf;
		} else {
			lpg = leaf;
			rpg = mpg;
		}

		CHECKP(rpg);
		p = (char *) XADDR(rpg);


		memcpy(tmp_pg, p, BT_PAGE_SIZE);
		vmm_delete_block(rpg);

		CHECKP(lpg);
		p = (char *) XADDR(lpg);


		U_ASSERT(
			(*(object *)(p + BT_CHNK_ITEM_SHIFT(p, 0) + (BT_CHNK_ITEM_SIZE(p, 0) - 1) * sizeof(object)))
			  <
			(*(object *)(tmp_pg + BT_CHNK_ITEM_SHIFT(tmp_pg, 0)))
		);

		drop_cluster = BT_IS_CLUS_HEAD(p) && BT_IS_CLUS_TAIL(tmp_pg);
		next_page = BT_NEXT(tmp_pg);
		heap_modification_point = BT_CHNK_ITEM_SHIFT(tmp_pg, 0);
		heap_insertion_size = BT_CHNK_ITEM_SIZE(p, 0) * sizeof(object);

		memmove(
			tmp_pg + BT_HEAP(tmp_pg) - heap_insertion_size,
			tmp_pg + BT_HEAP(tmp_pg),
			(shft) (heap_modification_point - BT_HEAP(tmp_pg)));

		BT_HEAP(tmp_pg) -= heap_insertion_size;

		memcpy(
			tmp_pg + heap_modification_point - heap_insertion_size,
			p + BT_CHNK_ITEM_SHIFT(p, 0),
			heap_insertion_size);

		BT_CHNK_ITEM_AT(tmp_pg, 0)->c_size += BT_CHNK_ITEM_SIZE(p, 0);

		for (int i = 0; i < BT_KEY_NUM(tmp_pg); i++) {
			if (BT_VARIABLE_KEY_TYPE(tmp_pg)) {
				if (BT_KEY_ITEM_AT(tmp_pg, i)->k_shft < heap_modification_point) {
					BT_KEY_ITEM_AT(tmp_pg, i)->k_shft -= heap_insertion_size;
				}
			}
			if (BT_CHNK_ITEM_AT(tmp_pg, i)->c_shft <= heap_modification_point) {
				BT_CHNK_ITEM_AT(tmp_pg, i)->c_shft -= heap_insertion_size;
			}
		}

		BT_PREV(tmp_pg) = BT_PREV(p);

		if (drop_cluster) {
			BT_IS_CLUS(tmp_pg) = false;
			BT_IS_CLUS_HEAD(tmp_pg) = false;
			BT_IS_CLUS_TAIL(tmp_pg) = false;
		} else {
			BT_IS_CLUS_HEAD(tmp_pg) = BT_IS_CLUS_HEAD(p);
		}

		VMM_SIGNAL_MODIFICATION(lpg);
		memcpy(p + sizeof(vmm_sm_blk_hdr), tmp_pg + sizeof(vmm_sm_blk_hdr), BT_PAGE_SIZE - sizeof(vmm_sm_blk_hdr));


		if (next_page != XNULL) {
			CHECKP(next_page);
			VMM_SIGNAL_MODIFICATION(next_page);
			BT_PREV((char *)XADDR(next_page)) = lpg;
		}

		return lpg;
	} else {
		return leaf;
        /// TODO: May be balancing is needed here ...
	}
}


/*
   =======================================================================
   =======================================================================
   =======================================================================
*/


/*
	to write.
*/

bool bt_try_merge_pages(xptr pr)
{
	CHECKP(pr);
	if (BT_IS_CLUS((char*) XADDR(pr))) { return false; }
	U_ASSERT(BT_LMP((char *) XADDR(pr)) == XNULL);
	if (!bt_pages_mergable(BT_PREV((char*) XADDR(pr)), pr, 0)) { return false; }
	CHECKP(pr);
	return bt_merge_pages(BT_PREV((char*) XADDR(pr)), pr);
}





template<typename object>
object bt_get_cluster_obj_from_left_tmpl(char * pg) {
	object a = * (object *) (pg + BT_CHNK_ITEM_SHIFT(pg, 0) + (BT_CHNK_ITEM_SIZE(pg, 0) - 1) * sizeof(object));
	if (BT_CHNK_ITEM_SIZE(pg, 0) == 1) {
		bt_leaf_delete_key_tmpl<object>(pg, 0);
	} else {
		bt_delete_obj_tmpl<object>(pg, 0, BT_CHNK_ITEM_SIZE(pg, 0) - 1);
	}
	return a;
}


void bt_delete_page(xptr pg)
{
	CHECKP(pg);
	U_ASSERT(BT_KEY_NUM((char*) XADDR(pg)) == 0);
	xptr next = BT_NEXT((char*) XADDR(pg));
	xptr prev = BT_PREV((char*) XADDR(pg));
	vmm_delete_block(pg);
	if (next != XNULL) {
		CHECKP(next)
		VMM_SIGNAL_MODIFICATION(next);
		BT_PREV((char*) XADDR(next)) = prev;
	}
	if (prev != XNULL) {
		CHECKP(prev)
		VMM_SIGNAL_MODIFICATION(prev);
		BT_NEXT((char*) XADDR(prev)) = next;
	}
}

struct break_data
{
	bool broken;
	int level;
	xptr pg;
	bt_key key;
};

template<typename object>
bool bt_recoursive_delete_tmpl(const bt_key& key, bt_path &path_fore, shft obj_idx, break_data &bd)
{
	bt_path_item pi = path_fore.front();
	xptr ptr = pi.pg; CHECKP(ptr);
	int key_idx = pi.idx;
	char *pg = (char*) XADDR(ptr);

	if (BT_IS_LEAF(pg)) {
		if (BT_CHNK_ITEM_SIZE(pg, key_idx) > 1) {
			bt_delete_obj_tmpl<object>(pg, key_idx, obj_idx);
		} else {
			if (BT_IS_CLUS_TAIL(pg) && (key_idx == 0)) {
				xptr ptr_prev = BT_PREV(pg);
				CHECKP(ptr_prev);
				object a = bt_get_cluster_obj_from_left_tmpl<object>((char *) XADDR(ptr_prev));
				CHECKP(ptr);
				* (object *) (pg + BT_CHNK_ITEM_SHIFT(pg, 0)) = a;
			} else {
				bt_leaf_delete_key_tmpl<object>(pg, key_idx);
			}
		}
		if (BT_IS_CLUS(pg)) {
			path_fore.front().pg = bt_try_squeeze_cluster_tmpl<object>(ptr);
			CHECKP(path_fore.front().pg);
		}
		return true;
	}

	bool has_key;
	path_fore.pop_front();

	if (!bt_recoursive_delete_tmpl<object>(key, path_fore, obj_idx, bd)) {
		return false;
	}

	pi = path_fore.front();

	CHECKP(ptr);

	if (key_idx >= 0) {
		bt_key akey(pg, key_idx);
		has_key = akey == key;
	} else {
		has_key = false;
	}

	if (bd.broken) {
		if (has_key) {
			bt_nleaf_delete_key(pg, key_idx);
		}
		bd.level++;
		return true;
	}

	int key_num = BT_KEY_NUM(pg);

	CHECKP(pi.pg);
	pg = (char*) XADDR(pi.pg);

	bool merged, mergable = BT_PFS(pg) > BT_CRITICAL_FREESPACE;
	xptr right_node = (key_idx == key_num - 1) ? XNULL : BT_NEXT(pg);
	xptr left_node = (key_idx == -1) ? XNULL : BT_PREV(pg);

	pg = (char*) XADDR(ptr);

	if (mergable && (left_node != XNULL) && bt_try_merge_pages(pi.pg)) {
		CHECKP(ptr);
		bt_nleaf_delete_key(pg, key_idx);
		return true;
	}

	if (merged = (mergable && (right_node != XNULL) && bt_try_merge_pages(right_node))) {
		CHECKP(ptr);
		bt_nleaf_delete_key(pg, key_idx + 1);
	}

	if (has_key) {
		CHECKP(pi.pg);

		if (BT_KEY_NUM((char*) XADDR(pi.pg)) == 0) {
			bt_delete_page(pi.pg);
			CHECKP(ptr);
			bt_nleaf_delete_key(pg, key_idx);

//			U_ASSERT(false);
//			throw USER_EXCEPTION2(SE1008, "No keys left on page, it must have been merged with some other");
		} else {
			bt_key skey((char*) XADDR(pi.pg), 0);
			if (skey != key) {
				CHECKP(ptr);
				if (!bt_nleaf_subst_key(pg, key_idx, skey)) {
					bt_nleaf_delete_key(pg, key_idx);
					bd.key = skey;
					bd.pg = pi.pg;
					bd.broken = true;
					bd.level = 1;
				}
				return true;
			} else {
				return false;
			}
		}
	}
/*
	if (!merged && mergable) {
	}
*/
	return merged;
}

template<typename object>
bool bt_internal_delete_tmpl(xptr &root, const bt_key& key, shft obj_idx, bt_path &path)
{
	break_data bd;

	bd.broken = false;
	bt_recoursive_delete_tmpl<object>(key, path, obj_idx, bd);

	if (bd.broken) {
		bt_path split_path;
		shft new_key_idx;

		if (bt_find_key(root, &bd.key, new_key_idx, &path, true)) {
			U_ASSERT(false);
		}

		while (bd.level > 0) {
			split_path.push_back(path.front());
			path.pop_front();
			bd.level--;
		}

		bt_nleaf_insert_tmpl<object>(root, bd.key, bd.pg, split_path);
	}

	CHECKP(root);
	char * pg = (char*) XADDR(root);
	if ((BT_KEY_NUM(pg) == 0) && (BT_LMP(pg) != XNULL)) {
		xptr new_root = BT_LMP(pg);
		vmm_delete_block(root);
		root = new_root;
	}

	return true;
}
















/*
	bt_delete_obj
	bt_leaf_delete_key
	bt_nleaf_delete_key

	This function provide actual (phisical) deletion of objects. They don't check anything.
*/

template<typename object>
void bt_delete_obj_tmpl(char* pg, shft key_idx, shft obj_idx)
{
// This function works properly in assumption that CHUNK_ITEM_SIZE >= 2

	if (*((shft*)BT_CHNK_TAB_AT(pg, key_idx) + 1) == 1)
    {
        bt_leaf_delete_key_tmpl<object>(pg, key_idx);
        return;
    }

	shft	old_heap_shift = BT_HEAP(pg);
	shft	obj_shift = BT_CHNK_ITEM_SHIFT(pg, key_idx) + obj_idx * sizeof(object);

	VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));
	memmove(pg + old_heap_shift + sizeof(object), pg + old_heap_shift, (shft) (obj_shift - old_heap_shift));

	// update all heap "pointers"

	if (BT_KEY_SIZE(pg) == 0) {										// in case of variable key length, update key pointers
		for (int i = 0; i < BT_KEY_NUM(pg); i++) {
			if (BT_KEY_ITEM_AT(pg, i)->k_shft < obj_shift)
				{ BT_KEY_ITEM_AT(pg, i)->k_shft += sizeof(object); }
		}
	}

	for (int i = 0; i < BT_KEY_NUM(pg); i++) {						// update chunk "pointers"
		if (BT_CHNK_ITEM_AT(pg, i)->c_shft <= obj_shift)
			{ BT_CHNK_ITEM_AT(pg, i)->c_shft += sizeof(object); }
	}

	BT_HEAP(pg) += sizeof(object);
	BT_CHNK_ITEM_AT(pg, key_idx)->c_size -= 1;

}

template<typename object>
void bt_leaf_delete_key_tmpl(char* pg, shft key_idx)
{
// This function works properly in assumption that CHUNK_ITEM_SIZE = 1 !!!
	U_ASSERT(BT_CHNK_ITEM_SIZE(pg, key_idx) == 1);

	shft	heap_shift = BT_HEAP(pg);
	shft	obj_shift = BT_CHNK_ITEM_SHIFT(pg, key_idx);
	char *	key_pos = BT_KEY_TAB_AT(pg, key_idx);
	bool	var_key_size = BT_KEY_SIZE(pg) == 0;
	shft	key_size = (var_key_size ? 2 * sizeof(shft) : BT_KEY_SIZE(pg));
	shft	actkey_shift = (var_key_size ? BT_KEY_ITEM_AT(pg, key_idx)->k_shft : 0);
	shft	actkey_size = (var_key_size ? BT_KEY_ITEM_AT(pg, key_idx)->k_size : 0);
	char *	chnk_pos = BT_CHNK_TAB_AT(pg, key_idx);
	shft	chnk_size = sizeof(btree_chnk_hdr);
	char *	last = BT_CHNK_TAB_AT(pg, BT_KEY_NUM(pg));

	VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));

	shft	len = (chnk_pos - key_pos) - key_size;
	memmove(key_pos, key_pos + key_size, len);
	memmove(key_pos + len, chnk_pos + chnk_size, (shft) ((last - chnk_pos) - chnk_size));
	memmove(pg + heap_shift + sizeof(object), pg + heap_shift, (shft) (obj_shift - heap_shift));
	heap_shift += sizeof(object);

	BT_KEY_NUM(pg) -= 1;

	if (var_key_size) {
		memmove(pg + heap_shift + actkey_size, pg + heap_shift, (shft) (actkey_shift - heap_shift));
		heap_shift += actkey_size;

		for (int i = 0; i < BT_KEY_NUM(pg); i++) {						// update key "pointers"
			btree_key_hdr * v = BT_KEY_ITEM_AT(pg, i);
			int s = v->k_shft; // save unmodified shift
			if (s < obj_shift) { v->k_shft += sizeof(object); }
			if (s < actkey_shift) { v->k_shft += actkey_size; }
		}
	}

	for (int i = 0; i < BT_KEY_NUM(pg); i++) {						// update chunk "pointers"
		btree_chnk_hdr * v = BT_CHNK_ITEM_AT(pg, i);
		int s = v->c_shft; // save unmodified shift
		if (s < obj_shift) { v->c_shft += sizeof(object); }
		if (s < actkey_shift) { v->c_shft += actkey_size; }
	}

	BT_HEAP(pg) = heap_shift;

}


void bt_nleaf_delete_key(char* pg, shft key_idx)
{

	shft	heap_shift = BT_HEAP(pg);
	char *	key_pos = BT_KEY_TAB_AT(pg, key_idx);
	bool	var_key_size = BT_KEY_SIZE(pg) == 0;
	shft	key_size = (var_key_size ? 2 * sizeof(shft) : BT_KEY_SIZE(pg));
	shft	actkey_shift = (var_key_size ? BT_KEY_ITEM_AT(pg, key_idx)->k_shft : 0);
	shft	actkey_size = (var_key_size ? BT_KEY_ITEM_AT(pg, key_idx)->k_size : 0);
	char *	ptr_pos = BT_BIGPTR_TAB_AT(pg, key_idx);
	shft	ptr_size = sizeof(xptr);
	char *	last = BT_BIGPTR_TAB_AT(pg, BT_KEY_NUM(pg));

	VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));

	shft	len = (ptr_pos - key_pos) - key_size;
	memmove(key_pos, key_pos + key_size, len);
	memmove(key_pos + len, ptr_pos + ptr_size, (shft) ((last - ptr_pos) - ptr_size));

	BT_KEY_NUM(pg) -= 1;

	if (var_key_size) {
		memmove(pg + heap_shift + actkey_size, pg + heap_shift, (shft) (actkey_shift - heap_shift));
		heap_shift += actkey_size;

		for (int i = 0; i < BT_KEY_NUM(pg); i++) {						// update key "pointers"
			btree_key_hdr * v = BT_KEY_ITEM_AT(pg, i);
			if (v->k_shft < actkey_shift) { v->k_shft += actkey_size; }
		}
	}

	BT_HEAP(pg) = heap_shift;

}


bool bt_nleaf_subst_key(char* pg, shft key_idx, bt_key key)
{

	VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));

	if (BT_VARIABLE_KEY_TYPE(pg)) {
		shft heap_shift = BT_HEAP(pg);
		shft actkey_shift = BT_KEY_ITEM_AT(pg, key_idx)->k_shft;
		int actkey_size_diff = BT_KEY_ITEM_AT(pg, key_idx)->k_size - key.get_size();

		memmove(pg + heap_shift + actkey_size_diff, pg + heap_shift, (shft) (actkey_shift - heap_shift));
		memcpy(pg + actkey_shift + actkey_size_diff, key.data(), key.get_size());

		heap_shift += actkey_size_diff;

		for (int i = 0; i < BT_KEY_NUM(pg); i++) {						// update key "pointers"
			btree_key_hdr * v = BT_KEY_ITEM_AT(pg, i);
			if (v->k_shft <= actkey_shift) { v->k_shft += actkey_size_diff; }
		}
		BT_HEAP(pg) = heap_shift;

		BT_KEY_ITEM_AT(pg, key_idx)->k_size = key.get_size();
	} else {
		memcpy(BT_KEY_TAB_AT(pg, key_idx), key.data(), BT_KEY_TAB_SIZE(pg));
	}

	return true;
}


#define MAKE_IMPLS(t) \
	template bool bt_internal_delete_tmpl<t>(xptr &root, const bt_key& key, shft obj_idx, bt_path &path); \
    template void bt_leaf_delete_key_tmpl<t>(char* pg, shft key_idx);
#include "tr/btree/make_impl.h"
