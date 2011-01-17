/*
 * File:  btree.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/vmm/vmm.h"

#include "tr/btree/btree.h"
#include "tr/btree/btintern.h"
#include "tr/btree/btpage.h"
#include "tr/btree/btstruct.h"
#include "tr/btree/buff.h"

/* variables for debug */
//shft	BTREE_HEIGHT=1;

xptr bt_create(xmlscm_type t)
{
    xptr    root;
    char*   pg;

    vmm_alloc_data_block(&root);
	VMM_SIGNAL_MODIFICATION(root);
    pg = (char*)XADDR(root);
    bt_page_markup(pg, t);
    return root;
}

void bt_drop(const xptr root)
{
    char*   cur_pg;
    xptr    cur_xpg = root;

    do {
        CHECKP(cur_xpg);
        cur_pg = (char*)XADDR(cur_xpg);
        xptr next_level_lmp = BT_LMP(cur_pg);
        if ((next_level_lmp == XNULL) && !BT_IS_LEAF(cur_pg))
            throw USER_EXCEPTION2(SE1008, "Encountered XNULL LMP field when trying to get left-most path in btree");

        /* walk through pages at current layer */
        do {
            xptr tmp = cur_xpg;
            CHECKP(cur_xpg);
            cur_xpg = BT_NEXT(cur_pg);
            cur_pg = (char*)XADDR(cur_xpg);
            vmm_delete_block(tmp);
        } while (cur_xpg != XNULL);

        cur_xpg = next_level_lmp;
    } while (cur_xpg != XNULL);
}

int bt_walk_nodes(const xptr root)
{
    xptr cur_xpg = root;
    int c = 0;

    do {
        CHECKP(cur_xpg);
        xptr next_level_lmp = BT_LMP(xaddr(cur_xpg));

        do {
			CHECKP(cur_xpg);
            cur_xpg = BT_NEXT(xaddr(cur_xpg));
            ++c;
        } while (cur_xpg != XNULL);

        cur_xpg = next_level_lmp;
    } while (cur_xpg != XNULL);

    return c;
}


template<typename object>
bt_cursor_tmpl<object> bt_find_tmpl(const xptr root, const bt_key &key)
{
    bool rc;
    shft key_idx;
    xptr res_blk = root;

    CHECKP(res_blk);
    rc = bt_find_key(res_blk, (bt_key*)&key, key_idx);
    if (!rc) /* no matching key */
        return bt_cursor_tmpl<object>();
    else
        return bt_cursor_tmpl<object>((char*)XADDR(res_blk), key_idx);
}

/// !!! potential error here
template<typename object>
bt_cursor_tmpl<object> bt_find_ge_tmpl(const xptr root, const bt_key &key)
{
    bool rc;
    shft key_idx;
    xptr next;
    xptr res_blk = root;

    rc = bt_find_key(res_blk, (bt_key*)&key, key_idx);

	CHECKP(res_blk);
    char* pg = (char*)XADDR(res_blk);

    if (!rc && key_idx == BT_RIGHTMOST)
    {
#ifdef PERMIT_CLUSTERS
        if (BT_IS_CLUS(pg))
        {
            pg = bt_cluster_tail(pg);
            if (BT_KEY_NUM(pg) > 1) return bt_cursor_tmpl<object>(pg, 1);
        }
#endif
        next = BT_NEXT(pg);
        if (next != XNULL)
        {
            CHECKP(next);
            return bt_cursor_tmpl<object>((char*)XADDR(next), 0);
        }
        else return bt_cursor_tmpl<object>();
    }
    else return bt_cursor_tmpl<object>((char*)XADDR(res_blk), key_idx);
}

template<typename object>
bt_cursor_tmpl<object> bt_find_gt_tmpl(const xptr root, const bt_key &key)
{
    bool rc;
    shft key_idx;
    xptr next;
    xptr res_blk = root;
	bt_key old_val=key;

    rc = bt_find_key(res_blk, (bt_key*)&key, key_idx);
    CHECKP(res_blk);
    char* pg = (char*)XADDR(res_blk);

    if (!rc && key_idx == BT_RIGHTMOST)
    {
#ifdef PERMIT_CLUSTERS
        if (BT_IS_CLUS(pg))
        {
            pg = bt_cluster_tail(pg);
            if (BT_KEY_NUM(pg) > 1) return bt_cursor_tmpl<object>(pg, 1);
        }
#endif
        next = BT_NEXT(pg);
        if (next != XNULL)
        {
            CHECKP(next);
            return bt_cursor_tmpl<object>((char*)XADDR(next), 0);
		}
        else return bt_cursor_tmpl<object>();
    }
    else
    {
        bt_cursor_tmpl<object> c((char*)XADDR(res_blk), key_idx);
		if (!bt_cmp_key(c.get_key(),old_val))
		{
			c.bt_next_key();
			return c;
		}
		else
		{
			return c;
		}
    }

    return bt_cursor_tmpl<object>();
}

template<typename object>
bt_cursor_tmpl<object> bt_lm_tmpl(const xptr root)
{
    if (root == XNULL) return bt_cursor_tmpl<object>();

    CHECKP(root);
    char* pg = (char*)XADDR(root);

#ifdef PERMIT_CLUSTERS
    /// !!! FIX ME UP
#endif

    if (!BT_IS_LEAF(pg))
    {
        return bt_lm_tmpl<object>(BT_LMP(pg));
	}
    else
    {
        return bt_cursor_tmpl<object>(pg, 0);
    }
}


template<typename object>
void bt_insert_tmpl(xptr &root, const bt_key &key, const object &obj,bool with_bt)
{
    bool  rc;
    shft  key_idx = 0;
    shft  obj_idx = 0;
    xptr  insert_xpg = root; /* xptr passed to search functions, that can change, pointing to insert new data */
    char* insert_pg;
	bt_path split_path;

    /* check key length doesn't exceed limit */
    if (key.get_size() >= BT_PAGE_PAYLOAD / 2)
        throw USER_EXCEPTION2(SE1008, "The size of the index key exceeds max key limit");

    rc = bt_find_key(insert_xpg, (bt_key*)&key, key_idx, &split_path, with_bt);
    /* page could change */
    insert_pg = (char*)XADDR(insert_xpg);

	if (rc) {
		bt_leaf_find_obj_tmpl<object>(insert_xpg, obj, key_idx, obj_idx);
	    CHECKP(insert_xpg);
	    insert_pg = (char*)XADDR(insert_xpg);

		if (obj_idx == BT_RIGHTMOST)
		   obj_idx = *((shft*)BT_CHNK_TAB_AT(insert_pg, key_idx) + 1);
	} else {
	    if (key_idx == BT_RIGHTMOST)
			key_idx = BT_KEY_NUM(insert_pg);
	}

	bt_internal_insert_tmpl<object>(root, insert_pg, key_idx, !rc, key, obj, obj_idx, split_path, with_bt, XNULL);

//	bt_check_btree(root);
}

template<typename object>
void bt_modify_tmpl(xptr &root, const bt_key &old_key, const bt_key &new_key, const object &obj)
{
    bt_delete_tmpl<object>(root, old_key, obj);
    bt_insert_tmpl<object>(root, new_key, obj);
}

/* light delete functions - delete only data from pages, not droping the pages, i.e no
   implementation of page merge/drop
 */
/* delete key/obj pair */
template<typename object>
void bt_delete_tmpl(xptr &root, const bt_key& key, const object &obj)
{
    shft    key_idx;
    shft    obj_idx;
    xptr    delete_xpg = root; /* xptr passed to search functions, that can change, pointing to page where data resides */
    char*   delete_pg = (char*)XADDR(delete_xpg);
	bt_path merge_path;
	bt_path_item pi;

    CHECKP(delete_xpg);

    if (bt_find_key(delete_xpg, (bt_key*)&key, key_idx, &merge_path))
    {
		if (!bt_leaf_find_obj_tmpl<object>(delete_xpg, obj, key_idx, obj_idx)) {
			U_ASSERT(false);
			throw USER_EXCEPTION2(SE1008, "Cannot delete object which is not in the btree");
		}

        CHECKP(delete_xpg);
        delete_pg = (char*)XADDR(delete_xpg);
		pi.pg = delete_xpg;
		pi.idx = key_idx;
		merge_path.push_back(pi);

		bt_internal_delete_tmpl<object>(root, key, obj_idx, merge_path);
    }
	else
	{
		U_ASSERT(false);
		throw USER_EXCEPTION2(SE1008, "Cannot delete object which is not in the btree");
	}

//	bt_check_btree(root);
}



/* delete key and all associated objects */
template<typename object>
void bt_delete_tmpl(xptr &root, const bt_key &key)
{
    bool    rc;
    shft    key_idx;
    xptr    delete_xpg = root; /* xptr passed to search functions, that can change, pointing to page where data resides */
    char*   delete_pg = (char*)XADDR(delete_xpg);

    CHECKP(delete_xpg);

    rc = bt_find_key(delete_xpg, (bt_key*)&key, key_idx);
    if (rc)
    { /* page could change */
        CHECKP(delete_xpg);
        delete_pg = (char*)XADDR(delete_xpg);
        bt_leaf_delete_key_tmpl<object>(delete_pg, key_idx);
    }
}
/* drop empty page*/
/*
void           bt_drop_page(const btree_blk_hdr * pg)
{
	xptr page=ADDR2XPTR(pg);
	btree_blk_hdr * page_hdr= (btree_blk_hdr*)XADDR(page);
	xptr left=page_hdr->prev;
	xptr right=page_hdr->next;
	//1. fixing block chain
	if (left!=XNULL)
	{
		CHECKP(left);
		VMM_SIGNAL_MODIFICATION(left);
		((btree_blk_hdr*)XADDR(left))->next=right;


	}
	if (right!=XNULL)
	{
		CHECKP(right);
		VMM_SIGNAL_MODIFICATION(right);
		((btree_blk_hdr*)XADDR(right))->prev=left;


	}
	//remove key from upper layers
	xptr parent=page_hdr->parent;
	if (parent==XNULL)
		return;
	vmm_delete_block(page);
	CHECKP(parent);
	btree_blk_hdr * par_hdr= (btree_blk_hdr*)XADDR(parent);
	// fix lmp
	if (par_hdr->lmp==page)
		par_hdr->lmp=right;
	//find key by xptr
	int key_idx=-1;
	for (int i=0;i<BT_KEY_NUM(par_hdr);i++)
	{
		if (page==( *(xptr*)BT_BIGPTR_TAB_AT((char*)par_hdr, i)))
		{
			key_idx=i;
			break;
		}
	}
	if (key_idx<0)
		throw USER_EXCEPTION2(SE1008, "System error in indexes");

	bt_nleaf_delete_key((char*)par_hdr,key_idx);
	//VMM_SIGNAL_MODIFICATION(parent);
	//recursive walthrough
	if (!BT_KEY_NUM(par_hdr))
	{
		if (par_hdr->lmp!=XNULL)
		{
			//the case whe only one pointer left in non-leaf page
			xptr grandpa=par_hdr->parent;//save parent
			xptr left_unc=par_hdr->prev;//save left
			xptr right_unc=par_hdr->next;//save right
			xptr lmp=par_hdr->lmp;
			CHECKP(lmp);
			char*   dst = bt_tune_buffering(true, 4);
			memcpy(dst,(char*)XADDR(lmp)+sizeof(vmm_sm_blk_hdr),BT_PAGE_SIZE-sizeof(vmm_sm_blk_hdr));
			CHECKP(parent);
			VMM_SIGNAL_MODIFICATION(parent);
			memcpy((char*)XADDR(parent)+sizeof(vmm_sm_blk_hdr),dst,BT_PAGE_SIZE-sizeof(vmm_sm_blk_hdr));
			par_hdr->parent=grandpa;//restore
			par_hdr->prev=left_unc;
			par_hdr->next=right_unc;

			vmm_delete_block(lmp);

		}
		else
			bt_drop_page(par_hdr);
	}
}
*/


#define MAKE_IMPLS(t) \
	template bt_cursor_tmpl<t> bt_find_tmpl(const xptr root, const bt_key &key); \
	template bt_cursor_tmpl<t> bt_find_ge_tmpl(const xptr root, const bt_key &key); \
    template bt_cursor_tmpl<t> bt_find_gt_tmpl<t>(const xptr root, const bt_key &key); \
	template bt_cursor_tmpl<t> bt_lm_tmpl<t>(const xptr root); \
	template void bt_insert_tmpl<t>(xptr &root, const bt_key &key, const t &obj,bool with_bt); \
	template void bt_delete_tmpl<t>(xptr &root, const bt_key& key, const t &obj); \
	template void bt_delete_tmpl<t>(xptr &root, const bt_key& key);
#include "tr/btree/make_impl.h"
