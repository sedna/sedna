/*
 * File:  btree.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/idx/btree/btree.h"
#include "tr/idx/btree/btintern.h"
#include "tr/idx/btree/btpage.h"
#include "tr/idx/btree/btstruct.h"
#include "tr/idx/btree/buff.h"
#include "tr/vmm/vmm.h"

/* variables for debug */
//shft	BTREE_HEIGHT=1;

xptr bt_create(xmlscm_type t)
{
    xptr    root;
    char*   pg;

    vmm_alloc_data_block(&root);
    pg = (char*)XADDR(root);
    bt_page_markup(pg, t);
    return root;
}

void bt_drop(const xptr &root)
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

bt_cursor bt_find(const xptr &root, const bt_key &key)
{
    bool rc;
    shft key_idx;
    xptr res_blk = root;

    CHECKP(res_blk);
    rc = bt_find_key(res_blk, (bt_key*)&key, key_idx);
    if (!rc) /* no matching key */
        return bt_cursor();
    else
        return bt_cursor((char*)XADDR(res_blk), key_idx);
}

/// !!! potential error here
bt_cursor bt_find_ge(const xptr &root, const bt_key &key)
{
    bool rc;
    shft key_idx;
    xptr next;
    xptr res_blk = root;

    CHECKP(res_blk);
    char* pg = (char*)XADDR(res_blk);

    rc = bt_find_key(res_blk, (bt_key*)&key, key_idx);
    if (!rc && key_idx == BT_RIGHTMOST)
    {
#ifdef PERMIT_CLUSTERS
        if (BT_IS_CLUS(pg))
        {
            pg = bt_cluster_tail(pg);
            if (BT_KEY_NUM(pg) > 1) return bt_cursor(pg, 1);
        }
#endif
        next = BT_NEXT(pg);
        if (next != XNULL)
        {
            CHECKP(next);
            return bt_cursor((char*)XADDR(next), 0); 
        } 
        else return bt_cursor();
    } 
    else return bt_cursor((char*)XADDR(res_blk), key_idx);
}

/// !!! potential error here
bt_cursor bt_find_gt(const xptr &root, const bt_key &key)
{
    bool rc;
    shft key_idx;
    xptr next;
    xptr res_blk = root;
	bt_key old_val=key;
    CHECKP(res_blk);
    char* pg = (char*)XADDR(res_blk);

    rc = bt_find_key(res_blk, (bt_key*)&key, key_idx);
    if (!rc && key_idx == BT_RIGHTMOST)
    {
#ifdef PERMIT_CLUSTERS
        if (BT_IS_CLUS(pg))
        {
            pg = bt_cluster_tail(pg);
            if (BT_KEY_NUM(pg) > 1) return bt_cursor(pg, 1);
        }
#endif
        next = BT_NEXT(pg);
        if (next != XNULL)
        {
            CHECKP(next);
            return bt_cursor((char*)XADDR(next), 0);
		}
        else return bt_cursor();
    }
    else
    {
        bt_cursor c((char*)XADDR(res_blk), key_idx);
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
}

bt_cursor bt_lm(const xptr& root) 
{
    CHECKP(root);
    char* pg = (char*)XADDR(root);

#ifdef PERMIT_CLUSTERS
    /// !!! FIX ME UP
#endif

    if (!BT_IS_LEAF(pg)) 
    {	
        return bt_lm(BT_LMP(pg));
	} 
    else 
    {
        return bt_cursor(pg, 0);
    }
}

void bt_insert(xptr &root, const bt_key &key, const object &obj)
{
    bool  rc;
    shft  key_idx = 0;
    shft  obj_idx = 0;
    xptr  insert_xpg = root; /* xptr passed to search functions, that can change, pointing to insert new data */
    char* insert_pg;

    /* check key length doesn't exceed limit */
    if (key.get_size() >= BT_PAGE_PAYLOAD / 2)
        throw USER_EXCEPTION2(SE1008, "The size of the index key exceeds max key limit");

    rc = bt_find_key(insert_xpg, (bt_key*)&key, key_idx);
    /* page could change */
    insert_pg = (char*)XADDR(insert_xpg);

    if (rc)
    {
        rc = bt_leaf_find_obj(insert_xpg, obj, key_idx, obj_idx);

      /*  if (rc) throw USER_EXCEPTION2(SE1008, "The key/object pair already exists in btree");
        else
        {*/
            CHECKP(insert_xpg);
            insert_pg = (char*)XADDR(insert_xpg);

            if (obj_idx == BT_RIGHTMOST)
                obj_idx = *((shft*)BT_CHNK_TAB_AT(insert_pg, key_idx) + 1);

            bt_leaf_insert(root, insert_pg, key_idx, false, key, obj, obj_idx);
        /*}*/
    } 
    else
    {
        CHECKP(insert_xpg);

        if (key_idx == BT_RIGHTMOST)
            key_idx = BT_KEY_NUM(insert_pg);
        bt_leaf_insert(root, insert_pg, key_idx, true, key, obj, obj_idx);
    }
}

void bt_modify(xptr &root, const bt_key &old_key, const bt_key &new_key, const object &obj)
{
    bt_delete(root, old_key, obj);
    bt_insert(root, new_key, obj);
}

/* light delete functions - delete only data from pages, not droping the pages, i.e no
   implementation of page merge/drop
 */
/* delete key/obj pair */
void bt_delete(xptr &root, const bt_key& key, const object &obj)
{
    bool    rc;
    shft    key_idx;
    shft    obj_idx;
    xptr    delete_xpg = root; /* xptr passed to search functions, that can change, pointing to page where data resides */
    char*   delete_pg = (char*)XADDR(delete_xpg);

    CHECKP(delete_xpg);

    rc = bt_find_key(delete_xpg, (bt_key*)&key, key_idx);
    if (rc)
    {
        rc = bt_leaf_find_obj(delete_xpg, obj, key_idx, obj_idx);
        /* page could change */
        CHECKP(delete_xpg);
        delete_pg = (char*)XADDR(delete_xpg);
        if (rc) bt_delete_obj(delete_pg, key_idx, obj_idx);
        else throw USER_EXCEPTION2(SE1008, "Cannot delete object which is not in the btree");
    }
    else throw USER_EXCEPTION2(SE1008, "Cannot delete object which is not in the btree");
}

/* delete key and all associated objects */
void bt_delete(xptr &root, const bt_key &key) 
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
        bt_leaf_delete_key(delete_pg, key_idx);
    }
}
/* drop empty page*/
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
		((btree_blk_hdr*)XADDR(left))->next=right;
		VMM_SIGNAL_MODIFICATION(left);

	}
	if (right!=XNULL)
	{
		CHECKP(right);
		((btree_blk_hdr*)XADDR(right))->prev=left;
		VMM_SIGNAL_MODIFICATION(right);

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
			memcpy(dst,(char*)XADDR(lmp)+sizeof(vmm_sm_blk_hdr),PAGE_SIZE-sizeof(vmm_sm_blk_hdr));
			CHECKP(parent);
			memcpy((char*)XADDR(parent)+sizeof(vmm_sm_blk_hdr),dst,PAGE_SIZE-sizeof(vmm_sm_blk_hdr));
			par_hdr->parent=grandpa;//restore
			par_hdr->prev=left_unc;
			par_hdr->next=right_unc;
			VMM_SIGNAL_MODIFICATION(parent);
			vmm_delete_block(lmp);

		}
		else
			bt_drop_page(par_hdr);	 
	}
}
