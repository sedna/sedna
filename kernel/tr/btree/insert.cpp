/*
 * File:  insert.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/btree/btintern.h"


/* temporary buffer used for performing page insert operations */
char insert_buf[BT_PAGE_SIZE];
/* debug counter of btree height */
//extern shft	BTREE_HEIGHT;


/* splitting function makes the splitting of page pg in the following manner:
   1) The split key is located. All keys < it will be transfered to the left page,
      all keys >= to the right page (rpg).
	  Note that pretender_idx identifies location where the caller function wishes to
	  insert new structures (that may be key with object or single object). In both
	  cases the size claimed by the caller function is passed in pretender_size parameter.
	  pretender_size and pretender_idx are accounted when calculating dividing key.
   2) the keys are being transfered together with contents and corresponding load
      data (big_ptrs for non-leaf pages; chunk table slots and chunks of objects
	  for leaf pages) between pg and rpg pages, using buffering techniques exploiting
	  internal static buffers (see functions in buff.cpp module)
   3) the function clears out in which of two pages the pretender is to be created,
      modifies the pretender_idx correspondingly if required and returns xptr of
	  pretender's page
      Note: in utmost case if in the page to be splitted there is only one key, this
	  situation is treated as well: one of the pages becomes empty, while all the data
	  in original page settle in another page.
 */
template<typename object>
xptr bt_page_split_tmpl(char* pg, const xptr rpg, shft & pretender_idx, shft pretender_size, bool insert_key)
{
	xptr    pg_xptr = ADDR2XPTR(pg);
    xptr    next_for_rpg = BT_NEXT(pg);
    bool    is_leaf_page = BT_IS_LEAF(pg);
	bool	var_key_size = BT_VARIABLE_KEY_TYPE(pg);
    shft    key_size = BT_KEY_SIZE(pg);
    shft    key_num = BT_KEY_NUM(pg);
    shft    split_idx = 0;
    char    *dst = NULL;
    int i;

	U_ASSERT(pretender_idx <= key_num);

	if ((key_num == 1)  && (pretender_idx == key_num)) {
		split_idx = 1;
	}
	else if ((next_for_rpg == XNULL) && (pretender_idx == key_num))
    {
		split_idx = key_num - 1;
    }
    else
    {
        split_idx = bt_find_split_key_tmpl<object>(pg, pretender_idx, pretender_size);
    }

	U_ASSERT(pretender_idx <= key_num);


//	 I. Copy data from the current node to the new one (from the last key to the split key)
//			All keys with values >= split key will go to the right page

	dst = insert_buf;
	CHECKP(pg_xptr);
	bt_page_markup(dst, BT_KEY_TYPE(pg));

	BT_KEY_NUM(dst) = key_num - split_idx;
	BT_IS_LEAF(dst) = BT_IS_LEAF(pg);
	BT_PREV(dst) = pg_xptr;
	BT_NEXT(dst) = BT_NEXT(pg);
//	BT_PARENT(dst) = BT_PARENT(pg);

	U_ASSERT(!BT_IS_CLUS(pg) || BT_IS_CLUS_TAIL(pg));

	if (var_key_size) {
		for(i = split_idx; i < key_num; i++) {
			btree_key_hdr *ks, *kd;
			ks = BT_KEY_ITEM_AT(pg, i);
			kd = BT_KEY_ITEM_AT(dst, i-split_idx);

			BT_HEAP(dst) -= ks->k_size;
			memcpy(dst + BT_HEAP(dst), pg + ks->k_shft, ks->k_size);

			kd->k_shft = BT_HEAP(dst);
			kd->k_size = ks->k_size;
		}
	} else {
		memcpy(BT_KEY_TAB(dst), BT_KEY_TAB_AT(pg, split_idx), key_size * (key_num - split_idx));
	}

	if (is_leaf_page) {
		for(i = split_idx; i < key_num; i++) {
			btree_chnk_hdr *cs, *cd;
			cs = BT_CHNK_ITEM_AT(pg, i);
			cd = BT_CHNK_ITEM_AT(dst, i-split_idx);

			BT_HEAP(dst) -= cs->c_size * sizeof(object);
			memcpy(dst + BT_HEAP(dst), pg + cs->c_shft, cs->c_size * sizeof(object));

			cd->c_shft = BT_HEAP(dst);
			cd->c_size = cs->c_size;
		}
	} else {
		memcpy(BT_BIGPTR_TAB(dst), BT_BIGPTR_TAB_AT(pg, split_idx), sizeof(xptr) * (key_num - split_idx));
	}

	CHECKP(rpg);
	VMM_SIGNAL_MODIFICATION(rpg);
	memcpy((char *) XADDR(rpg) + sizeof(vmm_sm_blk_hdr), dst + sizeof(vmm_sm_blk_hdr), BT_PAGE_SIZE - sizeof(vmm_sm_blk_hdr));


//	 II. Normalize data at the current node

	CHECKP(pg_xptr);
	memcpy(dst, pg, sizeof(btree_blk_hdr));
	BT_HEAP_SET_NULL(dst);
	BT_KEY_NUM(dst) = split_idx;
	BT_NEXT(dst) = rpg;

	if (var_key_size) {
		for(i = 0; i < split_idx; i++) {
			btree_key_hdr *ks, *kd;
			ks = BT_KEY_ITEM_AT(pg, i);
			kd = BT_KEY_ITEM_AT(dst, i);

			BT_HEAP(dst) -= ks->k_size;
			memcpy(dst + BT_HEAP(dst), pg + ks->k_shft, ks->k_size);

			kd->k_shft = BT_HEAP(dst);
			kd->k_size = ks->k_size;
		}
	} else {
		memcpy(BT_KEY_TAB(dst), BT_KEY_TAB(pg), key_size * split_idx);
	}

	if (is_leaf_page) {
		for(i = 0; i < split_idx; i++) {
			btree_chnk_hdr *cs, *cd;
			cs = BT_CHNK_ITEM_AT(pg, i);
			cd = BT_CHNK_ITEM_AT(dst, i);

			BT_HEAP(dst) -= cs->c_size * sizeof(object);
			memcpy(dst + BT_HEAP(dst), pg + cs->c_shft, cs->c_size * sizeof(object));

			cd->c_shft = BT_HEAP(dst);
			cd->c_size = cs->c_size;
		}
	} else {
		memcpy(BT_BIGPTR_TAB(dst), BT_BIGPTR_TAB(pg), sizeof(xptr) * split_idx);
	}

	VMM_SIGNAL_MODIFICATION(pg_xptr);
	memcpy(pg + sizeof(vmm_sm_blk_hdr), dst + sizeof(vmm_sm_blk_hdr), BT_PAGE_SIZE - sizeof(vmm_sm_blk_hdr));


	if (next_for_rpg!=XNULL)
	{
		CHECKP(next_for_rpg);
		VMM_SIGNAL_MODIFICATION(next_for_rpg);
		/* right <- */
		(*BT_PREV_PTR((char*)XADDR(next_for_rpg))) = rpg;
	}

    /* clear out in which block pretender will reside and adjust new index of pretender in that page
		if we insert only object, we must return the page, that actually contains the pretender key!
	*/

	if ((pretender_idx < split_idx) || ((pretender_idx == split_idx) && insert_key && (pretender_idx < key_num))) {
        /* pretender index remains the same */
		U_ASSERT(pretender_idx <= split_idx);
        return pg_xptr;
	}
    else
    {
        /* pretender goes to the right page */
        pretender_idx = pretender_idx - split_idx;
		U_ASSERT(pretender_idx <= (key_num - split_idx));

		return rpg;
    }
}

/* Locate split key - the first key in order from left to right in key table such that the joint memory volume
   occupied by all keys together with their load data, that are less than split key, overpass the half of
   page total available payload space (BT_PAGE_PAYLOAD). When calculating the virtual "pretender" is also
   accounted via it's claimed position and size;
   returns the index of split key;
   the flag pretender_goes_left designates, if pretender is accounted in left or in right part;
 */
template<typename object>
shft bt_find_split_key_tmpl(char* pg, const shft pretender_idx, shft pretender_size)
{
    bool    is_leaf_page = BT_IS_LEAF(pg);
    shft    key_num = BT_KEY_NUM(pg);
    shft    key_size = BT_KEY_SIZE(pg);
    shft    volume = 0;					/* accumulates the total volume occupied by keys together with load */
    shft    border_volume = BT_PAGE_PAYLOAD / 2;

    for (int i = 0; i < key_num; i++)
    {
        /* account pretender */
        if (i == pretender_idx)
        {
            volume += pretender_size;
            if (volume > border_volume)
                /* i-1 was below half, the pretender size is less than half, it can not overload the page
                   this is the case when pretender becomes the last key of left-hand page - pretender idx
                   equals split idx */
            return i;
        }

        /* account key volume */
        if (key_size) /* fixed-size keys */
            volume += key_size;
        else
        {
            volume += 2*sizeof(shft);
            volume += *(((shft*)BT_KEY_TAB_AT(pg, i))+1);
        }

        /* account load data volume */
        if (is_leaf_page)
        {
            volume += 2 * sizeof(shft);
            volume += (*(((shft*)BT_CHNK_TAB_AT(pg, i)) + 1)) * sizeof(object);
        } else
            volume+=sizeof(xptr);

        if (volume > border_volume)
        {
			/* if the key that passed the half overloaded the page, take it's predecessor,
			   otherwise - take this key */
            if (volume >= BT_PAGE_PAYLOAD) return i;
            else return ++i;
        }
    }

    throw USER_EXCEPTION2(SE1008, "The overall volume of keys with loads in page is beneaf half of BT_PAGE_PAYLOAD");
}

/* for given leaf page insert a given pair (key, obj);
   create_new_key flag indicates if target key exists in this page;
   in case the key exists, key_idx points to that key in key table and object is to be added
   to the chunk of that existing key into obj_idx position;
   in case the key doesn't exist, it is created in key_idx place in key table, the object being
   allocated in new chunk of that created key;
   modifieable parameter 'root' gives xptr of the current btree root, which can be changed in case
   tree height is increased inside function;
   returns xptr of the leaf page where the operands were actually located;
   note: page splitting may occur inside that function, in case lack of enough space in original
   page. In that case the corresponding key is promoted up to the parent page, causing possibly
   recursive splitting of non-leaf parent pages.
 */
void get_clust_head (xptr & pg)
{

	char* blk=(char*)XADDR(pg);
	CHECKP(pg);
	while (true)
	{
		if (BT_PREV(blk)==XNULL || !BT_IS_CLUS(blk)||BT_IS_CLUS_HEAD(blk))
		{
			return ;
		}
		pg=BT_PREV(blk);
		CHECKP(pg);
		blk=(char*)XADDR(pg);
		if (!BT_IS_CLUS(blk))
			throw USER_EXCEPTION2(SE1008, "Cluster error");

	}
	return;
}

void bt_cluster_head (xptr & pg)
{

	char* blk=(char*)XADDR(pg);
	CHECKP(pg);
	while (true)
	{
		if (BT_PREV(blk)==XNULL || !BT_IS_CLUS(blk)||BT_IS_CLUS_HEAD(blk))
		{
			return ;
		}
		pg=BT_PREV(blk);
		CHECKP(pg);
		blk=(char*)XADDR(pg);
		if (!BT_IS_CLUS(blk))
			throw USER_EXCEPTION2(SE1008, "Cluster error");

	}
	return;
}

template<typename object>
xptr bt_nleaf_insert_tmpl(xptr &root, const bt_key &new_key, xptr new_big_ptr, bt_path &path)
{
	bt_path_item pi = path.back();
	path.pop_back();
	CHECKP(pi.pg);
	return bt_internal_insert_tmpl<object>(root, (char *) XADDR(pi.pg), pi.idx, true, new_key, NULL_OBJECT, 0, path, true, new_big_ptr);
}

template<typename object>
xptr bt_internal_insert_tmpl(
	xptr &root,            // root page (may be changed after insertion)
	char* pg,              // the page to insert into
	shft key_idx,          // key index in the page to insert into
	bool create_new_key,   // do we create new key
	const bt_key &new_key, // the key to insert
	const object &obj,     // the object to insert (if inserting object)
	shft obj_idx,          // object index to insert
	bt_path &path,         // path to the page to insert into
	bool with_bt,          //
	xptr new_big_ptr       // the page pointer to insert (if inserting subtree)
)
{
	bt_key key(new_key);
	shft total_key_size;
	shft key_with_load;
	bool insert_further = true;
	xptr pg_xptr = ADDR2XPTR(pg);
	xptr key_pg_xptr;
    xptr rpg;

	while (insert_further) {
		CHECKP(pg_xptr);
		insert_further = false;
		total_key_size = key.get_size() + (BT_VARIABLE_KEY_TYPE(pg)? (2 * sizeof(shft)) : 0);

		if (!BT_IS_LEAF(pg)) {
			// Ia.  Insert key into nonleaf page

			key_with_load = total_key_size + sizeof(xptr);

			if(!bt_page_fit(pg, key_with_load))
			{
				vmm_alloc_data_block(&rpg);
				CHECKP(pg_xptr);
				key_pg_xptr = bt_page_split_tmpl<object>(pg, rpg, key_idx, key_with_load);
				insert_further = true;
			} else {
				key_pg_xptr = pg_xptr;
			}

			CHECKP(key_pg_xptr);
			bt_nleaf_do_insert_key((char*)XADDR(key_pg_xptr), key_idx, key, new_big_ptr);
		} else if (create_new_key) {
			// Ib.  Insert key into leaf page

			key_with_load = total_key_size + sizeof(object) + sizeof(btree_chnk_hdr);

			if (!bt_page_fit(pg, key_with_load))
			{
				vmm_alloc_data_block(&rpg);
				CHECKP(pg_xptr);
				key_pg_xptr = bt_page_split_tmpl<object>(pg, rpg, key_idx, key_with_load, true);
				insert_further = true;
			} else {
				key_pg_xptr = pg_xptr;
			}
			CHECKP(key_pg_xptr);

			U_ASSERT(bt_page_fit((char*)XADDR(key_pg_xptr), key_with_load));

			bt_leaf_do_insert_key_tmpl<object>((char*)XADDR(key_pg_xptr), key_idx, key, obj);
		} else {
			// Ic.  Insert object into leaf page

			if (!bt_page_fit(pg, sizeof(object)))
			{
				vmm_alloc_data_block(&rpg);
				CHECKP(pg_xptr);

				if (BT_KEY_NUM(pg) == 1)
				{ /* cluster case - nothing is promoted, instantly return */
				#ifdef PERMIT_CLUSTERS
					bt_page_clusterize_tmpl<object>(root, pg, rpg, obj, obj_idx);
					return rpg;
				#else
					throw USER_EXCEPTION2(SE1008, "Not enough space to insert new key/object into page (clusterization prohibited)");
				#endif
				}
				/* not cluster case - page splitting */
				key_pg_xptr = bt_page_split_tmpl<object>(pg, rpg, key_idx, sizeof(object), false);
				insert_further = true;
			} else {
				key_pg_xptr = pg_xptr;
			}

			CHECKP(key_pg_xptr);
			bt_do_insert_obj_tmpl<object>((char*)XADDR(key_pg_xptr), key_idx, obj, obj_idx);
		}

		// II. If splitting took place
		if (insert_further) {
			CHECKP(rpg);
			new_big_ptr = rpg;
			key.setnew((char *) XADDR(rpg), 0);
			bt_cluster_head(rpg);
			new_big_ptr = rpg;

			if (path.empty()) {
				// IIa. If pg was root, make new root
				xmlscm_type key_type = BT_KEY_TYPE((char *) XADDR(rpg));
				xptr lpg = pg_xptr;

				CHECKP(pg_xptr);
				pg = (char *) XADDR(pg_xptr);
				if (BT_IS_LEAF(pg) && BT_IS_CLUS(pg)) { bt_cluster_head(lpg); };

				U_ASSERT(BT_PREV((char*)(XADDR(lpg))) == XNULL);
				U_ASSERT(root == lpg);

				vmm_alloc_data_block(&pg_xptr);
				pg = (char *) XADDR(pg_xptr);
				VMM_SIGNAL_MODIFICATION(pg_xptr);
				bt_page_markup(pg, key_type);

				BT_IS_LEAF(pg) = false;
				BT_LMP(pg) = lpg;
				key_idx = 0;

				root = pg_xptr;
			} else {
				// IIb. If pg was not root, just promote key
				pg_xptr = path.back().pg;
				// optimization:
				// key_idx = path.back().idx;
				path.pop_back();
				pg = (char *) XADDR(pg_xptr);

				CHECKP(pg_xptr);
				U_ASSERT(!BT_IS_LEAF(pg));
				if (bt_nleaf_find_key(pg, &key, key_idx, true)) {
					throw USER_EXCEPTION2(SE1008, "The key to be inserted to non-leaf page is already there");
				}

				CHECKP(pg_xptr);
				if (key_idx == BT_RIGHTMOST) key_idx = BT_KEY_NUM(pg);
			}
		}
	}
	return root;
}

/* attach new (rpg) page next to pg as cluster page. If the original pg page is not cluster yet,
   mark it as cluster, i.e. form the cluster. The new page is initially unformatted. Insert object
   into new page
 */
template<typename object>
void bt_page_clusterize_tmpl(xptr &root, char* pg, const xptr rpg, const object &obj, shft obj_idx)
{
	xptr        pg_xptr = ADDR2XPTR(pg);
    xptr        next_for_rpg;
//    xmlscm_type pg_type = BT_KEY_TYPE(pg);
	btree_chnk_hdr c = *(BT_CHNK_ITEM_AT(pg, 0));
	char * tmp_rpg = insert_buf;
	char * goal_page;

    if (!BT_IS_LEAF(pg)) throw USER_EXCEPTION2(SE1008, "Attempt to clusterize non-leaf page");
    if (BT_KEY_NUM(pg) != 1) throw USER_EXCEPTION2(SE1008, "Number of keys in original page is not equal to 1");

	CHECKP(pg_xptr);
	memcpy(tmp_rpg, pg, BT_CHNK_TAB_AT(pg, BT_KEY_NUM(pg)) - pg);

	VMM_SIGNAL_MODIFICATION(pg_xptr);

    if (!BT_IS_CLUS(pg))
    {
        (*BT_IS_CLUS_PTR(pg)) = true;
        (*BT_IS_CLUS_HEAD_PTR(pg)) = true;
        (*BT_IS_CLUS_PTR(tmp_rpg)) = true;
        (*BT_IS_CLUS_TAIL_PTR(tmp_rpg)) = true;
    }

    next_for_rpg = BT_NEXT(pg);
	(*BT_NEXT_PTR(pg)) = rpg;
    BT_IS_CLUS_HEAD(tmp_rpg) = false;
	BT_HEAP_SET_NULL(tmp_rpg);
	BT_PREV(tmp_rpg) = pg_xptr;

	if (BT_IS_CLUS_TAIL(pg)) {
		BT_IS_CLUS_TAIL(pg) = false;
	}

	// move key from pg to tmp_rpg
	if (BT_VARIABLE_KEY_TYPE(pg)) {
		btree_key_hdr k = *(BT_KEY_ITEM_AT(pg, 0));
		BT_HEAP(tmp_rpg) = (shft) (BT_PAGE_SIZE - k.k_size);
		memcpy(tmp_rpg + BT_HEAP(tmp_rpg), pg + k.k_shft, k.k_size);
		BT_KEY_ITEM_AT(tmp_rpg, 0)->k_shft = BT_HEAP(tmp_rpg);
	}

	if (obj_idx == c.c_size) {
		BT_HEAP(tmp_rpg) -= sizeof(object);
		* ((object *) (tmp_rpg + BT_HEAP(tmp_rpg))) = obj;
		BT_CHNK_ITEM_AT(tmp_rpg, 0)->c_shft = BT_HEAP(tmp_rpg);
		BT_CHNK_ITEM_AT(tmp_rpg, 0)->c_size = 1;
	} else {
		int split_idx = c.c_size / 2;

		BT_HEAP(tmp_rpg) -= (c.c_size - split_idx) * sizeof(object);
		BT_CHNK_ITEM_AT(tmp_rpg, 0)->c_shft = BT_HEAP(tmp_rpg);
		BT_CHNK_ITEM_AT(tmp_rpg, 0)->c_size = (c.c_size - split_idx);

		BT_HEAP(pg) += (c.c_size - split_idx) * sizeof(object);
		BT_CHNK_ITEM_AT(pg, 0)->c_size = split_idx;
		BT_CHNK_ITEM_AT(pg, 0)->c_shft = BT_PAGE_SIZE
			- (BT_VARIABLE_KEY_TYPE(pg) ? BT_KEY_ITEM_AT(pg, 0)->k_size : 0)
			- split_idx * sizeof(object);

		memcpy(
			tmp_rpg + BT_CHNK_ITEM_AT(tmp_rpg, 0)->c_shft,
			pg + c.c_shft + split_idx * sizeof(object),
			(c.c_size - split_idx) * sizeof(object));

		memmove(
			pg + BT_CHNK_ITEM_AT(pg, 0)->c_shft,
			pg + c.c_shft,
			split_idx * sizeof(object));

		// When we are sure, the key is at the end of block, it's just easier to work with it.

		if (BT_VARIABLE_KEY_TYPE(pg)) {
			BT_KEY_ITEM_AT(pg, 0)->k_shft = BT_PAGE_SIZE - BT_KEY_ITEM_AT(pg, 0)->k_size;
			memmove(
				tmp_rpg + BT_KEY_ITEM_AT(tmp_rpg, 0)->k_shft,
				pg + BT_KEY_ITEM_AT(pg, 0)->k_shft,
				BT_KEY_ITEM_AT(pg, 0)->k_size);
		}

		if (obj_idx >= split_idx) {
			goal_page = tmp_rpg;
			obj_idx -= split_idx;
		} else {
			goal_page = pg;
		}

		// DO INSERT OBJECT

		BT_HEAP(goal_page) -= sizeof(object);
		memmove(goal_page + BT_HEAP(goal_page), goal_page + BT_HEAP(goal_page) + sizeof(object), obj_idx * sizeof(object));
		BT_CHNK_ITEM_AT(goal_page, 0)->c_shft -= sizeof(object);
		BT_CHNK_ITEM_AT(goal_page, 0)->c_size ++;
		*(((object *) (goal_page + BT_HEAP(goal_page))) + obj_idx) = obj;
	}


	if (next_for_rpg!=XNULL)
	{
		CHECKP(next_for_rpg);
		VMM_SIGNAL_MODIFICATION(next_for_rpg);
		/* right <- */
		(*BT_PREV_PTR((char*)XADDR(next_for_rpg))) = rpg;

	}

	CHECKP(rpg);
	VMM_SIGNAL_MODIFICATION(rpg);
	memcpy((char*) XADDR(rpg) + sizeof(vmm_sm_blk_hdr), tmp_rpg + sizeof(vmm_sm_blk_hdr), BT_PAGE_SIZE - sizeof(vmm_sm_blk_hdr));

}


/* make actual insertion of key and object sticking to that key into given place in key table
   of leaf page; this function is used when new key is to be created in the page;
   no check for fittness
 */
template<typename object>
void bt_leaf_do_insert_key_tmpl(char* pg, shft key_idx, const bt_key& key, const object &obj)
{
	char *	key_pos = BT_KEY_TAB_AT(pg, key_idx);
	bool	var_key_size = BT_KEY_SIZE(pg) == 0;
	shft	key_size = (var_key_size ? sizeof(btree_key_hdr) : BT_KEY_SIZE(pg));
	char *	chnk_pos = BT_CHNK_TAB_AT(pg, key_idx);
	shft	chnk_size = sizeof(btree_chnk_hdr);
	char *	last = BT_CHNK_TAB_AT(pg, BT_KEY_NUM(pg));

/*
	U_ASSERT(
		((pg + BT_HEAP(pg) - sizeof(object) - key.get_size()) -
		(last + key_size + chnk_size)) > 0
		);
*/

	VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));

	memmove(chnk_pos + chnk_size + key_size, chnk_pos, (shft) (last - chnk_pos));
	memmove(key_pos + key_size,              key_pos,  (shft) (chnk_pos - key_pos));

	BT_KEY_NUM(pg) += 1;

	if (var_key_size) {
		BT_HEAP(pg) -= key.get_size();
		memcpy(pg + BT_HEAP(pg), key.data(), key.get_size());
		BT_KEY_ITEM_AT(pg, key_idx)->k_shft = BT_HEAP(pg);
		BT_KEY_ITEM_AT(pg, key_idx)->k_size = key.get_size();
	} else {
		memcpy(BT_KEY_TAB_AT(pg, key_idx), key.data(), key.get_size());
	}

	BT_HEAP(pg) -= sizeof(object);
	* (object *) (pg + BT_HEAP(pg)) = obj;
	BT_CHNK_ITEM_AT(pg, key_idx)->c_shft = BT_HEAP(pg);
	BT_CHNK_ITEM_AT(pg, key_idx)->c_size = 1;

}

/* make actual insertion of key and big_ptr sticking to that key into given place in key table
   of non-leaf page; this function is used when new key is to be created in the page
   no check for fittness
 */
void bt_nleaf_do_insert_key(char* pg, shft key_idx, const bt_key& key, const xptr big_ptr)
{
	U_ASSERT(key_idx <= BT_KEY_NUM(pg));

	char *	key_pos = BT_KEY_TAB_AT(pg, key_idx);
	bool	var_key_size = BT_KEY_SIZE(pg) == 0;
	shft	key_size = (var_key_size ? 2 * sizeof(shft) : BT_KEY_SIZE(pg));
	char *	ptr_pos = BT_BIGPTR_TAB_AT(pg, key_idx);
	shft	ptr_size = sizeof(xptr);
	char *	last = BT_BIGPTR_TAB_AT(pg, BT_KEY_NUM(pg));
/*
	U_ASSERT(
		(pg + BT_HEAP(pg)) -
		(BT_BIGPTR_TAB_AT(pg, BT_KEY_NUM(pg)) + key.get_size() + (var_key_size ? sizeof(btree_key_hdr) : 0) + ptr_size)
		> 0
		);
*/
	VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));

	memmove(ptr_pos + ptr_size + key_size, ptr_pos, (shft) (last - ptr_pos));
	memmove(key_pos + key_size,            key_pos, (shft) (ptr_pos - key_pos));

	BT_KEY_NUM(pg) += 1;

	* (xptr *) BT_BIGPTR_TAB_AT(pg, key_idx) = big_ptr;

	if (var_key_size) {
		BT_HEAP(pg) -= key.get_size();
		memcpy(pg + BT_HEAP(pg), key.data(), key.get_size());
		BT_KEY_ITEM_AT(pg, key_idx)->k_shft = BT_HEAP(pg);
		BT_KEY_ITEM_AT(pg, key_idx)->k_size = key.get_size();
	} else {
		memcpy(BT_KEY_TAB_AT(pg, key_idx), key.data(), key.get_size());
	}

}

/* make actual insertion of object into chunk of given key of leaf page;
   the key designated with index already exists in key table;
   no check for fittness
 */
template<typename object>
void bt_do_insert_obj_tmpl(char* pg, shft key_idx, const object &obj, shft obj_idx)
{
	int		old_heap_shift = BT_HEAP(pg);
	int		new_heap_shift = old_heap_shift - sizeof(object);
	int		chnk_shift = BT_CHNK_ITEM_SHIFT(pg, key_idx);
	int		obj_shift = chnk_shift + obj_idx * sizeof(object);
	int		insertion_obj_shift = obj_shift - sizeof(object);
/*
	U_ASSERT(
		(pg + BT_HEAP(pg)) - (BT_CHNK_TAB_AT(pg, BT_KEY_NUM(pg)) + sizeof(object))	> 0
		);
*/

	VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));
	memmove(pg + new_heap_shift, pg + old_heap_shift, (shft) (obj_shift - old_heap_shift));

	* (object *) (pg + insertion_obj_shift) = obj;

	// update all heap "pointers"

	if (BT_KEY_SIZE(pg) == 0) {										// in case of variable key length, update key pointers
		for (int i = 0; i < BT_KEY_NUM(pg); i++) {
			if (BT_KEY_ITEM_AT(pg, i)->k_shft < chnk_shift)
				{ BT_KEY_ITEM_AT(pg, i)->k_shft -= sizeof(object); }
		}
	}

	for (int i = 0; i < BT_KEY_NUM(pg); i++) {						// update chunk "pointers"
		if (BT_CHNK_ITEM_AT(pg, i)->c_shft <= chnk_shift)
			{ BT_CHNK_ITEM_AT(pg, i)->c_shft -= sizeof(object); }
	}

	BT_HEAP(pg) -= sizeof(object);
	BT_CHNK_ITEM_AT(pg, key_idx)->c_size += 1;

/*
	if (BT_IS_CLUS(pg) && (key_idx == 0)) {
		xptr xpg = ADDR2XPTR(pg);
		if ((obj_idx == 0) && !BT_IS_CLUS_HEAD(pg)) {
			xptr pxpg = BT_PREV(pg);
			object a, b;
			b = * (object*) (pg + BT_CHNK_ITEM_AT(pg, 0)->c_shft);
			CHECKP(pxpg);
			pg = (char *) XADDR(pxpg);
			a = * (object*) (pg + BT_CHNK_ITEM_AT(pg, 0)->c_shft + (BT_CHNK_ITEM_AT(pg, 0)->c_size - 1) * sizeof(object));
			U_ASSERT(a < b);
		}
		if ((obj_idx == (BT_CHNK_ITEM_AT(pg, 0)->c_size - 1)) && !BT_IS_CLUS_TAIL(pg)) {
			xptr nxpg = BT_NEXT(pg);
			object a, b;
			a = * (object*) (pg + BT_CHNK_ITEM_AT(pg, 0)->c_shft + (BT_CHNK_ITEM_AT(pg, 0)->c_size - 1) * sizeof(object));
			CHECKP(nxpg);
			pg = (char *) XADDR(nxpg);
			b = * (object*) (pg + BT_CHNK_ITEM_AT(pg, 0)->c_shft);
			U_ASSERT(a < b);
			CHECKP(xpg);
		}
		CHECKP(xpg);
	}
*/
}

/* check if the given page will fit insertion of data (key/obj) of given size
   makes pessimistic calculus, assuming new table places will be certainly created
   note: the function can deny insertion for some cases when insertion is still possible,
   that's no matter cause the page is still almost full in this cases.
 */
bool bt_page_fit(char* pg, shft size)
{
    return !(BT_PFS(pg) <= size);
}



#define MAKE_IMPLS(t) \
    template xptr bt_internal_insert_tmpl<t>(xptr &root, char* pg, shft key_idx, bool create_new_key, const bt_key &new_key, const t &obj, shft obj_idx, bt_path &path, bool with_bt, xptr new_big_ptr); \
	template xptr bt_nleaf_insert_tmpl<t>(xptr &root, const bt_key &new_key, xptr new_big_ptr, bt_path &path);

#include "tr/btree/make_impl.h"
