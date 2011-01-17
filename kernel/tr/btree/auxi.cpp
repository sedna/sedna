/*
 * File:  auxi.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/vmm/vmm.h"

#include "tr/btree/btintern.h"
#include "tr/btree/btstruct.h"
#include "tr/btree/btpage.h"

void bt_page_markup(char* pg, xmlscm_type t) {
//	VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));
	(*BT_NEXT_PTR(pg))=XNULL;
	(*BT_PREV_PTR(pg))=XNULL;
//	(*BT_PARENT_PTR(pg))=XNULL;
	(*BT_LMP_PTR(pg))=XNULL;
	(*BT_IS_LEAF_PTR(pg))=true;
	(*BT_IS_CLUS_PTR(pg))=false;
	(*BT_IS_CLUS_HEAD_PTR(pg))=false;
	(*BT_IS_CLUS_TAIL_PTR(pg))=false;
	(*BT_KEY_TYPE_PTR(pg))=t;
	(*BT_KEY_SIZE_PTR(pg))=xmlscm_type_size(t);
	(*BT_KEY_NUM_PTR(pg))=0;
	BT_HEAP_SET_NULL(pg);
};

/* check page consistency */
void bt_page_consistency(char* pg, bt_key* key) {
	/* make some checks of page and key conformance */
	if (BT_KEY_TYPE(pg)!= key->get_type())
        throw USER_EXCEPTION2(SE1008, "Incorrespondence key types of the page and key searched for");
	if ((BT_VARIABLE_KEY_TYPE(key->get_type()) && BT_KEY_SIZE(pg)) || (!BT_VARIABLE_KEY_TYPE(key->get_type()) && !BT_KEY_SIZE(pg)))
        throw USER_EXCEPTION2(SE1008, "Divergence between key type and key size fields in page header");
}


#define CHECK_ASSERTION(p) if (!(p)) throw USER_EXCEPTION2(SE1008, "Index consistency check failed");

/* check page consistency (adult) */

template<typename object>
void bt_check_page_consistency_tmpl(xptr pg, bt_key* k, bool leftmost)
{
	char * p = (char *) XADDR(pg);
	bt_key ck;
	CHECKP(pg);

//	U_ASSERT(BT_KEY_NUM(p) != 0);

	if (!leftmost) {
		CHECK_ASSERTION(BT_KEY_TYPE(p) == k->get_type());
	}

	// check if keys are in right order
	for (int i = 0; i < BT_KEY_NUM(p); i++) {
	   // check if all key pointers are at least in the heap
		if (BT_VARIABLE_KEY_TYPE(p)) {
			btree_key_hdr * v = BT_KEY_ITEM_AT(p, i);
			CHECK_ASSERTION((v->k_shft >= BT_HEAP(p)) && (v->k_shft + v->k_size <= BT_PAGE_SIZE));
		}
		ck.setnew(p, i);
		if (!leftmost) {
			CHECK_ASSERTION((*k) <= ck);
		} else {
			leftmost = false;
		}
		k->setnew(p, i);
	}

	if (BT_IS_LEAF(p)) {
		// check the existance of objects
		for (int i = 0; i < BT_KEY_NUM(p); i++) {
			btree_chnk_hdr * v = BT_CHNK_ITEM_AT(p, i);
			xptr* pt = (xptr *) (p + v->c_shft);
			CHECK_ASSERTION((v->c_shft >= BT_HEAP(p)) && (v->c_shft + v->c_size <= BT_PAGE_SIZE));
			xptr save = XNULL;
			for (int j = 0; j < v->c_size; j++) {
				// order of objects
				CHECK_ASSERTION(bt_cmp_obj_tmpl<object>(pt[j], save) > 0);
				save = pt[j];
				CHECK_ASSERTION(XADDR(pt[j]) != NULL);
				CHECKP(pg);
			}
		}
	} else {
		for (int i = 0; i < BT_KEY_NUM(p); i++) {
			xptr pt = *((xptr *) BT_BIGPTR_TAB_AT(p, i));
			char * ptr = (char *) XADDR(pt);
			CHECK_ASSERTION((((uintptr_t) ptr) & PAGE_REVERSE_BIT_MASK) == 0);
		}
	}
}

template<typename object>
void bt_check_bsubtree_tmpl(xptr pg, bt_key* left_key, bool leftmost)
{
	char * p = (char *) XADDR(pg);

	bt_check_page_consistency_tmpl<object>(pg, left_key, leftmost);

	CHECKP(pg);

	xptr cpage;
	bt_key ck, nk;
	if (!BT_IS_LEAF(p)) {
		cpage = BT_LMP(p);
		if (cpage != XNULL) {
			bt_check_bsubtree_tmpl<object>(cpage, &ck, true);
			CHECKP(pg);
		}

		for (int i = 0; i < BT_KEY_NUM(p); i++) {
			nk.setnew(p, i);
			if (cpage != XNULL) CHECK_ASSERTION(ck < nk);
			cpage = *((xptr *) BT_BIGPTR_TAB_AT(p, i));
			ck = nk;
			bt_check_bsubtree_tmpl<object>(cpage, &ck, false);
			CHECKP(pg);
		}
	}
}

template<typename object>
void bt_check_btree_tmpl(xptr pg)
{
	bt_key k;
	bt_check_bsubtree_tmpl<object>(pg, &k, true);
}

void  bt_check_btree(xptr btree)
{
    bt_check_btree_tmpl<xptr>(btree);
}
