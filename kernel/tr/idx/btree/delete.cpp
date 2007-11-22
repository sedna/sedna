/*
 * File:  delete.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/idx/btree/btpage.h"
#include "tr/idx/btree/btintern.h"
#include "tr/idx/btree/btstruct.h"
#include "tr/idx/btree/buff.h"
#include "tr/idx/btree/btree.h"
#include "tr/vmm/vmm.h"


/* temporary buffer used for performing page delete operations */
char delete_buf[PAGE_SIZE];

void bt_delete_obj(char* pg, shft key_idx, shft obj_idx) 
{
// This function works properly in assumption that CHUNK_ITEM_SIZE >= 2
    if (*((shft*)BT_CHNK_TAB_AT(pg, key_idx) + 1) == 1)
    {
        bt_leaf_delete_key(pg, key_idx);
        return;
    }

	shft	old_heap_shift = BT_HEAP(pg);
	shft	obj_shift = BT_CHNK_ITEM_SHIFT(pg, key_idx) + obj_idx * sizeof(object);

	VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));
	memmove(pg + old_heap_shift + sizeof(object), pg + old_heap_shift, obj_shift - old_heap_shift);

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

void bt_leaf_delete_key(char* pg, shft key_idx) 
{
// This function works properly in assumption that CHUNK_ITEM_SIZE = 1 !!!

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
	memmove(key_pos + len, chnk_pos + chnk_size, (last - chnk_pos) - chnk_size);
	memmove(pg + heap_shift + sizeof(object), pg + heap_shift, obj_shift - heap_shift);
	heap_shift += sizeof(object);

	BT_KEY_NUM(pg) -= 1;

	if (var_key_size) {
		memmove(pg + heap_shift + actkey_size, pg + heap_shift, actkey_shift - heap_shift);
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
	memmove(key_pos + len, ptr_pos + ptr_size, (last - ptr_pos) - ptr_size);

	BT_KEY_NUM(pg) -= 1;

	if (var_key_size) {
		memmove(pg + heap_shift + actkey_size, pg + heap_shift, actkey_shift - heap_shift);
		heap_shift += actkey_size;

		for (int i = 0; i < BT_KEY_NUM(pg); i++) {						// update key "pointers"
			btree_key_hdr * v = BT_KEY_ITEM_AT(pg, i);
			if (v->k_shft < actkey_shift) { v->k_shft += actkey_size; }
		}
	}

	BT_HEAP(pg) = heap_shift;
}
