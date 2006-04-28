/*
 * File:  auxi.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "btstruct.h"
#include "btpage.h"
#include "vmm.h"
#include "nodes.h"

void bt_page_markup(char* pg, xmlscm_type t) {
	char dummy[10];
	(*BT_NEXT_PTR(pg))=XNULL;
	(*BT_PREV_PTR(pg))=XNULL;
	(*BT_PARENT_PTR(pg))=XNULL;
	(*BT_LMP_PTR(pg))=XNULL;
	(*BT_IS_LEAF_PTR(pg))=true;
	(*BT_IS_CLUS_PTR(pg))=false;
	(*BT_IS_CLUS_HEAD_PTR(pg))=false;
	(*BT_IS_CLUS_TAIL_PTR(pg))=false;
	(*BT_KEY_TYPE_PTR(pg))=t;
	(*BT_KEY_SIZE_PTR(pg))=xmlscm_type_size(t);
	(*BT_KEY_NUM_PTR(pg))=0;
	(*BT_HEAP_PTR(pg))=PAGE_SIZE;

    VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));
};

/* check page consistency */
void bt_page_consistency(char* pg, bt_key* key) {
	/* make some checks of page and key conformance */
	if (BT_KEY_TYPE(pg)!= key->get_type())
        throw USER_EXCEPTION2(SE1008, "Incorrespondence key types of the page and key searched for");
	if ((BT_VARIABLE_KEY_TYPE(key->get_type()) && BT_KEY_SIZE(pg)) || (!BT_VARIABLE_KEY_TYPE(key->get_type()) && !BT_KEY_SIZE(pg)))
        throw USER_EXCEPTION2(SE1008, "Divergence between key type and key size fields in page header");
}
