/*
 * File:  delete.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "btpage.h"
#include "btintern.h"
#include "btstruct.h"
#include "buff.h"
#include "vmm.h"


/* temporary buffer used for performing page delete operations */
char delete_buf[PAGE_SIZE];

/* */
void bt_delete_obj(char* pg, shft key_idx, shft obj_idx) 
{
    /* if there is only one object for given key, the key is to be deleted with all associated data */
    if (*((shft*)BT_CHNK_TAB_AT(pg, key_idx) + 1) == 1)
    {
        bt_leaf_delete_key(pg, key_idx);
        return;
    }

    shft    key_size = BT_KEY_SIZE(pg);
    char*   dst = bt_tune_buffering(true, key_size);
    char*   buf = dst;
    char*   src = BT_KEY_TAB(pg);
    shft    heap_shft;
    int     i;

    bt_buffer_header(pg, dst);
    /* copy keys */
    for(i = 0; i < BT_KEY_NUM(pg); i++) bt_buffer_key(pg, src, dst);

	/* copy chunks, deleting object from needed position */
    src = BT_CHNK_TAB(pg);
    for (i = 0; i < BT_KEY_NUM(pg); i++)
    {
        bt_buffer_chnk(pg, src, dst);
        if (i == key_idx)
        {
            /* delete object */
            /* heap pointer is at the begining of the target chunk now */
            heap_shft = bt_buffer_heap_shft();
            char* chnk_tab_slot = BT_CHNK_TAB_AT(buf, key_idx);
            char* area_begin = buf+ heap_shft;
            char* area_end = buf + heap_shft + obj_idx * sizeof(object);
            shft  area_size = (shft)(area_end - area_begin);
            /* delete object in needed position */
            memcpy(delete_buf, area_begin, area_size);
            memcpy(area_begin + sizeof(object), delete_buf, area_size);
            bt_buffer_heap_shft_inc(sizeof(object));
            /* update chunk table slot correspondingly */
            *(shft*)chnk_tab_slot += sizeof(object);
            *((shft*)chnk_tab_slot + 1) -= 1;
        }
    }
    /* copy buffers to the pages and actualize headers */
    memcpy(pg, buf, PAGE_SIZE);
    (*BT_HEAP_PTR(pg)) = bt_buffer_heap_shft();

    VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));
}

/* */
void bt_leaf_delete_key(char* pg, shft key_idx)
{
    shft    key_size = BT_KEY_SIZE(pg);
    shft    key_tab_slot_size;
    char*   dst = bt_tune_buffering(true, key_size);
    char*   buf = dst;
    char*   src = BT_KEY_TAB(pg);
    shft    heap_shft;
    int     i;

    /* set the key table slot size */
    if (!key_size)
        key_tab_slot_size = 2*sizeof(shft);
    else
        key_tab_slot_size = key_size;

    bt_buffer_header(pg, dst);
    /* copy all but 'key_idx'-th keys */
    for(i = 0; i < BT_KEY_NUM(pg); i++)
    {
        if (i == key_idx)
        { /* move to next key tab slot */
            src += key_tab_slot_size;
            continue;
        }
        bt_buffer_key(pg, src, dst);
    }
    /* copy all but 'key_idx'-th chunks */
    src = BT_CHNK_TAB(pg);
    for (i = 0; i < BT_KEY_NUM(pg); i++)
    {
        if (i == key_idx)
        { /* move to next chnk tab slot */
            src += 2 * sizeof(shft);
            continue;
        }
		bt_buffer_chnk(pg, src, dst);
	}
    /* copy buffers to the pages and actualize headers */
    memcpy(pg, buf, PAGE_SIZE);
    (*BT_HEAP_PTR(pg)) = bt_buffer_heap_shft();
    (*BT_KEY_NUM_PTR(pg)) -= 1;

    VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));
}
