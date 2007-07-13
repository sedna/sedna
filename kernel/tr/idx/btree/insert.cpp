/*
 * File:  insert.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/idx/btree/btintern.h"
#include "tr/idx/btree/btpage.h"
#include "tr/idx/btree/btstruct.h"
#include "tr/idx/btree/buff.h"
#include "tr/vmm/vmm.h"



/* temporary buffer used for performing page insert operations */
char insert_buf[PAGE_SIZE];
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
xptr bt_page_split(char* pg, const xptr &rpg, shft & pretender_idx, shft pretender_size)
{
    xptr    pg_xptr = ADDR2XPTR(pg);
	xptr    next_for_rpg = BT_NEXT(pg);
    bool    is_leaf_page = BT_IS_LEAF(pg);
    shft    key_size = BT_KEY_SIZE(pg);
    shft    key_num = BT_KEY_NUM(pg);
    bool    pretender_goes_left=true;	/* flag showing where pretender will fall */
    shft    split_idx = 0;              //bt_find_split_key(pg, pretender_idx, pretender_size, pretender_goes_left);
    char    *buf1 = NULL, *buf2 = NULL;
    shft    heap_buf1, heap_buf2;
    char    *dst = NULL, *src = NULL;
	int i;
    	
    if (next_for_rpg == XNULL ) 
    {
        split_idx = key_num - 1;
	    shft    volume = 0;
	    for (i = split_idx; i>-1; i--)
		{
			if (i==pretender_idx)
				pretender_goes_left=false;
			/* account key volume */
		    if (key_size) /* fixed-size keys */
        		volume += key_size;
			else
        	{
				volume += 2*sizeof(shft);
        		volume += *(((shft*)BT_KEY_TAB_AT(pg, i))+1);
			}
			if (volume>=pretender_size) break;
			split_idx--;
		}
    }
    else
    {
	    split_idx = bt_find_split_key(pg, pretender_idx, pretender_size, pretender_goes_left);
    }
    
    /* prepare the left-hand page */
    dst = bt_tune_buffering(true, key_size);
    buf1 = dst;
    src = BT_KEY_TAB(pg);
    bt_buffer_header(pg, dst);
    /* copy low keys */
    for(i = 0; i < split_idx; i++) bt_buffer_key(pg, src, dst);
	/* copy load data */
    if (is_leaf_page)
    {
        src = BT_CHNK_TAB(pg);
        for (i = 0; i < split_idx; i++) bt_buffer_chnk(pg, src, dst);
    }
    else
    {
        src = BT_BIGPTR_TAB(pg);
        for (i = 0; i < split_idx; i++) bt_buffer_bigptr(pg, src, dst);
    }
    heap_buf1 = bt_buffer_heap_shft();
	
	/* prepare the right-hand page */
    dst = bt_tune_buffering(false, key_size);
    buf2 = dst;
    src = BT_KEY_TAB_AT(pg, split_idx);
    bt_buffer_header(pg, dst);
    /* copy high keys */
    for(i = split_idx; i < key_num; i++) bt_buffer_key(pg, src, dst);
    /* copy load data */
    if (is_leaf_page)
    {
        src = BT_CHNK_TAB_AT(pg, split_idx);
        for (i = split_idx; i < key_num; i++) bt_buffer_chnk(pg, src, dst);
    }
    else 
    {
        src = BT_BIGPTR_TAB_AT(pg, split_idx);
        for (i = split_idx; i < key_num; i++) bt_buffer_bigptr(pg, src, dst);
    }
    heap_buf2 = bt_buffer_heap_shft();

    /* copy buffers to the pages and actualize headers */
    memcpy(pg, buf1, PAGE_SIZE);
	
    (*BT_NEXT_PTR(pg)) = rpg;
    (*BT_KEY_NUM_PTR(pg)) = split_idx;
    (*BT_HEAP_PTR(pg)) = heap_buf1;
    VMM_SIGNAL_MODIFICATION(pg_xptr);

    CHECKP(rpg);
    char* rpg_addr = (char*)XADDR(rpg);
    memcpy((char*)rpg_addr + sizeof(vmm_sm_blk_hdr), buf2 + sizeof(vmm_sm_blk_hdr), PAGE_SIZE - sizeof(vmm_sm_blk_hdr));
    (*BT_PREV_PTR(rpg_addr)) = pg_xptr;
    /* reset LMP field in right-hand page */
    (*BT_LMP_PTR(rpg_addr)) = XNULL;
    (*BT_KEY_NUM_PTR(rpg_addr)) = key_num - split_idx;
    (*BT_HEAP_PTR(rpg_addr)) = heap_buf2;
	(*BT_NEXT_PTR(rpg_addr)) = next_for_rpg;
	if (BT_IS_CLUS_HEAD(rpg_addr))
	{
		VMM_SIGNAL_MODIFICATION(rpg);
		CHECKP(pg_xptr);
		(*BT_IS_CLUS_PTR(pg))=false;
		(*BT_IS_CLUS_HEAD_PTR(pg))=false;
		(*BT_IS_CLUS_TAIL_PTR(pg))=false;    
		VMM_SIGNAL_MODIFICATION(pg_xptr);
	}
	else
	{
		(*BT_IS_CLUS_PTR(rpg_addr))=false;
		(*BT_IS_CLUS_HEAD_PTR(rpg_addr))=false;
		(*BT_IS_CLUS_TAIL_PTR(rpg_addr))=false;    
		VMM_SIGNAL_MODIFICATION(rpg);
	}
	if (next_for_rpg!=XNULL)
	{
		CHECKP(next_for_rpg);
		/* right <- */
		(*BT_PREV_PTR((char*)XADDR(next_for_rpg))) = rpg;
		VMM_SIGNAL_MODIFICATION(next_for_rpg);
	}

    /* clear out in which block pretender will reside and adjust new index of pretender in that page */
    if (pretender_goes_left)
        /* pretender index remains the same */
        return pg_xptr;
    else
    {
        /* pretender goes to the right page */
        pretender_idx = pretender_idx - split_idx;
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
shft bt_find_split_key(char* pg, shft pretender_idx, shft pretender_size, bool & pretender_goes_left)
{
    bool    is_leaf_page = BT_IS_LEAF(pg);
    shft    key_num = BT_KEY_NUM(pg);
    shft    key_size = BT_KEY_SIZE(pg);
    shft    volume = 0;					/* accumulates the total volume occupied by keys together with load */
    shft    border_volume = BT_PAGE_PAYLOAD / 2;
    pretender_goes_left = false;
	
    for (int i = 0; i < key_num; i++)
    {
        /* account pretender */
        if (i == pretender_idx)
        {
            pretender_goes_left = true;
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

void propagate_parent_to_cluster(xptr& head, xptr& parent)
{
	xptr tmp=head;
	char* blk=(char*)XADDR(head);
	CHECKP(head);
	while (true)
	{
		(*BT_PARENT_PTR(blk)) = parent;
         VMM_SIGNAL_MODIFICATION(tmp);
		tmp=BT_NEXT(blk);
if (tmp==XNULL) return;
		CHECKP(tmp);
		blk=(char*)XADDR(tmp);
		if (!BT_IS_CLUS(blk))
			return;		
	}
}

xptr bt_leaf_insert(xptr &root, char* pg, shft key_idx, bool create_new_key, const bt_key &key, const object &obj, shft obj_idx,bool with_bt)
{
    char*       key_pg = pg;
    xptr        key_pg_xptr = ADDR2XPTR(pg);
    xptr        pg_xptr = ADDR2XPTR(pg);
    xptr        rpg;						/* xptr of right page, originating in case of page splitting */
    bool        splitting_occurred = false;	/* flag showing if the splitting took place */
    xmlscm_type pg_type = BT_KEY_TYPE(pg);

    /* insert key/object into page */
    if (create_new_key)
    {
        shft key_with_load_size;
        /* account place for key and, new chunk slot and the chunk itself */
        if (BT_VARIABLE_KEY_TYPE(pg))
            /* key_content + slot in key tab + slot in chunk tab (for new chunk) + object */
            key_with_load_size = key.get_size() + 2 * sizeof(shft) + 2 * sizeof(shft) + sizeof(object);
        else
            /* key_content + slot in chunk tab (for new chunk) + object */
            key_with_load_size = key.get_size() + 2 * sizeof(shft) + sizeof(object);

        if (!bt_page_fit(pg, key_with_load_size))
        {
            vmm_alloc_data_block(&rpg);
            CHECKP(pg_xptr);
            xptr tmp = bt_page_split(pg, rpg, key_idx, key_with_load_size);

            CHECKP(tmp);
            key_pg = (char*)XADDR(tmp);
            key_pg_xptr = tmp;
            splitting_occurred = true;
        }

        bt_leaf_do_insert_key(key_pg, key_idx, key, obj);
    } 
    else
    { /* space required only for new object */
        if (!bt_page_fit(pg, sizeof(object)))
        {
            vmm_alloc_data_block(&rpg);
            CHECKP(pg_xptr);

            if (BT_KEY_NUM(pg) == 1)
            { /* cluster case - nothing is promoted, instantly return */
#ifdef PERMIT_CLUSTERS
                bt_page_clusterize(root, pg, rpg, obj,with_bt);
                return rpg;
#else
                throw USER_EXCEPTION2(SE1008, "Not enough space to insert new key/object into page (clusterization prohibited)");
#endif
            }
            /* not cluster case - page splitting */
            xptr tmp = bt_page_split(pg, rpg, key_idx, sizeof(object));

            CHECKP(tmp);
            key_pg = (char*)XADDR(tmp);
            key_pg_xptr = tmp;
            splitting_occurred = true;
        }

        bt_do_insert_obj(key_pg, key_idx, obj, obj_idx);
    }


    /* if splitting took place, promote splitting key (i.e. leftmost key in rpg page) to the parent page 
       note, if there is no parent block, new one is created an parent links of 'pg' and 'rpg' are made
       pointing to it */
    if (splitting_occurred)
    {
        CHECKP(rpg);
        xptr parent_pg = BT_PARENT((char*)XADDR(rpg));

        /* if this was the root page */
        if (parent_pg == XNULL)
        {
            vmm_alloc_data_block(&parent_pg);
            bt_page_markup((char*)XADDR(parent_pg), pg_type);
            /* make new root non-leaf and set the left-most pointer in new root page */
			get_clust_head(pg_xptr);
            (*BT_IS_LEAF_PTR((char*)XADDR(parent_pg))) = false;
            (*BT_LMP_PTR((char*)XADDR(parent_pg))) = pg_xptr;            
   			/* TEMP */
            CHECKP(pg_xptr);
            U_ASSERT(BT_PREV((char*)(XADDR(pg_xptr))) == XNULL);
			/* END */
            root = parent_pg;

			propagate_parent_to_cluster(pg_xptr,parent_pg);

            CHECKP(rpg);
            (*BT_PARENT_PTR((char*)XADDR(rpg))) = parent_pg;
            VMM_SIGNAL_MODIFICATION(rpg);
           // BTREE_HEIGHT++;
        }

        bt_promote_key(root, rpg, parent_pg,with_bt);
    }
    return key_pg_xptr;
}


/* attach new (rpg) page next to pg as cluster page. If the original pg page is not cluster yet, 
   mark it as cluster, i.e. form the cluster. The new page is initially unformatted. Insert object
   into new page
 */
void bt_page_clusterize(xptr &root, char* pg, const xptr &rpg, const object &obj,bool with_bt)
{
    xptr        pg_xptr = ADDR2XPTR(pg);
    xptr        next_for_rpg;
	xptr        pg_parent;
    xmlscm_type pg_type = BT_KEY_TYPE(pg);

    if (!BT_IS_LEAF(pg)) throw USER_EXCEPTION2(SE1008, "Attempt to clusterize non-leaf page");
    if (BT_KEY_NUM(pg) != 1) throw USER_EXCEPTION2(SE1008, "Number of keys in original page is not equal to 1");

    if (!BT_IS_CLUS(pg))
    {
        (*BT_IS_CLUS_PTR(pg)) = true;
        (*BT_IS_CLUS_HEAD_PTR(pg)) = true;
    } 
    else 
    {
        if (!BT_IS_CLUS_TAIL(pg) || BT_IS_CLUS_HEAD(pg))
            throw USER_EXCEPTION2(SE1008, "Trying to clusterize non-tail cluster page");
        (*BT_IS_CLUS_TAIL_PTR(pg)) = false;
    }

    next_for_rpg = BT_NEXT(pg);
	pg_parent = BT_PARENT(pg);
    /* left -> */
    (*BT_NEXT_PTR(pg)) = rpg;
    VMM_SIGNAL_MODIFICATION(pg_xptr);
    /* create cluster key */
    bt_key key(pg, 0);

    CHECKP(rpg);
    char* rpg_addr = (char*)XADDR(rpg);
    bt_page_markup(rpg_addr, pg_type);
    /* insert cluster key and new object into rpg page */
    bt_leaf_insert(root, rpg_addr, 0, true, key, obj, 0,with_bt);
    CHECKP(rpg);
    /* mark page as the cluster tail and link to neighbours */
    (*BT_IS_CLUS_PTR(rpg_addr)) = true;
    (*BT_IS_CLUS_TAIL_PTR(rpg_addr)) = true;
    /* left <- */
    (*BT_PREV_PTR(rpg_addr)) = pg_xptr;
    /* right -> */
    (*BT_NEXT_PTR(rpg_addr)) = next_for_rpg;
	(*BT_PARENT_PTR(rpg_addr)) = pg_parent;
    VMM_SIGNAL_MODIFICATION(rpg);
	if (next_for_rpg!=XNULL)
	{
		CHECKP(next_for_rpg);
		/* right <- */
		(*BT_PREV_PTR((char*)XADDR(next_for_rpg))) = rpg;
		VMM_SIGNAL_MODIFICATION(next_for_rpg);
	}
}

/* for given non-leaf page insert a given pair (key, big_ptr) 
   note: page splitting may occur inside that function, in case lack of enough space in original
   page. In that case the corresponding key is promoted up to the parent page, causing possibly
   recursive splitting of non-leaf parent pages;
   modifieable parameter 'root' gives xptr of the current btree root, which can be changed in case
   tree height is increased inside function;
*/
void bt_nleaf_insert(xptr &root, char* pg, const bt_key& key, const xptr &big_ptr,bool with_bt)
{
    char*       key_pg = pg;
    xptr        pg_xptr = ADDR2XPTR(pg);
    xptr        rpg;					/* xptr of right page, originating in case of page splitting */
    bool	    splitting_occur = false;/* flag showing if the splitting took place */
    bool        rc;
    shft        key_idx;
    shft        key_with_load_size;
    xmlscm_type pg_type = BT_KEY_TYPE(pg);

    /* find where to insert the key */
    rc = bt_nleaf_find_key(pg, (bt_key*)&key, key_idx,with_bt);
    if (rc)	throw USER_EXCEPTION2(SE1008, "The key to be inserted in non-leaf page is already there");

    if (key_idx == BT_RIGHTMOST) key_idx = BT_KEY_NUM(pg);
	
    /* account place for the key with load data */
    if (BT_VARIABLE_KEY_TYPE(pg))
        /* key content + slot in key tab + slot in bigptr tab */
        key_with_load_size = key.get_size() + 2 * sizeof(shft) + sizeof(xptr);
    else
        /* key content + slot in bigptr tab */
        key_with_load_size = key.get_size() + sizeof(xptr);

    if(!bt_page_fit(pg, key_with_load_size))
    {
        vmm_alloc_data_block(&rpg);

        CHECKP(pg_xptr);
        xptr tmp = bt_page_split(pg, rpg, key_idx, key_with_load_size);

        CHECKP(tmp);
        key_pg = (char*)XADDR(tmp);
        splitting_occur = true;
	}

    bt_nleaf_do_insert_key(key_pg, key_idx, key, big_ptr);

    /* if splitting took place, make changes of parent pointer in all child pages referenced from rpg;
       promote splitting key (i.e. leftmost key in rpg page) to the parent page note, if there is no 
       parent block, new one is created and parent links of 'pg' and 'rpg' are made pointing to it 
     */
    if (splitting_occur)
    {
        CHECKP(rpg);

        char* rpg_addr = (char*)XADDR(rpg);
        shft  rpg_key_num = BT_KEY_NUM(rpg_addr);
		/* note: no need to change LMP because it is always XNULL for rpg */
        for (int i = 0; i < rpg_key_num; i++)
        {
            xptr big_ptr = *(xptr*)BT_BIGPTR_TAB_AT(rpg_addr, i);
            /*CHECKP(big_ptr);
            (*BT_PARENT_PTR((char*)XADDR(big_ptr))) = rpg;
            VMM_SIGNAL_MODIFICATION(big_ptr);*/
			propagate_parent_to_cluster(big_ptr, rpg);
            CHECKP(rpg);
        }

        xptr parent_pg = BT_PARENT((char*)XADDR(rpg));
        /* if this was the root page */
        if (parent_pg == XNULL)
        {
            vmm_alloc_data_block(&parent_pg);
            bt_page_markup((char*)XADDR(parent_pg), pg_type);
            /* make new root non-leaf and set the left-most pointer in new root page */
            (*BT_IS_LEAF_PTR((char*)XADDR(parent_pg))) = false;
			get_clust_head(pg_xptr);
            (*BT_LMP_PTR((char*)XADDR(parent_pg))) = pg_xptr;
            /* TEMP */
            CHECKP(pg_xptr);
            U_ASSERT(BT_PREV((char*)(XADDR(pg_xptr))) == XNULL);
            /* END */
            root = parent_pg;

            /*CHECKP(pg_xptr);
            (*BT_PARENT_PTR(pg)) = parent_pg;
            VMM_SIGNAL_MODIFICATION(pg_xptr);*/
			propagate_parent_to_cluster(pg_xptr, parent_pg);
            CHECKP(rpg);
            (*BT_PARENT_PTR((char*)XADDR(rpg))) = parent_pg;
            VMM_SIGNAL_MODIFICATION(rpg);
            //BTREE_HEIGHT++;
        }

        bt_promote_key(root, rpg, parent_pg,with_bt);
    }
}

/* Takes the first key in given page and copies it to the parent page. In case the parent page is
   absent, i.e. this is the root page, create new root page and makes it parent;
   accordingly sets the big_ptr for promoted key to the given page;
   note: the function may cause possible recursive splitting of parent pages in case there
   is no enough space in them to store promoted keys
 */
void bt_promote_key(xptr &root, const xptr &pg, const xptr &parent_pg,bool with_bt)
{
    CHECKP(pg);
    bt_key promote_key((char*)XADDR(pg), 0);

    CHECKP(parent_pg);
    bt_nleaf_insert(root, (char*)XADDR(parent_pg), promote_key, pg,with_bt);
}

/* make actual insertion of key and object sticking to that key into given place in key table
   of leaf page; this function is used when new key is to be created in the page;
   no check for fittness
 */
void bt_leaf_do_insert_key(char* pg, shft key_idx, const bt_key& key, const object &obj)
{
    shft key_size = BT_KEY_SIZE(pg);
    /* area of page from key_idx position in key/chunk table up to the end of chunk table
       is to be shifted right */
    char*   area_begin = BT_KEY_TAB_AT(pg, key_idx);
    char*   area_end = BT_CHNK_TAB_AT(pg, BT_KEY_NUM(pg));
    shft    area_size = (shft)(area_end - area_begin);
    char*   ptr;
    char*   heap;

    /* insert new key in key table */
    if (key_size)
    { /* fixed-size key */
        memcpy(insert_buf, area_begin, area_size);
        memcpy(area_begin, key.data(), key_size);
        memcpy(area_begin + key_size, insert_buf, area_size);
    } 
    else
    {
        memcpy(insert_buf, area_begin, area_size);
        (*BT_HEAP_PTR(pg)) -= key.get_size();
        heap = pg + BT_HEAP(pg);
        memcpy(heap, key.data(), key.get_size());
        *(shft*)area_begin = BT_HEAP(pg);
        *((shft*)area_begin + 1) = key.get_size();
        memcpy(area_begin + 2 * sizeof(shft), insert_buf, area_size);
    }

   (*BT_KEY_NUM_PTR(pg)) += 1;

    /* insert new chunk in chunk table and new object; 
       as we are creating new chunk, we allocate it in the top of heap */
    area_begin = BT_CHNK_TAB_AT(pg, key_idx);
    area_end = BT_CHNK_TAB_AT(pg, BT_KEY_NUM(pg) - 1);
    area_size = (shft)(area_end - area_begin);
    memcpy(insert_buf, area_begin, area_size);
    (*BT_HEAP_PTR(pg)) -= sizeof(object);
    heap = pg + BT_HEAP(pg);
    *(object*)heap = obj;
    *(shft*)area_begin = BT_HEAP(pg);
    *((shft*)area_begin + 1) = 1;
    memcpy(area_begin + 2 * sizeof(shft), insert_buf, area_size);

    VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));
    /* update page header */
}

/* make actual insertion of key and big_ptr sticking to that key into given place in key table
   of non-leaf page; this function is used when new key is to be created in the page
   no check for fittness
 */
void bt_nleaf_do_insert_key(char* pg, shft key_idx, const bt_key& key, const xptr &big_ptr)
{
    shft	key_size = BT_KEY_SIZE(pg);
    /* area of page from key_idx position in key/bigptr table up to the end of bigptr table
       is to be shifted right */
    char*   area_begin = BT_KEY_TAB_AT(pg, key_idx);
    char*   area_end = BT_BIGPTR_TAB_AT(pg, BT_KEY_NUM(pg));
    shft    area_size = (shft)(area_end - area_begin);
    char*   ptr;
    char*   heap;

    /* insert new key in key table */
    if (key_size)
    { /* fixed-size key */
        memcpy(insert_buf, area_begin, area_size);
        memcpy(area_begin, key.data(), key_size);
        memcpy(area_begin + key_size, insert_buf, area_size);
    } 
    else
    {
        memcpy(insert_buf, area_begin, area_size);
        (*BT_HEAP_PTR(pg)) -= key.get_size();
        heap = pg + BT_HEAP(pg);
        memcpy(heap, key.data(), key.get_size());
        *(shft*)area_begin = BT_HEAP(pg);
        *((shft*)area_begin + 1) = key.get_size();
        memcpy(area_begin + 2 * sizeof(shft), insert_buf, area_size);
    }
    (*BT_KEY_NUM_PTR(pg)) += 1;

    /* insert new bigptr in bigptr table */
    area_begin = BT_BIGPTR_TAB_AT(pg, key_idx);
    area_end = BT_BIGPTR_TAB_AT(pg, BT_KEY_NUM(pg) - 1);
    area_size = (shft)(area_end - area_begin);
    memcpy(insert_buf, area_begin, area_size);
    *(xptr*)area_begin = big_ptr;
    memcpy(area_begin + sizeof(xptr), insert_buf, area_size);

    VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));	
	/* update page header */
}

/* make actual insertion of object into chunk of given key of leaf page;
   the key designated with index already exists in key table;
   no check for fittness
 */
void bt_do_insert_obj(char* pg, shft key_idx, const object &obj, shft obj_idx)
{
    shft    key_size = BT_KEY_SIZE(pg);
    char*   dst = bt_tune_buffering(true, key_size);
    char*   buf = dst;
    char*   src = BT_KEY_TAB(pg);
    shft    heap_shft;
    int     i;

    bt_buffer_header(pg, dst);

    /* copy keys */
    for(i = 0; i < BT_KEY_NUM(pg); i++) bt_buffer_key(pg, src, dst);

    /* copy chunks, inserting object into needed position */
    src = BT_CHNK_TAB(pg);
    for (i = 0; i < BT_KEY_NUM(pg); i++)
    {
        bt_buffer_chnk(pg, src, dst);
        if (i == key_idx)
        { /* insert object */
          /* heap pointer is at the begining of the target chunk now */
            heap_shft = bt_buffer_heap_shft();
            char*   chnk_tab_slot = BT_CHNK_TAB_AT(buf, key_idx);
            char*   area_begin = buf + heap_shft;
            char*   area_end = buf + heap_shft + obj_idx * sizeof(object);
            shft    area_size = (shft)(area_end - area_begin);
            /* insert object in needed position */
            memcpy(insert_buf, area_begin, area_size);
            area_end -= sizeof(object);
            *(object*)area_end = obj;
            memcpy(area_begin - sizeof(object), insert_buf, area_size);
            bt_buffer_heap_shft_dec(sizeof(object));
            /* update chunk table slot correspondingly */
            *(shft*)chnk_tab_slot -= sizeof(object);
            *((shft*)chnk_tab_slot + 1) += 1;
        }
    }

    /* copy buffers to the pages and actualize headers */
    memcpy(pg, buf, PAGE_SIZE);
    (*BT_HEAP_PTR(pg)) = bt_buffer_heap_shft();

    VMM_SIGNAL_MODIFICATION(ADDR2XPTR(pg));
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



