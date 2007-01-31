/*
 * File:  test_helpers.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>
#include "common/sedna.h"
#include "tr/idx/btree/btintern.h"
#include <time.h>

using namespace std;

/* test helper functions */

/* given a seed string, randomly extends it to constitute lexicographically bigger string, adding
   randomly computed number of characters to the end of the seed;
   max_increase designates how many characters can be maximally added;
   str_size is changed accordingly
 */
void helper_stuff_string(char* str, shft & str_size, shft max_increase) {
	if (str_size <= 0)
        throw USER_EXCEPTION2(SE1008, "str must be not empty");
	/* make seed variable :) */
	srand((unsigned)time( NULL )+str_size);
	char*	ptr=str+str_size;

	/* get random stuff_size */
	shft	stuff_size = 1 + rand()%(max_increase);
	/* add random stuff_size characters to original string */
	for (int i=0; i<stuff_size; i++) {
		char a_code=97;
		char z_code=122;
		/* allow chars in diaposon a-z */
		char next = (char)(a_code + rand()%(z_code-a_code+1));
		*ptr=next;
		ptr++;
	}
	str_size+=stuff_size;
}

/* randomly populate the page with key_num keys; key_size gives size of keys (only int size supported),
   if null - random string var-size keys are generated;
   is_leaf defines whether to create leaf or non-leaf page;
   for non-leaf page bigptr table is stuffed with single value; 
   for leaf page the chunks of ordered objects are created, each chunk no more than max_chunk_size;
   if needed to fill the page up to the end of free space, just set the key_num parameter very big;
   the function assumes the page is initially markuped with bt_page_markup()
 */
void helper_populate_page(char* pg, bool is_leaf, shft key_size, shft key_num, shft max_chunk_size) {
	/* maximum joint space occupied by all payload tables together with load data */
	shft	max_space=BT_PAGE_PAYLOAD;	
	shft	sum_size=0;
	shft	key_with_load_size;
	shft	space_count=0;
	shft	key_count=0;
	int		int_key=48;		/* key iterator for fixed-size integer keys */
	shft	char_key_size=4;
	char	char_key[PAGE_SIZE];
	memcpy(char_key, "seed", 4);
	shft	obj_num;
	shft	obj_nums[100000];
	char*	tmp;
	xptr	bigptr;			/* stuff all with single bigptr value */
	bigptr.layer=0;
	bigptr.addr=(void*)0x12345678;
	xptr	obj;			/* stuff object (increasing by 1 addr field) */
	obj.layer=0;
	obj.addr=(void*)0x00000001;
	
	/* prepare left-most key for fixed-size keys if the case */
	if (key_size) {
		if (key_size != sizeof(int))
            throw USER_EXCEPTION2(SE1008, "only integer fixed key size supported");
	}

	/* populate keys */
	for (key_count=0; key_count < key_num; key_count++) {
		key_with_load_size = key_size?key_size:char_key_size+2*sizeof(shft);
		if (is_leaf) {
			/* at least 1 object in chunk ! */
			obj_num = 1+ rand()%(max_chunk_size);
			obj_nums[key_count]=obj_num;
			key_with_load_size += obj_num*sizeof(object) + 2*sizeof(shft);
		} else 
			key_with_load_size += sizeof(xptr);
		sum_size += key_with_load_size;

		if (sum_size >= max_space)
			break;
		if (key_size) {
			/* fixed-size keys */
			(*(int*)BT_KEY_TAB_AT(pg, key_count)) = int_key;
			int_key++;
		} else {
			(*BT_HEAP_PTR(pg))-=char_key_size;
			memcpy(pg + BT_HEAP(pg), char_key, char_key_size);
			tmp = BT_KEY_TAB_AT(pg, key_count);
			*(shft*)tmp=BT_HEAP(pg);
			*((shft*)tmp+1)=char_key_size;
			helper_stuff_string(char_key, char_key_size,3);
		}
		(*BT_KEY_NUM_PTR(pg))+=1;
	}
	key_num=key_count;

	/* populate objects/bigptrs */
	for (key_count=0; key_count < key_num; key_count++) {
		if (is_leaf) {
			(*BT_HEAP_PTR(pg))-=obj_nums[key_count]*sizeof(object);
			for (int i=0; i< obj_nums[key_count]; i++) {
				shft heap = BT_HEAP(pg);
				*(object*)(pg + BT_HEAP(pg) + sizeof(object)*i)=obj;
				obj.addr=((char*)(obj.addr))+1;
			}
			tmp = BT_CHNK_TAB_AT(pg, key_count);
			*(shft*)tmp=BT_HEAP(pg);
			*((shft*)tmp+1)=obj_nums[key_count];
		} else {
			*(xptr*)BT_BIGPTR_TAB_AT(pg, key_count)=bigptr;
		}
	}

	/* set header fields */
	if (!is_leaf)
		(*BT_IS_LEAF_PTR(pg))=false;
	(*BT_KEY_SIZE_PTR(pg)) = key_size;
	if (key_size) 
		(*BT_KEY_TYPE_PTR(pg)) = xs_integer;
	else
		(*BT_KEY_TYPE_PTR(pg)) = xs_string;

}

/* calculate how many bytes occupy keys with indexes [start_key_idx; end_key_idx] with all their
   load data
 */
shft helper_count_key_with_load_data_volume(char* pg, shft start_key_idx, shft end_key_idx) {
	bool is_leaf = BT_IS_LEAF(pg);
	shft key_size = BT_KEY_SIZE(pg);
	shft result=0;
	for (int i=start_key_idx; i<=end_key_idx; i++) {
		if (key_size)
			result +=key_size;
		else 
			result +=2*sizeof(shft)+*((shft*)BT_KEY_TAB_AT(pg, i)+1);
		if (is_leaf)
			result +=2*sizeof(shft)+sizeof(object)*(*((shft*)BT_CHNK_TAB_AT(pg, i)+1));
		else
			result +=sizeof(xptr);
	}
	return result;
}

/* prints page contents, depending on values of driving flags: header, tab_key, key_contents,
   tab_bigptr, tab_chnk, chunks
 */
void helper_print_page(char* pg, bool header, bool tab_key, bool key_contents, bool  tab_bigptr, bool tab_chnk, bool chunks) {
	char	buf[PAGE_SIZE];
	shft	key_size;
	shft	key_shft;
	shft	chnk_size;
	shft	chnk_shft;
	shft	tabs_volume=0;
	if (header) {
		cout << "page:("<< (void*)pg << ")" << endl;
		cout << "~~~~~~~~ header ~~~~~~~~~~~~" << endl;
		cout << "next page:" << XADDR(BT_NEXT(pg)) << endl;
		cout << "prev page:" << XADDR(BT_PREV(pg)) << endl;
		cout << "parent page:" << XADDR(BT_PARENT(pg)) << endl;
		cout << "leftmost page:" << XADDR(BT_LMP(pg)) << endl;
		cout << "is leaf:" << BT_IS_LEAF(pg) << endl;
		cout << "is cluster:" << BT_IS_CLUS(pg) << endl;
		cout << "is cluster head:" << BT_IS_CLUS_HEAD(pg) << endl;
		cout << "is cluster tail:" << BT_IS_CLUS_TAIL(pg) << endl;
		cout << "key type:" << BT_KEY_TYPE(pg) << endl;
		cout << "key size:" << BT_KEY_SIZE(pg) << endl;
		cout << "key num:" << BT_KEY_NUM(pg) << endl;
		cout << "heap shft:" << BT_HEAP(pg) << endl;
		cout << "header volume(fixed):" << BT_HSIZE << endl;
		if (BT_KEY_SIZE(pg))
			tabs_volume=BT_KEY_NUM(pg)*BT_KEY_SIZE(pg);
		else
			tabs_volume=BT_KEY_NUM(pg)*2*sizeof(shft);
		if (BT_IS_LEAF(pg))
			tabs_volume += BT_KEY_NUM(pg)*2*sizeof(shft);
		else
			tabs_volume += BT_KEY_NUM(pg)*sizeof(object);
		cout << "tabs volume:" << tabs_volume << endl;
	}
	if (tab_key) {
		cout << "~~~~~~~~ key table ~~~~~~~~~~~~" << endl;
		for (int i=0; i<BT_KEY_NUM(pg); i++) {
			if (BT_KEY_SIZE(pg))
				cout << "key_slot[" << i << "]:" << *(int*)BT_KEY_TAB_AT(pg, i) << endl;
			else {
				key_size = *(((shft*)BT_KEY_TAB_AT(pg, i))+1);
				key_shft = *(shft*)BT_KEY_TAB_AT(pg, i);
				if (key_contents) {
					memcpy(buf, pg + key_shft, key_size);
					buf[key_size]='\0';
					cout << "key_slot[" << i << "]:(" << key_shft << "," << key_size << ")=" << buf << endl;
				} else
					cout << "key_slot[" << i << "]:(" << key_shft << "," << key_size << ")" << endl;
			}
		}
	}
	if (tab_bigptr) {
		cout << "~~~~~~~~ bigptr table ~~~~~~~~~~~~" << endl;
		for (int i=0; i<BT_KEY_NUM(pg); i++) {
			xptr bigptr=*(xptr*)BT_BIGPTR_TAB_AT(pg, i);
			cout << "bigptr_slot[" << i << "]:(" << bigptr.layer << "," << bigptr.addr << ")" << endl;
		}
	}
	if (tab_chnk) {
		cout << "~~~~~~~~ chnk table ~~~~~~~~~~~~" << endl;
		for (int i=0; i<BT_KEY_NUM(pg); i++) {
			chnk_size = *(((shft*)BT_CHNK_TAB_AT(pg, i))+1);
			chnk_shft = *(shft*)BT_CHNK_TAB_AT(pg, i);
			if (chunks) {
				cout << "chnk_slot[" << i << "]:(" << chnk_shft << "," << chnk_size << ")=" <<endl;
				for (int j=0; j< chnk_size; j++) {
					object obj = *(object*)(pg + chnk_shft + j*sizeof(object));
					cout << "\t\t{" << obj.layer << "," << obj.addr << "}" << endl;
				}
			} else 
				cout << "chnk_slot[" << i << "]:(" << chnk_shft << "," << chnk_size << ")" <<endl;
		}
	}
}

void helper_spool_page(char* pg) {
	FILE*	f=fopen("spool", "w+b");
	int		bytes;
	bytes = fwrite(pg, 1, PAGE_SIZE, f);
	if (bytes != PAGE_SIZE)
        throw USER_EXCEPTION2(SE1008, "Not all pages spooled to disk");
	fclose(f);
}

void helper_read_page(char* pg) {
	FILE*	f=fopen("spool", "r+b");
	int bytes=0;
	while(bytes < PAGE_SIZE) {
		int tmp = fread(pg+bytes, 1, PAGE_SIZE-bytes, f);
		if (tmp <= 0)
            throw USER_EXCEPTION2(SE1008, "Error reading from spool file");
		bytes += tmp;
	}
	fclose(f);
}

void helper_print_spool() {
	char	pg[PAGE_SIZE];
	helper_read_page(pg);

	/* print page (leaf page with fixed-key size) */
//	helper_print_page(pg, true, true, true, false, true, true);
	/* print page (leaf page with var-key size) */
	helper_print_page(pg, true, true, true, false, true, true);

}
