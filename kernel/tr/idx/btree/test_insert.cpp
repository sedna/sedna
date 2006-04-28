/*
 * File:  test_insert.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>
#include "sedna.h"
#include "btintern.h"
#include "test.h"

using namespace std;


/*
->bt_find_split_key(char* pg, shft pretender_idx, shft pretender_size)
->CASE: leaf/non-leaf, fixed- variable-size keys;

   testing method: 
   1) create needed page
   2) define pretender size and idx
   3) try bt_find_split_key() with this page and this pretender 
   4) try various cases
*/
void test_bt_find_split_key() {
	char	pg[PAGE_SIZE];
	shft	split_key_idx;
	shft	first_half_size;	/* sizes of left- and right-hand halfs sizes correspondingly */
	shft	second_half_size;   /* accounting the pretender as well */
	shft	last_key_size_with_load_data_from_first_half;
	shft	pretender_size=18;
	shft	pretender_idx=17;
	bool	pretender_goes_left;

	bt_page_markup(pg, xs_string);
	/* leaf page with fixed-size keys */
	//helper_populate_page(pg, true, sizeof(int), 65300, 1);
	/* leaf page with var-size keys */
	//helper_populate_page(pg, true, 0, 65300, 1);
	/* non-leaf page with fixed-size keys */
	//helper_populate_page(pg, false, sizeof(int), 65300, 1);
	/* non-leaf page with var-size keys */
	helper_populate_page(pg, false, 0, 65300, 1);

	/* note: split_key_idx can not be 0 - this would mean that in the first half fall only pretender
	   and overpasses the half of payload in page - this is impossible because pretender_size can not
	   exceed half of payload
     */
	split_key_idx = bt_find_split_key(pg, pretender_idx, pretender_size, pretender_goes_left);
	first_half_size = helper_count_key_with_load_data_volume(pg, 0, split_key_idx-1);
	second_half_size = helper_count_key_with_load_data_volume(pg, split_key_idx, BT_KEY_NUM(pg)-1);
	if (pretender_goes_left)
		first_half_size+=pretender_size;
	else
		second_half_size+=pretender_size;
	if (pretender_idx == split_key_idx && pretender_goes_left)
		last_key_size_with_load_data_from_first_half=pretender_size;
	else
		last_key_size_with_load_data_from_first_half = helper_count_key_with_load_data_volume(pg, split_key_idx-1, split_key_idx-1);
	/* leaf page case */
	//helper_print_page(pg, true, true, false, false, false, false);
	/* non-leaf page case */
	helper_print_page(pg, true, true, false, true, false, false);
}

/*
->bt_page_split(char* pg, shft pretender_idx, shft pretender_size)
->CASE: leaf/non-leaf, fixed- variable-size keys;

   testing method: 
   1) create needed page
   2) define pretender size and idx
   3) try bt_page_split() with this page and this pretender 
   4) try various cases
*/
void test_bt_page_split() {
	char	pg[PAGE_SIZE];
	char	rpg_buf[PAGE_SIZE];
	xptr	rpg;
	rpg.addr = rpg_buf;

	shft	split_key_idx;
	shft	first_half_size;	/* sizes of left- and right-hand halfs sizes correspondingly */
	shft	second_half_size;   /* accounting the pretender as well */
	shft	last_key_size_with_load_data_from_first_half;
	shft	pretender_size=480;
	shft	pretender_idx=0;
	bool	pretender_goes_left;

	bt_page_markup(pg, xs_string);
	/* leaf page with fixed-size keys */
//	helper_populate_page(pg, true, sizeof(int), 65300, 1);
	/* leaf page with var-size keys */
	helper_populate_page(pg, true, 0, 8, 10);
	/* non-leaf page with var-size keys */
//	helper_populate_page(pg, false, sizeof(int), 65300, 1);
	/* non-leaf page with var-size keys */
//	helper_populate_page(pg, false, 0, 65300, 10);

	/* make first the testual calculation how splitting will occur */
	/* note: split_key_idx can not be 0 - this would mean that in the first half fall only pretender
	   and overpasses the half of payload in page - this is impossible because pretender_size can not
	   exceed half of payload
     */
	split_key_idx = bt_find_split_key(pg, pretender_idx, pretender_size, pretender_goes_left);
	first_half_size = helper_count_key_with_load_data_volume(pg, 0, split_key_idx-1);
	second_half_size = helper_count_key_with_load_data_volume(pg, split_key_idx, BT_KEY_NUM(pg)-1);
	if (pretender_goes_left)
		first_half_size+=pretender_size;
	else
		second_half_size+=pretender_size;
	if (pretender_idx == split_key_idx && pretender_goes_left)
		last_key_size_with_load_data_from_first_half=pretender_size;
	else
		last_key_size_with_load_data_from_first_half = helper_count_key_with_load_data_volume(pg, split_key_idx-1, split_key_idx-1);
	/* print original page (leaf page with fixed-key size) */
//	helper_print_page(pg, true, true, true, false, true, true);
	/* print original page (leaf page with var-key size) */
	helper_print_page(pg, true, true, true, false, true, true);
	/* print original page (non-leaf page with fixed-key size) */
//	helper_print_page(pg, true, true, true, true, false, false);
	/* print original page (non-leaf page with var-key size) */
//	helper_print_page(pg, true, true, true, true, false, false);

	cout << endl << "splitting key idx:" << split_key_idx << endl << endl;

	/* actually split the page */
	bt_page_split(pg, rpg, pretender_idx, pretender_size);

	/* print half (leaf page with fixed-key size) - the result of splitting */
//	helper_print_page(pg, true, true, true, false, true, true);
//	helper_print_page((char*)rpg.addr, true, true, true, false, true, true);
	/* print half (leaf page with var-key size) - the result of splitting */
	helper_print_page(pg, true, true, true, false, true, true);
	helper_print_page((char*)rpg.addr, true, true, true, false, true, true);
	/* print half (non-leaf page with fixed-key size) - the result of splitting */
//	helper_print_page(pg, true, true, true, true, false, false);
//	helper_print_page((char*)rpg.addr, true, true, true, true, false, false);
	/* print half (non-leaf page with fixed-key size) - the result of splitting */
//	helper_print_page(pg, true, true, true, true, false, false);
//	helper_print_page((char*)rpg.addr, true, true, true, true, false, false);
}


/*
->bt_leaf_do_insert(char* pg, shft key_idx, bt_key* key, object obj)
->bt_nleaf_do_insert_key(char* pg, shft key_idx, bt_key* key, xptr big_ptr)
->bt_do_insert_obj(pg, obj_key_idx, obj, obj_idx)
->CASE: leaf/non-leaf, fixed- variable-size keys;

   testing method: 
   1) create needed page
   2) create key & object to insert
   3)do insert the key into certain position
*/
void test_bt_do_insert() {
	char	pg[PAGE_SIZE];

	/* string key */
	bt_key	key1;
	key1.type = xs_string;
	key1.size = 10;
	key1.head = "abcdefghij";
		
	/* integer key */
	bt_key	key2;
	key2.type = xs_integer;
	key2.size = 4;
	key2.head = (char*)malloc(sizeof(int));
	*(int*)key2.head = 5555555;

	shft	key_idx=10;
	shft	obj_key_idx=9;
	object	obj;
	obj.layer=0;
	obj.addr=(void*)0x10101010;
	shft	obj_idx=2;
	xptr	bigptr=obj;

	bt_page_markup(pg, xs_string);
	/* leaf page with fixed-size keys */
//	helper_populate_page(pg, true, sizeof(int), 10, 5);
	/* leaf page with var-size keys */
	helper_populate_page(pg, true, 0, 10, 10);
	/* non-leaf page with var-size keys */
//	helper_populate_page(pg, false, sizeof(int), 10, 1);
	/* non-leaf page with var-size keys */
//	helper_populate_page(pg, false, 0, 10, 10);

	/* print original page (leaf page with fixed-key size) */
//	helper_print_page(pg, true, true, true, false, true, true);
	/* print original page (leaf page with var-key size) */
	helper_print_page(pg, true, true, true, false, true, true);
	/* print original page (non-leaf page with fixed-key size) */
//	helper_print_page(pg, true, true, true, true, false, false);
	/* print original page (non-leaf page with var-key size) */
//	helper_print_page(pg, true, true, true, true, false, false);

	/* leaf page with fixed-size key */
//	bt_leaf_do_insert_key(pg, key_idx, &key2, obj);
	/* leaf page var-size key */
	bt_leaf_do_insert_key(pg, key_idx, &key1, obj);
	/* non-leaf page with fixed-size key */
//	bt_nleaf_do_insert_key(pg, key_idx, &key2, bigptr);
	/* non-leaf page var-size key */
//	bt_nleaf_do_insert_key(pg, key_idx, &key1, bigptr);

	cout << endl;
	/* print modified page (leaf page with fixed-key size) */
//	helper_print_page(pg, true, true, true, false, true, true);
	/* print modified page (leaf page with var-key size) */
	helper_print_page(pg, true, true, true, false, true, true);
	/* print modified page (non-leaf page with fixed-key size) */
//	helper_print_page(pg, true, true, true, true, false, false);
	/* print modified page (non-leaf page with var-key size) */
//	helper_print_page(pg, true, true, true, true, false, false);

	bt_do_insert_obj(pg, obj_key_idx, obj, obj_idx);

	/* print modified page (leaf page with fixed-key size) */
//	helper_print_page(pg, true, true, true, false, true, true);
	/* print modified page (leaf page with var-key size) */
	helper_print_page(pg, true, true, true, false, true, true);
}

/*
->bt_key(char* pg, shft key_idx) (constructor upon a page)
->CASE: leaf, variable-size keys;

   testing method: 
   1) create page
   2) create key out of the page
   3) print out createde key
*/
void test_bt_create_key() {
	char	pg[PAGE_SIZE];
	shft	key_idx=5;
	char	key_content[PAGE_SIZE];

	bt_page_markup(pg, xs_string);

	/* leaf page with fixed-size keys */
//	helper_populate_page(pg, true, sizeof(int), 10, 5);
	/* leaf page with var-size keys */
	helper_populate_page(pg, true, 0, 10, 10);

	/* print original page (leaf page with fixed-key size) */
//	helper_print_page(pg, true, true, true, false, true, true);
	/* print original page (leaf page with var-key size) */
	helper_print_page(pg, true, true, true, false, true, true);
	cout << endl;
	
	bt_key key(pg, key_idx, false);
	memcpy(key_content, key.head, key.size);
	key_content[key.size]='\0';
	cout << "key.type=" << key.type << ", key.size=" << key.size << ", key.head=" << key_content << endl;
}

/*
->bt_page_clusterize(char* pg, xptr rpg, object obj)
->CASE: leaf, variable-size keys;

   testing method: 
   1) create page with one key
   2) create key out of the page
   3) print out createde key
*/
void test_bt_page_clusterize() {
	xptr	root;
	char	pg[PAGE_SIZE];
	xptr	pg_xptr;
	pg_xptr.layer=0;
	pg_xptr.addr=pg;

	char	rpg[PAGE_SIZE];
	xptr	rpg_xptr;
	rpg_xptr.layer=0;
	rpg_xptr.addr=rpg;

	char	next_for_pg[PAGE_SIZE];
	xptr	next_for_pg_xptr;
	next_for_pg_xptr.layer=0;
	next_for_pg_xptr.addr=next_for_pg;

	char	prev_for_pg[PAGE_SIZE];
	xptr	prev_for_pg_xptr;
	prev_for_pg_xptr.layer=0;
	prev_for_pg_xptr.addr=(void*)prev_for_pg;

	xptr	next_for_next_for_pg_xptr;
	next_for_next_for_pg_xptr.layer=0;
	next_for_next_for_pg_xptr.addr=(void*)0x22222222;

	object	obj;
	obj.layer=0;
	obj.addr=(void*)0x10101010;


	bt_page_markup(pg, xs_string);
	bt_page_markup(next_for_pg, xs_string);
	bt_page_markup(prev_for_pg, xs_string);

	/* leaf page with fixed-size keys */
//	helper_populate_page(pg, true, sizeof(int), 10, 5);
	/* leaf page with var-size keys */
	helper_populate_page(pg, true, 0, 1, 1000);

	/* make a cluster page */
	(*BT_IS_CLUS_PTR(pg))=true;
	(*BT_IS_CLUS_TAIL_PTR(pg))=true;
	//(*BT_IS_CLUS_HEAD_PTR(pg))=true;

	/* set neighboring pointers */
	(*BT_NEXT_PTR(pg)) = next_for_pg_xptr;
	(*BT_PREV_PTR(pg)) = prev_for_pg_xptr;
	(*BT_NEXT_PTR(next_for_pg)) = next_for_next_for_pg_xptr;
	(*BT_PREV_PTR(next_for_pg)) = pg_xptr;
	(*BT_NEXT_PTR(prev_for_pg)) = pg_xptr;

	/* print original page (leaf page with fixed-key size) */
//	helper_print_page(pg, true, true, true, false, true, true);
	/* print original page (leaf page with var-key size) */
	helper_print_page(pg, true, true, true, false, true, true);
	cout << endl;

	bt_page_clusterize(root, pg, rpg_xptr, obj);

	helper_print_page(pg, true, true, true, false, true, true);
	cout << endl;
	helper_print_page(rpg, true, true, true, false, true, true);
	cout << endl;
	helper_print_page(next_for_pg, true, true, true, false, true, true);
}

//main() {
	//test_bt_page_split();
	//test_bt_do_insert();
	//test_bt_create_key();
//	test_bt_page_clusterize();
//}