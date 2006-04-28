/*
 * File:  test_find.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>
#include "sedna.h"
#include "btintern.h"
#include "test.h"

using namespace std;


/*
->bt_leaf_find_obj(char* pg, xptr rpg, object obj)
->CASE: leaf, variable-size keys;

   testing method: 
   1) read spool page from file, knowing which object in which chunk exist in this page
   3) print out page
   4) find given key/object pair in the page
*/
void test_bt_leaf_find_obj() {
	char*	pg  = new char[PAGE_SIZE];
	xptr	xpg;
	xpg.layer=0;
	xpg.addr=pg;
	shft	key_idx=0;
	shft	obj_idx;
	object	obj;
	obj.layer=0;
	obj.addr=(void*)0x00000000;
	bool	rc;

	helper_read_page(pg);
	rc = bt_leaf_find_obj(xpg, obj, key_idx, obj_idx);
	cout << "object found=" << rc << " with index=" << obj_idx << " in page=" << (void*)pg << endl;
}

/*
->bt_leaf_find_key(char* & pg, bt_key* key, shft & key_idx)
->CASE: leaf, variable-size keys;
 */
void test_bt_leaf_find_key() {
	char*	pg  = new char[PAGE_SIZE];
	xptr	xpg;
	xpg.layer=0;
	xpg.addr=pg;
	shft	key_idx;
	bool	rc;
	bt_key	key;
	key.type=xs_string;
	key.size=4;
	key.head = "seem";

	helper_read_page(pg);
	rc = bt_leaf_find_key(xpg, &key, key_idx);
	cout << "key found=" << rc << " with index=" << key_idx << " in page=" << (void*)pg << endl;
}

/*
->bt_nleaf_find_key(char* & pg, bt_key key, shft & key_idx)
->CASE: non-leaf, variable-size keys;
 */
void test_bt_nleaf_find_key() {
	char*	pg  = new char[PAGE_SIZE];
	shft	key_idx;
	bool	rc;
	bt_key	key;
	key.type=xs_string;
	key.size=4;
	key.head = "seed";

	helper_read_page(pg);
	rc = bt_nleaf_find_key(pg, &key, key_idx);
	cout << "key found=" << rc << " with index=" << key_idx << " in page=" << (void*)pg << endl;
}

/* generate needed page and spool it into file */
void prepare_page() {
	char	pg[PAGE_SIZE];
	bt_page_markup(pg, xs_string);

	/* leaf page with fixed-size keys */
//	helper_populate_page(pg, true, sizeof(int), 10, 5);
	/* leaf page with var-size keys */
	helper_populate_page(pg, true, 0, 10, 10);

	/* print original page (leaf page with fixed-key size) */
//	helper_print_page(pg, true, true, true, false, true, true);
	/* print original page (leaf page with var-key size) */
	helper_print_page(pg, true, true, true, false, true, true);

	helper_spool_page(pg);
}



//void main() {
//	prepare_page();
//	helper_print_spool();
//	test_bt_leaf_find_obj();
//	test_bt_leaf_find_key();
//}