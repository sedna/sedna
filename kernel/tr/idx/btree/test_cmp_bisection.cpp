/*
 * File:  test_cmp_bisection.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "btintern.h"

/* 
->bool bt_locate_obj_bisection(object* ar, shft ar_size, object obj, shft &obj_idx)
->INCLUDE: int bt_cmp_obj(object o1, object o2)

   testing method: 
   1) create array of objects/keys of given size
   2) populate array
   3) search for given object/key
 */

void test_bt_locate_obj_bisection() {
	shft	ar_size=6;
	object	ar[6];
	bool	rc;
	/* object	obj = xptr(0, (void*)0x22222223); /* inside array case */
	/* object	obj = xptr(0, (void*)0x00000001);	/* left-most case */
	/* object	obj = xptr(0, (void*)0x77777777);	/* right-most case */
	/* object	obj = xptr(0, (void*)0x00000001);	/* left-most case */
	object obj = xptr(0, (void*)0x44444444);	/* hit case */

	shft	obj_idx;
	/* handy populate array */
	ar[0] = xptr(0, (void*)0x11111111);
	ar[1] = xptr(0, (void*)0x22222222);
	ar[2] = xptr(0, (void*)0x33333333);
	ar[3] = xptr(0, (void*)0x44444444);
	ar[4] = xptr(0, (void*)0x55555555);
	ar[5] = xptr(0, (void*)0x66666666);
	rc = bt_locate_obj_bisection(ar, ar_size, obj, obj_idx);
}

/*
->bool bt_locate_key_bisection(char* pg, char* ar, shft ar_size, shft ar_el_size, bt_key* key, shft &key_idx)
->CASE: fixed size keys (int)
->INCLUDE PART (fixed-size keys): int bt_cmp_key2(char* pg, void* tab_el, bt_key* k2)

   testing method: 
   1) create array of fixed-size keys of given size
   2) populate array
   3) search for given key
*/
void test_bt_locate_key_bisection() {
	shft	key_size = 4;
	shft	ar_size=6;
	char	ar[4*6];
	bt_key	key;
	shft	key_idx;
	bool	rc;

	key.head = (char*)malloc(key_size);
	key.size = key_size;
	key.type = xs_integer;
	*(int*)key.head = 2; /* various cases */

	shft	obj_idx;
	/* handy populate array */
	*(int*)ar = 2;
	*((int*)ar+1) = 4;
	*((int*)ar+2) = 6;
	*((int*)ar+3) = 7;
	*((int*)ar+4) = 9;
	*((int*)ar+5) = 10;
	rc = bt_locate_key_bisection(NULL, ar, ar_size, key_size, &key, key_idx);
}

/*
->int bt_cmp_key2(char* pg, void* tab_el, bt_key* k2)
->CASE: string keys

   testing method: 
   1) create variable-size key with contents, which live in emulated page
   2) create variable-size bt_key
   3) compare the two
*/
void test_bt_cmp_key2() {
	char	pg[PAGE_SIZE];
	shft	key_tab_slot_shft=345;
	char*	key_tab_slot = pg + key_tab_slot_shft;
	shft	key_contents_shft =4762;
	char*	key_contents="abcdefghij";
	shft	key_size=10;
	int	rc;

	bt_key key;
	key.head = "abcdefghiy";
	key.size = 10;
	key.type = xs_string;

	*(shft*)key_tab_slot=key_contents_shft;
	*((shft*)key_tab_slot+1)=key_size;
	memcpy(pg + key_contents_shft, key_contents, key_size);

	rc = bt_cmp_key(pg, key_tab_slot, key);
}

/*
->int bt_cmp_key(void* tab_el, bt_key* k2)
->CASE: string keys

   testing method: 
   2) create 2 variable-size bt_key
   3) compare the two
*/
void test_bt_cmp_key() {
	int	rc;

	bt_key key1;
	key1.head = "abcdefghia";
	key1.size = 10;
	key1.type = xs_string;

	bt_key key2;
	key2.head = "abcdefghi";
	key2.size = 9;
	key2.type = xs_string;

	rc = bt_cmp_key(key1, key2);
}

/* 
main() {
	test_bt_cmp_key();
}
*/
