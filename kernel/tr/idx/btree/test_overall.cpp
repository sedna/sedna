/*
 * File:  test_overall.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>
#include <exception>
#include <string>

#include "common/sedna.h"

#include "common/base.h"
#include "common/SSMMsg.h"
#include "tr/vmm/vmm.h"
#include "tr/structures/indirection.h"
#include "common/ph/pers_heap.h"
#include "common/persistent_db_data.h"
#include "tr/structures/nodes.h"

#include <stdlib.h>
#include <time.h>
#include "tr/idx/btree/btintern.h"
#include "tr/idx/btree/btree.h"
#include "tr/idx/btree/test.h"

using namespace std;

extern shft BTREE_HEIGHT;

/* generates random null-terminated string not exceeding given size in given (big enough) buffer */
void generate_random_string(char seed, char* str, shft max_size) {
	if (max_size <= 0)
		throw IDXException("[generate_random_string()] max_size must be not empty");
	char*	ptr=str;

	/* get random str_size */
	shft	str_size = 1 + rand()%(max_size);
	/* add random str_size characters to target string */
	for (int i=0; i<str_size; i++) {
		char a_code=97;
		char z_code=122;
		/* allow chars in diaposon a-z */
		char next = (char)(a_code + rand()%(z_code-a_code+1));
		*ptr=next;
		ptr++;
	}
	*(str+str_size)='\0';
}

/* generate random object with layer=0 and addr value not exceeding max_value */
object generate_random_object(int seed, int max_value) {
	/* make seed variable :) */
	object result;
	result.layer = 0;
	result.addr = (void*)(1 + rand()%(max_value));
	return result;
}

/* randomly populates btree with variable-size string keys */
void test_populate_btree_string(xptr &root) {
	char	buf[PAGE_SIZE];
	char	dummy[10];
	bt_key	key;
	object	obj;
	shft	max_key_size=1000;
	int		key_num = 100000;
	shft	max_obj_value=10000;
	char	key_seed='a';
	char	prev_key_seed='a';
	int		obj_seed=1;
	int		prev_obj_seed=1;
	int		exceptions_num=0;
	int		tmp;

	/* make seed variable :) */
	srand((unsigned)time( NULL ));

	key.type=xs_string;
	key.head=se_new char[max_key_size+1];
	for (int i=0; i<key_num; i++) {
		/* gen key */
		generate_random_string(key_seed, buf, max_key_size);
		key.size = strlen(buf);
		strcpy(key.head, buf);
		prev_key_seed=key_seed;
		for (int j=0; j<key.size; j++)
			tmp += buf[i];
		key_seed=tmp/23;
		if (key_seed == prev_key_seed)
			key_seed = (key_seed^2 + 1)/25;
		/* gen obj */
		obj = generate_random_object(obj_seed, max_obj_value);
		prev_obj_seed=obj_seed;
		obj_seed = (int)obj.addr;
		if (obj_seed == prev_obj_seed)
			obj_seed = (obj_seed^2 + 1)/587;

		/* insert key/obj pair in btree */
		cout << "inserting key/obj[:" << (i+1) << "]" << endl;
		cout << buf << endl;
		cout << obj.addr << endl;
		cout << "-------------------------------------------------------------" << endl;
		try {
				bt_insert(root, &key, obj);
		} catch (IDXException e) {
			cout << "catched exception:" << e.getMsg() << endl;
			exceptions_num++;
			//gets(dummy);
		}
	}
	cout << "Done! With " << exceptions_num << " exceptions. " << "Resultant btree height:" << BTREE_HEIGHT << endl;
	gets(dummy);
CHECKP(root);
	//helper_print_page((char*)XADDR(root), true, true, true, false, true, true);
}

/* populates btree with variable-size string keys taken from word dictionary */
void test_populate_btree_dictionary(xptr & root) {
	char	buf[PAGE_SIZE];
	char	dummy[10];
	bt_key	key;
	key.type=xs_string;
	object	obj;
	obj.layer=0;
	obj.addr=(void*)0x00000001;
	int		tmp;
	FILE*	f;
	char	ch;
	key.head = se_new char[1024];
	int		i=1;
	int		exceptions_num=0;
	char	tmpc[10];

	srand((unsigned)time( NULL ));
	f = fopen("c:/home/charisma/kernel/idx/btree/words3", "r");
    while((ch = fgetc(f)) != EOF) {
		obj = generate_random_object(0, 0x0fffffff);
		if(ch == '\n') {
			/* for display */
			key.head[key.size]='\0';
			/* insert key/obj pair in btree */
			cout << "inserting key/obj[:" << (i) << "]" << endl;
			cout << key.head << endl;
			cout << obj.addr << endl;
			cout << "-------------------------------------------------------------" << endl;
			try {
				bt_insert(root, &key, obj);
			} catch (IDXException e) {
				cout << "catched exception:" << e.getMsg() << endl;
				exceptions_num++;
				//gets(dummy);
			}
			/* reset key, prepare next obj value */
			key.size=0;
			//obj.addr=(void*)((int)obj.addr+1);
			i++;
		} else {
			key.head[key.size]=ch;
			key.size++;
		}
	}
	fclose(f);
	cout << "Done! With " << exceptions_num << " exceptions. " << "Resultant btree height:" << BTREE_HEIGHT << endl;
	//gets(dummy);
}

/* print out all keys from left to right direction from given btree */
void test_print_btree_keys(xptr root) {
	bt_key*		key;
	char		buf[PAGE_SIZE];
	/* get cursor on the left-most key */
	xptr	cur_xpg = root;
	char*	cur_pg = (char*)XADDR(root);
	while (!BT_IS_LEAF(cur_pg)) {
		//helper_print_page(cur_pg, true, false, false, false, false, false);
		if (BT_LMP(cur_pg) == XNULL)
			throw IDXException("[test_print_btree_keys()] enecountered XNULL LMP field when trying to get left-most path in btree");
		cur_xpg=BT_LMP(cur_pg);
CHECKP(cur_xpg);
		cur_pg = (char*)XADDR(cur_xpg);
	}
	bt_cursor c(cur_pg, 0);
	//helper_print_page(cur_pg, true, true, true, false, false, false);
	while(c.bt_next_key()) {
		memcpy(buf, c.key.head, c.key.size);
		buf[c.key.size]='\0';
		cout << buf << endl;
	}
}

/* find a definite key in btree */
void test_find_btree_key(xptr root) {
	bt_key		key;
	char		buf[1024];
	key.type=xs_string;
	key.size=7;
	key.head = se_new char[7];
	memcpy(key.head, "abdomen", 7);
	bt_cursor	c=bt_find(root, &key);
	if (c.bt_next_key()) {
		memcpy(buf, c.key.head, c.key.size);
		buf[c.key.size]='\0';
		cout << "found key:" << buf << endl;
	} else 
		cout << "key not found" << endl;
}


/* find a definite key in btree and print out all objects of that key */
void test_print_key_objects(xptr root) {
	bt_key		key;
	char		buf[1024];
	object		obj;
	int			i=0;

	key.type=xs_string;
	key.size=7;
	key.head=se_new char[7];
	memcpy(key.head, "abdomen", 7);
	bt_cursor	c=bt_find(root, &key);
	if (!c.is_null()) {
		while ((obj = c.bt_next_obj()) != XNULL)
			cout << "object[" << i << "]=(" << obj.layer << "," << obj.addr << ")" << endl;
		cout << "-------------------------------------------------------------------" << endl;
	} else
		cout << "key not found" << endl;
}

void test_delete_key(xptr root) {
	bt_key		key;

	key.type=xs_string;
	key.size=7;
	key.head=se_new char[7];
	memcpy(key.head, "abdomen", 7);
	
	bt_delete(root, &key);
}

void test_delete_obj(xptr root) {
	bt_key		key;
	object		obj;
	obj.layer=0;
	obj.addr=(void*)0x00001456;

	key.type=xs_string;
	key.size=7;
	key.head=se_new char[7];
	memcpy(key.head, "abdomen", 7);
	
	bt_delete(root, &key, obj);
}


void test_main() {
/*	xptr root = bt_create(xs_string);
//	test_populate_btree_string(root);
	test_populate_btree_dictionary(root);
	test_populate_btree_dictionary(root);
	test_populate_btree_dictionary(root);
	cout << "root page:" << (void*)XADDR(root) << endl;
*/
	xptr root2;
	root2.layer=0;
	root2.addr=(void*)0x383d0000;
CHECKP(root2);
	test_print_key_objects(root2);
	cout << "--------------------------------" << endl;
	test_delete_obj(root2);
	cout << "--------------------------------" << endl;
	test_print_key_objects(root2);
	cout << "--------------------------------" << endl;
	test_delete_key(root2);
	cout << "--------------------------------" << endl;
	test_print_btree_keys(root2);
	cout << "--------------------------------" << endl;
//	test_find_btree_key(root2);
	test_print_key_objects(root2);

}

persistent_db_data* entry_point;

void main() {

	char *db_name = "xmark";
    d_printf1("TR STARTED\n");

	win32_system_exception::install_handler();

    int ret_code = 0;
    sm_msg_struct msg;
	try {

        string ssmmsg_sm_id = string(CHARISMA_SSMMSG_SM_ID) + string(db_name);
        SSMMsg sm_server(SSMMsg::Client, sizeof (sm_msg_struct), ssmmsg_sm_id.c_str(), SM_NUMBER_OF_SERVER_THREADS);

        d_printf1("Connecting to SM...");
        if (sm_server.init() != 0)
            throw CharismaException("????: Connection to SM failed");
        d_printf1("OK\n");

        d_printf1("Initializing PH...");
        string ph_shared_memory = string(CHARISMA_PH_SHARED_MEMORY_NAME) + string(db_name);
        string pers_heap_semaphore = string(PERS_HEAP_SEMAPHORE_STR) + string(db_name);
        if (0 != pers_init(ph_shared_memory.c_str(), pers_heap_semaphore.c_str(), PH_ADDRESS_SPACE_START_ADDR))
            throw CharismaException("????: Initialization of persistent heap failed");
        d_printf1("OK\n");


        d_printf1("Initializing VMM...");
        entry_point = vmm_init(&sm_server, db_name);
        d_printf1("OK\n");

		d_printf1("Initializing indirection table...");
        init_indirection_table(db_name);
        d_printf1("OK\n");

        d_printf1("Initializing metadata...");
#ifdef NO_PERSISTENCY
		//metainfo_ptr= se_new metadata_cell*;
		//*metainfo_ptr=NULL;
#endif 
        init_metadata(&(entry_point->metadata), db_name);
        d_printf1("OK\n");

		test_main();

		d_printf1("Releasing metadata...");
        release_indirection_table();
        d_printf1("OK\n");

        d_printf1("Releasing VMM...		");
        vmm_destroy();
        d_printf1("OK\n");

        d_printf1("Releasing PH... ");
        if (pers_release() != 0)
            throw CharismaException("????: Release of persistent heap failed");
        d_printf1("OK\n");

	} catch (CharismaException &e) {
        cout << "Charisma exception" << endl;
        cout << e.getMsg() << endl;
    } catch (exception &e) {
        cout << "Library catched" << endl;
        cout << e.what() << endl;
    }

}
