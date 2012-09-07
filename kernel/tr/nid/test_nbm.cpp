/*
 * File:  test_nbm.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
#include <iostream>
#include <exception>
#include "tr/nid/numb_scheme.h"
#include "common/persistent_db_data.h"
#include "tr/nid/nidalloc.h"
#include "tr/nid/lex.h"

#include "common/base.h"
#include "common/SSMMsg.h"
#include "tr/vmm/vmm.h"
#include "tr/structures/indirection.h"
#include "common/ph/pers_heap.h"

#include "tr/pstr/pstr.h"
#include <stdlib.h>
#include <time.h>

using namespace std;

/* bit_idx is enumerated starting from "0" value */
void set_nbm_bit(int bit_idx, bool bit_value) {
	unsigned char*	p;
	unsigned char	mask=0x80;
	if(!entry_point->nbm)
		throw NIDException("[set_nbm_bit()] nbm not initialized");
	p=(unsigned char*)entry_point->nbm + bit_idx/8;
	mask>>=(bit_idx%8);
	if (bit_value)
		*p|=mask;
	else {
		*p|=mask;	/* ensure the bit is raised */
		*p^=mask;	/* reset the bit */
	}
}

/* bit_idx is enumerated starting from "0" value */
bool get_nbm_bit(int bit_idx) {
	unsigned char*	p;
	unsigned char	frame;
	unsigned char	mask=0x80;
	if(!entry_point->nbm)
		throw NIDException("[set_nbm_bit()] nbm not initialized");
	p=(unsigned char*)entry_point->nbm + bit_idx/8;
	frame = *p;
	mask>>=(bit_idx%8);
	frame&=mask;
	return (bool)frame;
}

/* byte_idx is enumerated starting from "0" value */
unsigned char get_nbm_byte(int byte_idx) {
	unsigned char*	p;
	if(!entry_point->nbm)
		throw NIDException("[set_nbm_bit()] nbm not initialized");
	p=(unsigned char*)entry_point->nbm + byte_idx;
	return *p;
}

void print_nbm_bit(bool bit_value) {
	if (bit_value)
		cout << "1";
	else 
		cout << "0";
}

void print_nbm() {
	/*  output all bit-matrix */
	int bytes_in_layer = (ALPHABET_SIZE-2)/8;
	if ((ALPHABET_SIZE-2)%8)
			bytes_in_layer++;
	int i, j;
	for (i=1; i<=entry_point->nbm_size; i++) {
		/* scan ALPHABIT_SIZE-2 bits moving the 1-byte frame  over the bitmap.
		   last symbol iz reserved for extension */
		for (j=1; j<ALPHABET_SIZE-1; j++) {
			print_nbm_bit(get_nbm_bit((i-1)*bytes_in_layer*8 + j -1));
		}
		cout << endl;
	}
}

void test1() {
	int	num_bits=100;
	nid_extend_bitmap();
	for (int i=0; i<num_bits; i++) {
		bool val = (i%2)?false:true;
		set_nbm_bit(i, val);
	}
	print_nbm();
	cout << endl;
}

void test2() {
	int		num_bits=100;
	xptr	result;
	//nid_extend_bitmap();
	//print_nbm();
	for (int i=0; i<num_bits/2; i++) {
		nid_create_root(result,true);
		//print_nbm_bit(get_nbm_bit(i));
		//cout << endl;
	}
	print_nbm();
	cout << endl;
}

void test3() {
	int		num_bits=100;
	xptr	result;
	for (int i=0; i<num_bits; i++) {
		nid_create_root(result,true);
		cout << " ";
	}
}

/* to launch this test you need to uncomment debug lines in nid_delete()
extern t_prefix tp;
void test4() {
	int		num_bits=1000;
	xptr	result;
	tp.prefix = (char*)nid_alloc();
	for (int i=0; i<num_bits; i++) {
		tp.prefix[0]=i%(ALPHABET_SIZE-1) + 1;
		tp.size=1;
		for(int j=1; j<=i/(ALPHABET_SIZE-1); j++) {
			tp.prefix[j]=ALPHABET_SIZE;
			tp.size++;
		}
		//lex_print(tp);
		nid_delete(XNULL);
	}
	nid_free(tp.prefix);
}*/

persistent_db_data* entry_point;

void main(void) {

	char *db_name = "xmark";
    d_printf1("TR STARTED\n");

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
		//entry_point->nbm=NULL;
		//entry_point->nbm_size=0;
        d_printf1("OK\n");

		d_printf1("Initializing indirection table...");
        init_indirection_table(db_name);
        d_printf1("OK\n");

        d_printf1("Initializing metadata...");
#ifdef NO_PERSISTENCY
		metainfo_ptr= se_new metadata_cell*;
		*metainfo_ptr=NULL;
#endif 
        init_metadata(&(entry_point->metadata), db_name);
        d_printf1("OK\n");

		test2();

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
        cout << e.what() << endl;
    } catch (exception &e) {
        cout << "Library catched" << endl;
        cout << e.what() << endl;
    }
}
