/*
 * File:  test.cpp
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

#include "tr/pstr/pstr.h"
#include <stdlib.h>
#include <time.h>
#include "tr/pstr/pstrblk.h"

using namespace std;
#define NO_PERSISTENCY

/*
void test1() {

	xptr	blk;
	char*	str1="Bigbigbigbigbigbigbigbigbigbigbigbig";
	int		size1;
	char*	str2="Smallsmallsmallsmall";
	int		size2;
	//vmm_alloc_data_block(&blk);
	XADDR(blk) = malloc(PAGE_SIZE);
	pstr_blk_markup(blk);
	pstr_print_blk(blk);
	size1 = strlen(str1)+1;
	size2 = strlen(str2)+1;
	for(int i=0; i<100; i++)
		pstrs[i] = pstr_allocate(blk, str1, size1);
	pstr_deallocate(blk, pstrs[0], size1);
	pstr_deallocate(blk, pstrs[9], size1);
	pstr_deallocate(blk, pstrs[8], size1);
	pstr_deallocate(blk, pstrs[7], size1);
	pstr_deallocate(blk, pstrs[6], size1);
	pstr_deallocate(blk, pstrs[10], size1);
	pstr_deallocate(blk, pstrs[11], size1);
	pstr_deallocate(blk, pstrs[27], size1);
	pstr_deallocate(blk, pstrs[15], size1);
	//pstr_print_blk(blk);
	//for (i=0; i<5; i++)
		pstr_allocate(blk, str2, size2);
	//pstr_print_blk(blk);
}

void test2() {

	xptr	blk;
	char*	str1="Bigbigbigbigbigbigbigbigbigbigbigbig";
	int		size1;
	char*	str2="Smallsmallsmallsmall";
	int		size2;
	xptr*	pstrs= new xptr[100];
	//vmm_alloc_data_block(&blk);
	XADDR(blk) = malloc(PAGE_SIZE);
	pstr_blk_markup(blk);
	pstr_print_blk(blk);
	size1 = strlen(str1)+1;
	size2 = strlen(str2)+1;
	for(int i=0; i<100; i++)
		pstrs[i] = pstr_allocate(blk, str2, size2);
	pstr_deallocate(blk, pstrs[0], size2);
	pstr_deallocate(blk, pstrs[11], size2);
	pstr_deallocate(blk, pstrs[27], size2);
	pstr_deallocate(blk, pstrs[15], size2);
	pstr_print_blk(blk);
	//for (i=0; i<5; i++)
		pstr_allocate(blk, str1, size1);
	pstr_print_blk(blk);
}
*/

/* generates random null-terminated string not exceeding given size in given (big enough) buffer */
void generate_random_string(char* str, shft max_size) {
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

void test3() {

	char	dummy[10];
	xptr	blk;
	char	buf[PAGE_SIZE];
	shft	str_num=100000;
	shft	allocated_str_num;
	shft	dealloc_num=0;
	shft	str_size;
	shft	max_str_size=1000;
	int		blk_num=1;
	bool	output=false;


	/* descriptors */
	xptr*	blks=new xptr[str_num];
	t_dsc*	dscs= new t_dsc[str_num];
	xptr*	xptrs=new xptr[str_num];
	shft*	sizes=new shft[str_num];
	/* initialize xptrs as pointers to the descriptors */
	for (int i=0; i<str_num; i++) {
		xptrs[i].addr = dscs+i;
	}
	blk = pstr_create_blk(true);
	/* fill with 'str_num' random strings */
	cout << "Allocating " << str_num << " random strings...";
	/*for(i=0; i<str_num; i++) {
		//generate_random_string(buf, max_str_size);
		str_size = strlen(buf);
		if (!pstr_fit_into_blk(blk, str_size)) {
			allocated_str_num=i;
			break;
			
			blks[blk_num]=blk;
			blk_num++;
			blk = pstr_create_blk(true);
			
		}
		pstr_allocate(blk, xptrs[i], buf, str_size);
		sizes[i]=str_size;
	}*/
	FILE* f = fopen("c:\\home\\charisma\\kernel\\idx\\btree\\words3", "r");
	allocated_str_num=0;
	char ch;
	str_size=0;
    while((ch = fgetc(f)) != EOF) {
		if(ch == '\n') {
			if (!pstr_fit_into_blk(blk, str_size)) {
				break;
				/*
				blks[blk_num]=blk;
				blk_num++;
				blk = pstr_create_blk(true);
				*/
			}
			pstr_allocate(blk, xptrs[allocated_str_num], buf, str_size);
			sizes[allocated_str_num]=str_size;
			/* reset key, prepare next obj value */
			str_size=0;
			//obj.addr=(void*)((int)obj.addr+1);
			allocated_str_num++;
		} else {
			buf[str_size]=ch;
			str_size++;
		}
	}
	fclose(f);
	cout << "Done. Occupied " << blk_num << " blocks." << endl;

	/* randomly delete 'str_num/5' strings */
	cout << "Trying to delete " << str_num/2 << " strings...";
	for (int j=0; j<allocated_str_num; j++) {
		shft str_idx= rand()%allocated_str_num;
		if (xptrs[str_idx] == XNULL)
			continue;
		pstr_deallocate(xptrs[str_idx]);
		xptrs[str_idx]=XNULL;
		dealloc_num++;
	}
	cout << "Done. Actually deallocated " << dealloc_num << " strings." << endl;

	/* print out contents of all remaining strings */
	for (i=0; i<allocated_str_num; i++) {
		if (xptrs[i] != XNULL) {
			pstr_read_from_node(xptrs[i], buf);
			buf[sizes[i]]='\0';
			cout << buf << endl;;
		}
	}
	//pstr_print_blk(blk);
}

persistent_db_data* entry_point;

void init() {

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
        d_printf1("OK\n");

		d_printf1("Initializing indirection table...");
        init_indirection_table(db_name);
        d_printf1("OK\n");

        d_printf1("Initializing metadata...");
#ifdef NO_PERSISTENCY
		//metainfo_ptr= new metadata_cell*;
		//*metainfo_ptr=NULL;
#endif 
        init_metadata(&(entry_point->metadata), db_name);
        d_printf1("OK\n");

		test3();

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

void main() {
	init();
	//test3();
}

