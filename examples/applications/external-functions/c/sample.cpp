/* 
 * This file contains an example of an external XQuery function 'sq'
 * This function takes a sequence of values of the xs:integer type 
 * and returns a sequence of their squares.
 *
 * Thus the following query:
 *      declare function sq($a as xs:integer*) as xs:integer external;
 *      sq((10, 11, 3, "4", 1.0))
 *
 * would return the following sequence:
 *      (100, 121, 9, -1 -1)
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sedna_ef.h"


#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" {
//This is needed to ensure that functions will be exported as C functions
//and to let MSVC know what functions to export
#ifdef _WIN32
char const DLLEXPORT *ef_names[];
#endif
SEDNA_SEQUENCE_ITEM DLLEXPORT *sq(SEDNA_EF_INIT *init, SEDNA_EF_ARGS *args, char * error_msg_buf);
void DLLEXPORT sq_init(SEDNA_EF_INIT *init, char * error_msg_buf);
void DLLEXPORT sq_deinit(SEDNA_EF_INIT *init, char * error_msg_buf);
}

char const *ef_names[] = { "sq", NULL};


void sq_init(SEDNA_EF_INIT *init, char * error_msg_buf)
{
}

void sq_deinit(SEDNA_EF_INIT *init, char * error_msg_buf)
{
}


SEDNA_SEQUENCE_ITEM *sq(SEDNA_EF_INIT *init, SEDNA_EF_ARGS *args, char * error_msg_buf)
{
	SEDNA_SEQUENCE_ITEM *item, *res = NULL, *last;
	
	if (args->length != 1)
	{
		sprintf(error_msg_buf, "bad number of arguments!");
		return NULL;
	}
	
	SEDNA_SEQUENCE_ITEM *it = args->args[0];
	while (it != NULL)
	{
		item = (SEDNA_SEQUENCE_ITEM*)init->sedna_malloc(sizeof(SEDNA_SEQUENCE_ITEM));
		if (res == NULL)
			res = item;
		else
			last->next = item;
		item->next = NULL;

		last = item;
		item->data.type = SEDNATYPE_integer;
		
		if (it->data.type == SEDNATYPE_integer)
			item->data.val_integer = it->data.val_integer * it->data.val_integer;
		else
			item->data.val_integer = -1;
	
		it = it->next;
	}
	
	return res;
}

