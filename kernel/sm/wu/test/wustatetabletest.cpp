#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "wustatetable.h"

struct A
{
	int a, b, c, d;
};

struct B
{
	char str[12];
};

StateTable t;

int main()
{
	TICKET ticketA, ticketB; int rowId1, rowId2;
	A *a; B *b;
	void *dummy;
	int i; int isVacant;

	InitialiseStateTable(&t);
#if 0
	for (i=0;i<100;++i)
	{
		ReserveStateTableColumn(&t,&ticketA,sizeof(A),0);
		fprintf(stderr,"%p\n",ticketA);
	}
#endif
	ReserveStateTableColumn(&t,&ticketA,sizeof(A),0);
	ReserveStateTableColumn(&t,&ticketB,sizeof(B),0);
	CreateStateTableRows(&t,100,0x10000);
	
	OccupyStateTableFirstVacantRow(&t,&rowId1,0,INT_MAX);
	OccupyStateTableFirstVacantRow(&t,&rowId2,10,INT_MAX);
	OccupyStateTableFirstVacantRow(&t,&rowId2,9,INT_MAX);
	OccupyStateTableFirstVacantRow(&t,&i,9,10);

	SetStateTableIsVacantRowFlag(&t,99,0);

	IsStateTableRowVacant(&t,&isVacant,0);
	assert(isVacant==0);
	IsStateTableRowVacant(&t,&isVacant,9);
	assert(isVacant==0);
	IsStateTableRowVacant(&t,&isVacant,10);
	assert(isVacant==0);
	IsStateTableRowVacant(&t,&isVacant,99);
	assert(isVacant==0);

	IsStateTableRowVacant(&t,&isVacant,1);
	assert(isVacant==1);
	IsStateTableRowVacant(&t,&isVacant,5);
	assert(isVacant==1);
	IsStateTableRowVacant(&t,&isVacant,8);
	assert(isVacant==1);
	IsStateTableRowVacant(&t,&isVacant,11);
	assert(isVacant==1);
	IsStateTableRowVacant(&t,&isVacant,30);
	assert(isVacant==1);
	IsStateTableRowVacant(&t,&isVacant,50);
	assert(isVacant==1);
	IsStateTableRowVacant(&t,&isVacant,98);
	assert(isVacant==1);


	DbgDumpStateTable(&t);
	
	GetStateTableCell(&t,(void**)&a,ticketA,rowId2);
	
	a->a=0xFFFFFFFF;
	a->b=0xAAAAAAAA;
	a->c=0xFFFFFFFF;
	a->d=0xAAAAAAAA;
	DbgDumpStateTable(&t);

	SetStateTableIsVacantRowFlag(&t,rowId2,1);
	OccupyStateTableFirstVacantRow(&t,&rowId2,rowId2,INT_MAX);
	DbgDumpStateTable(&t);

	GetStateTableCell(&t,(void**)&b,ticketB,rowId1);
	SetStateTableIsVacantRowFlag(&t,1,0);

	/* overflow */ 
	strcpy(b->str,"\x01\x02\x03\x04" "\x05\x06\x07\x08" "\x09\x0A\x0B\x0C" "\x0D\x0E\x0F\x10" 
		"stupid overflow");

	/* and underflow */ 
	memset(b->str-36,0xCC,36);

	/* GetStateTableCell(&t,&dummy,0,ticketA); */ 
	DbgDumpStateTable(&t);

	DeinitialiseStateTable(&t);
	return 0;
}
