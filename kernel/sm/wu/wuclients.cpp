#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "wuaux.h"
#include "wuclients.h"
#include "wustatetable.h"

/*	Global state. */ 

struct ClientsMgmtData
{
	int currentCntr;
};

struct ClientsThreadState
{
	int currentClientId;
	int clientSetLockCount;
};

static
#ifdef _MSC_VER
__declspec(thread)
#else
__thread
#endif
ClientsThreadState threadState =
{
	-1, 0
};

static StateTable stateTable;
static uint32_t *readyClientsBitmap=NULL;
static int clientsCount=0;
static int readyClientsCount=0;

/*	Functions. */ 
int ClInitialise()
{
	int success=0;
	TICKET dummy=NULL;

	readyClientsBitmap=NULL;
	clientsCount=0;
	readyClientsCount=0;
	InitialiseStateTable(&stateTable);

	success=ReserveStateTableColumn(&stateTable,&dummy,sizeof(ClientsMgmtData),0);
	return success;
}

int ClReserveStateBlocks(TICKET *ticket, size_t size)
{
	assert(ticket);
	return ReserveStateTableColumn(&stateTable,ticket,size,0);
}

int ClStartup(ClientsSetup *clientsSetup)
{
	int success=0;
	size_t size=0;

	if (clientsSetup->maxClientsCount<0)
	{
		ERROR(WUERR_BAD_PARAMS);
	}
	else
	{
		size=sizeof(uint32_t)*RoundSizeUp((size_t)clientsSetup->maxClientsCount,32)/32;
		readyClientsBitmap=(uint32_t*)malloc(size);
		if (readyClientsBitmap!=NULL) 
		{
			memset(readyClientsBitmap,0,size);
			success=CreateStateTableRows(&stateTable,
										 clientsSetup->maxClientsCount,
										 clientsSetup->maxSizePerClient);
		}
		if (!success)
		{
			free(readyClientsBitmap);
			readyClientsBitmap=NULL;
		}
	}
	return success;
}

void ClDeinitialise()
{
	free(readyClientsBitmap);
	readyClientsBitmap=NULL;
	clientsCount=0;
	readyClientsCount=0;
	DeinitialiseStateTable(&stateTable);
}

int ClQueryMaxClientsCount()
{
	return stateTable.rowsCount;
}

int ClRegisterClient(int *clientId, int isFixed)
{
	int success=0;
	int isVacant=0;
	ClientsMgmtData *mgmtData=NULL;

	assert(clientId);	
	if (isFixed) /* use clientId value, do not search for vacant slots */ 
	{
		if(!IsStateTableRowVacant(&stateTable,&isVacant,*clientId))
		{
			if (ISERROR(WUERR_STATE_TABLE_BAD_ROW_ID)) ERROR(WUERR_BAD_CLIENT_ID);
		}
		else if(!isVacant)
		{
			ERROR(WUERR_CLIENT_ID_ALREADY_IN_USE);
		}
		else
		{
			success=SetStateTableIsVacantRowFlag(&stateTable,*clientId,0);
		}
	}
	else
	{
		success=OccupyStateTableFirstVacantRow(&stateTable,clientId,0,INT_MAX);
	}

	if (success) 
	{
		GetStateTableMasterCell(&stateTable,(void**)&mgmtData,*clientId);
		assert(mgmtData);
		mgmtData->currentCntr=0;
		++clientsCount;
	}
	return success;
}

static
int ClMarkClientReadyOrLeaving(int clientId, int flag)
{
	int success=0;
	uint32_t mask=1, *pval=NULL;

	if (threadState.clientSetLockCount>0)
	{
		ERROR(WUERR_CLIENT_SET_DEADLOCK_DETECTED);
	}
	else
	{
		if (!IsValidStateTableRowId(&stateTable,clientId))
		{
			if (ISERROR(WUERR_STATE_TABLE_BAD_ROW_ID)) ERROR(WUERR_BAD_CLIENT_ID);
		}
		else
		{		
			pval=readyClientsBitmap+(clientId/32);
			mask<<=clientId%32;
			switch (flag)
			{			
			case 1: /* mark ready */ 
				if (0==(mask&*pval))
				{
					*pval|=mask;
					++readyClientsCount;
					success=1;
				}
				else
				{					
					ERROR(WUERR_CLIENT_ALREADY_MARKED_READY);
				}
				break;
			case 2: /* mark leaving */ 
				if (mask&*pval)
				{
					*pval&=~mask;
					--readyClientsCount;
					success=1;
				}
				else
				{
					ERROR(WUERR_CLIENT_ALREADY_MARKED_LEAVING);
				}
				break;
			default:
				assert(0);
			}
		}
	}
	return success;
}

int ClMarkClientReady(int clientId)
{
	return ClMarkClientReadyOrLeaving(clientId,1);
}

int ClMarkClientLeaving(int clientId)
{
	return ClMarkClientReadyOrLeaving(clientId,2);
}

int ClUnregisterClient(int clientId)
{
	int success=0;
	uint32_t mask=1, *pval=NULL;
	ClientsMgmtData *mgmtData=NULL;

	if (!GetStateTableMasterCell(&stateTable,(void**)&mgmtData,clientId))
	{
		if (ISERROR(WUERR_STATE_TABLE_BAD_ROW_ID)) ERROR(WUERR_BAD_CLIENT_ID); 
	}
	else if (mgmtData->currentCntr>0)
	{
		ERROR(WUERR_UNABLE_TO_UNREGISTER_CURRENT_CLIENT);
	}
	else
	{
		pval=readyClientsBitmap+(clientId/32);
		mask<<=clientId%32;

		if (*pval&mask)
		{
			ERROR(WUERR_UNABLE_TO_UNREGISTER_READY_CLIENT);
		}
		else if(SetStateTableIsVacantRowFlag(&stateTable,clientId,1))
		{
			--clientsCount;
			success=1;
		}
	}
	return success;
}

int ClGetCurrentClientId()
{
	return threadState.currentClientId;
}

int ClSetCurrentClientId(int clientId)
{
	int success=0; 
	ClientsMgmtData *mgmtData=NULL;

	if (threadState.currentClientId!=-1)
	{
		GetStateTableMasterCell(&stateTable,(void**)&mgmtData,threadState.currentClientId);
		assert(mgmtData && mgmtData->currentCntr>0);
		--mgmtData->currentCntr;
		threadState.currentClientId=-1;
	}

	if (clientId==-1)
	{
		success=1;
	}
	else
	{
		if (!GetStateTableMasterCell(&stateTable,(void**)&mgmtData,clientId))
		{
			if (ISERROR(WUERR_STATE_TABLE_BAD_ROW_ID)) ERROR(WUERR_BAD_CLIENT_ID);
		}
		else
		{
			++mgmtData->currentCntr;
			threadState.currentClientId=clientId;
			success=1;
		}
	}
	return success;
}

int ClGetCurrentStateBlock(void **ptr, TICKET ticket)
{
	return GetStateTableCell(&stateTable,ptr,ticket,threadState.currentClientId);
}

int ClGetStateBlock(void **ptr, TICKET ticket, int clientId)
{
	return GetStateTableCell(&stateTable,ptr,ticket,clientId);
}

int ClLockClientSet()
{
	int success=0;
	if (threadState.clientSetLockCount==INT_MAX)
	{
		ERROR(WUERR_CLIENT_SET_MAX_NUMBER_OF_LOCKS_EXCEEDED);
	}
	else
	{
		++threadState.clientSetLockCount;
		success=1;
	}
	return success;
}

int ClUnlockClientSet()
{
	int success=0;
	if (threadState.clientSetLockCount<=0)
	{
		assert(threadState.clientSetLockCount==0);
		ERROR(WUERR_CLIENT_SET_ALREADY_UNLOCKED);
	}
	else
	{
		--threadState.clientSetLockCount;
		success=1;
	}
	return success;
}

int ClEnumClients(ClientsEnumClientsInfo *enumClientsInfo, 
				  int(*enumProc)(ClientsEnumClientsInfo *enumClientsInfo, int clientId))
{
	int cnt=0, bitid=0, okstatus=0;
	uint32_t *begin=NULL, *i=NULL, *end=NULL, temp=0;

	assert(enumClientsInfo);

	begin=readyClientsBitmap;
	end=begin+RoundSizeUp(stateTable.rowsCount,32)/32;

	okstatus=ClLockClientSet();
	assert(okstatus);
	for(i=begin;i<end;++i)
	{
		temp=*i;
		while(temp)
		{
			bitid=ResetLowestBitSet(&temp);
			enumClientsInfo->clientsCount=readyClientsCount;
			enumClientsInfo->alreadyEnumeratedCount=cnt;
			if (!enumProc || 0==enumProc(enumClientsInfo,(i-begin)*32+bitid)) break;
			++cnt;
		}
	}
	okstatus=ClUnlockClientSet();
	assert(okstatus);
	return 1;
}

int ClIsClientReady(int *isReady, int clientId)
{
	int success=0; uint32_t m=1;

	assert(isReady);
	if (!IsValidStateTableRowId(&stateTable,clientId))
	{
		if (ISERROR(WUERR_STATE_TABLE_BAD_ROW_ID)) ERROR(WUERR_BAD_CLIENT_ID); 
	}
	else
	{
		m<<=clientId%32;
		*isReady=((readyClientsBitmap[clientId/32]&m)!=0);
		success=1;
	}
	return success;
}

void ClDbgDump(int reserved)
{
	DbgDumpMemoryMarks marks;
	DbgDumpMemoryParams params;

	fprintf(stderr,"ClDbgDump total %d, ready %d, cur %d, lock %d\n", 
			clientsCount, readyClientsCount, 
			threadState.currentClientId, threadState.clientSetLockCount);

	DbgDumpStateTableParams(&stateTable,&params);

	memset(&marks,0,sizeof marks);
	marks.mark='R';
	marks.next=NULL;
	marks.markBits=readyClientsBitmap;
	params.marks=&marks;
	DbgDumpMemory(&params);
}
