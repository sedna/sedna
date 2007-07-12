#include "wuclients.h"
#include "wustatetable.h"

/*	Note on synchronistaion: currently we have subsystems
	completely non-thread safe. However at least 2 threads
	exists simultaneously - the checkpoint thread and the
	main thread that is servicing trns. The *temporary* solution
	is to block in ClSetCurrentClientId until other threads
	reset their client-ids to -1. ClRegisterClient and
	ClUnRegisterClient are thread safe and never block.
	ClGetCurrentClientId is thread safe and fast (employs tls).
	ClUnRegisterClient will fail if is current-client-id
	for any thread. */ 

/**/ 

struct ClientMngmtData
{
	int currentCntr;
};

#ifdef _MSC_VER
__declspec(thread) int currentClientId=-1;
#else
__thread int currentClientId=-1;
#endif

StateTable stateTable;
uint32_t activeBitmask;
int clientsNum;
int activeClientsNum;

/*
	*/ 
int ClReserveStateBlocks(TICKET *ticket, size_t size)
{
	assert(ticket);
	return ReserveStateTableColumn(&stateTable,ticket,size,0);
}

int ClInitialise()
{
	int status=1;
	TICKET dummy=NULL;
	/* initialise also sems */ 

	status=InitialiseStateTable(&stateTable);
	if(status)
	{
		status=ReserveStateTableColumn(&stateTable,&dummy,sizeof(ClientMngmtData),0);
	}

	return status;
}

int ClStartup(ClientsSetup *clientsSetup)
{
	return CreateStateTableRows(&stateTable,clientsSetup->maxClientsNum,clientsSetup->maxSizePerClient);
}

void ClDeinitialise()
{
	/* free sems */ 
	DeinitialiseStateTable(&stateTable);
}

void ClQueryMaxClients(int *maxClientsNum)
{
	assert(maxClientsNum);
	*maxClientsNum=stateTable.rowsCount;
}

