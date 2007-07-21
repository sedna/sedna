#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "wuclients.h"

int enumProc(ClientsEnumClientsInfo *info, int clientId)
{
	fprintf(stderr,"%2d (%d of %d)\n", 
			clientId, info->alreadyEnumeratedCount+1, info->clientsCount);
	return 1;
}

int main()
{
	int i, dummy;
	TICKET ticket1, ticket2;
	int clientId1, clientId2;
	ClientsSetup setup={110,0x10000}; /* max 120 clients, 64KB storage for each */ 
	ClientsEnumClientsInfo info;

	ClInitialise();
	ClReserveStateBlocks(&ticket1,1); fprintf(stderr,"%08p\n",ticket1);
	ClReserveStateBlocks(&ticket1,1); fprintf(stderr,"%08p\n",ticket1);
	ClReserveStateBlocks(&ticket1,4); fprintf(stderr,"%08p\n",ticket1);
	ClReserveStateBlocks(&ticket2,8); fprintf(stderr,"%08p\n",ticket2);
	ClStartup(&setup);

	
	ClRegisterClient(&clientId1,0);
	ClRegisterClient(&clientId1,0);
	ClRegisterClient(&clientId1,1);
	clientId2=13; ClRegisterClient(&clientId2,1);
	ClSetCurrentClientId(clientId2);
	clientId2=77; ClRegisterClient(&clientId2,1);
	clientId2=79; ClRegisterClient(&clientId2,1);
	
	ClMarkClientReady(0);
	ClMarkClientReady(1);
	ClMarkClientReady(2); 
	
	ClMarkClientReady(77);

	ClLockClientSet();
	ClMarkClientReady(13);
	
	ClDbgDump(0);
	fputs("Clients marked ready:\n",stderr);
	ClEnumClients(&info,enumProc);
	fputs("\n",stderr);

	for (i=0;i<16;++i) ClRegisterClient(&dummy,0);

	ClUnlockClientSet();

	ClUnregisterClient(0);
	ClUnregisterClient(13);
	ClMarkClientLeaving(0);
	ClSetCurrentClientId(77);
	ClUnregisterClient(0);
	ClUnregisterClient(13);

	for (i=5;i<11;++i) ClMarkClientReady(i);

	ClDbgDump(0);
	fputs("Clients marked ready:\n",stderr);
	ClEnumClients(&info,enumProc);
	fputs("\n",stderr);

	ClDeinitialise();
	return 0;
}
