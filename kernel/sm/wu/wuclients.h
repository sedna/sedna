#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUCLIENTS_INCLUDED
#define WUCLIENTS_INCLUDED

/*	Clients component implements client id allocation, deallocation and
	provides memory for state blocks. */ 

#include "wutypes.h"

struct ClientsSetup
{
	int maxClientsNum;
	int maxSizePerClient;
};

/*	Initialise Clients. */ 
int ClInitialise();

/* */ 
int ClReserveStateBlocks(TICKET *ticket, size_t size);

int ClStartup(ClientsSetup *clientsSetup);
void ClDeinitialise();

void ClQueryMaxClients(int *maxClientsNum);
int ClRegisterClient(int *clientId, int reserved);
int ClMarkClientActive(int clientId);
int ClUnregisterClient(int clientId);
void ClGetCurrentClientId(int *clientId);
void ClSetCurrentClientId(int clientId);
int ClGetCurrentStateBlock(TICKET ticket, void **ptr);
int ClGetStateBlock(TICKET ticket, void **ptr, int clientId);
int ClGetActiveClientsBitmap(void *buf, size_t size, size_t *sizeRequired);

#endif
