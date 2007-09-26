#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUCLIENTS_INCLUDED
#define WUCLIENTS_INCLUDED

#include "wutypes.h"
#include "wuincguard.h"

struct ClSetup
{
	int maxClientsCount;
	int maxSizePerClient;
};

struct ClEnumerateClientsParams
{
	int clientsCount;
	int alreadyEnumeratedCount;
	void *userData;
};

typedef int (*ClEnumerateClientsProc)(ClEnumerateClientsParams *params, int clientId);

int  ClInitialize();

int  ClReserveStateBlocks(TICKET *ticket, size_t size);

int  ClStartup(ClSetup *setup);

int  ClShutdown();

void ClDeinitialize();

int  ClQueryMaxClientsCount();

int  ClRegisterClient(int *clientId, int isFixed);

int  ClMarkClientReady(int clientId);

int  ClMarkClientLeaving(int clientId);

int  ClUnregisterClient(int clientId);

int ClGetCurrentClientId();

int  ClSetCurrentClientId(int clientId);

int  ClGetCurrentStateBlock(void **ptr, TICKET ticket);

int  ClGetStateBlock(void **ptr, TICKET ticket, int clientId);

int ClEnumerateClients(ClEnumerateClientsParams *enumClientsInfo, 
					   ClEnumerateClientsProc enumProc);

int ClLockClientsSet();

int ClUnlockClientsSet();

int ClIsClientReady(int *isReady, int clientId);

void ClDbgDump(int reserved);

#endif
