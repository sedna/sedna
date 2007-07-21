#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUCLIENTS_INCLUDED
#define WUCLIENTS_INCLUDED

#include "wutypes.h"

/*	[Clients module.]
	- It does client ids assignment and reuse (ClRegisterClient, ClUnregisterClient).
	- Provides clients enumeration interface (ClEnumClients).
	- Allocates memory to store client-specific state for any subsystem
	  that requests this storage (ClReserveStateBlocks).
	- Provides access to stored client state by clientId and the ticket
	  obtained during reservation (ClGetStateBlock).
	- Stores the current client id since many operations are
	  performed on behalf of the particular client and passing clientId explicitly
	  is cumbersome (ClGetCurrentClientId, ClSetCurrentClientId).
	- Implements basic synchronization (ClMarkClientReady, ClMarkClientLeaving,
	  ClLockClientSet, ClUnlockClientSet).

	[On synchronization.]
	Clients module is thread safe except the initialisation, reservation, startup
	and deinitialisation functions. These functions are expected to be called from
	the main thread only. Before calling ClDeinitialise ensure that no threads are
	executing code from the module since ClDeinitialise carelessly tears things down.
	Access to client state ("state blocks") is not synchronized. Current client id
	is thread-local.
	
	Every client passes the following stages:
	(UNREG) -	Client does not have any id.
	(REG) -		ClRegisterClient called, id assigned. Client begins state blocks initialisation.
	(READY) -	ClMarkClientReady called. State blocks are initialised. Client becomes visible to 
				enumeration via ClEnumClients.
	(LEAVING) - ClMarkClientLeaving called. Client becomes invisible to enumeration.
				Client begins state blocks deinitialisation.
	(UNREG) -	State blocks deinitialised. ClUnregisterClient called. Client id is marked vacant
				and may be reused.

	Client will block in ClMarkClientReady and ClMarkClientLeaving functions until client set is 
	unlocked. ClLockClientSet and ClUnlockClientSet are provided to lock and unlock client set
	respectively. Multiple threads can lock client set simultaneously and a single thread
	can lock client set multiple times. A lock count is maintained internally for each thread
	and unlocking happens as soon as all threads' lock count reaches zero (lock count is initially 0). 
	If the thread with non-zero lock count attempts to mark any client ready or leaving 
	ClMarkClientsXXX function fails. Also note that ClEnumClients locks client set while 
	enumerating.

	Client set locking serves several purposes. In the most basic scenario every client has some
	properties which are assigned during initialisation and never changes (transaction timestamp,
	start lsn, whatever). Assume some activity needs to take *consistent* listing of currently
	active transactions with their properties. An example is recording the list of active transaction
	ids and their timestamps when creating snapshots. Use Cl clients to impersonate transactions
	and store transaction properties in state blocks. Even if every client runs in a separate thread
	it is safe to access any state block since they never change, except during initialisation
	and deinitialisation. Client is assumed to complete it's initialisation before it is
	marked ready and it must never perform deinitialisation before marked leaving. ClEnumClients
	enumerates only clients marked ready hence only "stable" data is observable. Recorded 
	listing is consistent since client set is locked during enumeration and no client
	is allowed either to enter READY stage or to leave it.

	In the more complex scenario client state changes. Obviously synchronization provided
	by Cl is not enough. However client set locking and ClMarkClientReady/ClMarkClientLeaving 
	protocol guarantees that the client is unable to flee while it is beeing examined by
	the concurrent thread. The solution also "hides" partially initialised or partially
	deinitialised clients. It is usefull since initialising/deinitialising clients may 
	violate some integrity constrains and are probably unable to serve particular 
	requests correctly.

	[Misc.]
	State blocks can be successfully obtained as soon as client gets an id assigned.
	ClGetStateBlock will fail if clientId passed is invalid. The function doesn't
	synchronize and may return incorrect output if clientId is becoming valid or
	is turning invalid during the call due to concurrent activity. ClIsClientReady
	is provided to check whether the given client is now in his READY stage. The function
	doesn't lock the client set and will produce incorrect results if the client set
	is not locked in advance.

	Multiple threads may have identical current client ids. If the client is current
	for any thread ClUnregisterClient fails. ClSetCurrentClientId accepts either a valid
	client id or -1 that is interpreted as "no current id". ClRegisterClient doesn't
	affect current client id and so does ClUnregisterClient. */ 

struct ClientsSetup
{
	int maxClientsCount;
	int maxSizePerClient;
};

int  ClInitialise();
int  ClReserveStateBlocks(TICKET *ticket, size_t size);
int  ClStartup(ClientsSetup *clientsSetup);
void ClDeinitialise();

void ClQueryMaxClientsCount(int *maxClientsCount);
int  ClRegisterClient(int *clientId, int isFixed);
int  ClMarkClientReady(int clientId);
int  ClMarkClientLeaving(int clientId);
int  ClUnregisterClient(int clientId);

void ClGetCurrentClientId(int *clientId);
int  ClSetCurrentClientId(int clientId);
int  ClGetCurrentStateBlock(void **ptr, TICKET ticket);
int  ClGetStateBlock(void **ptr, TICKET ticket, int clientId);

struct ClientsEnumClientsInfo
{
	int clientsCount;
	int alreadyEnumeratedCount;
	void *userData;
};

int ClEnumClients(ClientsEnumClientsInfo *enumClientsInfo, 
				  int(*enumProc)(ClientsEnumClientsInfo *enumClientsInfo, int clientId));

int ClLockClientSet();
int ClUnlockClientSet();
int ClIsClientReady(int *isReady, int clientId);

void ClDbgDump(int reserved);

#endif
