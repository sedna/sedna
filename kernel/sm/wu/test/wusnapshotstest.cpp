#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "wuclients.h"
#include "wusnapshots.h"

int FreeBlock(XPTR xptr)
{
	fprintf(stderr,"FreeBlock %0.16I64X\n",xptr);
	return 1;
}

#define BASETS 0xFFFF0000

int GetTimestamp(TIMESTAMP *ts)
{
	static TIMESTAMP sts=BASETS;
	*ts=sts++;
	return 1;
}

int OnDiscardSnapshot(TIMESTAMP ts)
{
	fprintf(stderr,"OnDiscardSnapshot %0.16I64X\n",ts);
	return 1;
}

int SaveListsProc(SnapshotsOnCheckpointParams *params, SnapshotsVersion *buf, size_t count, int isGarbage)
{
	SnapshotsVersion *ebuf=NULL;
	fprintf(stderr,"SaveListProc count %d, isGarbage %d\n", count, isGarbage);
	for(ebuf=buf+count;buf<ebuf; ++buf) fprintf(stderr,"%0.8X:%0.8X\n",(uint32_t)buf->lxptr,(uint32_t)buf->xptr);
	fputs("\n",stderr);
	return 1;
}

SnapshotsRequestForGc sample[16] =
{
	{0x01,0x0100,0},
	{0x02,0X0200,BASETS},
	{0x03,0x0300,BASETS+1},
	{0x04,0x0400,BASETS+2},
	{0x05,0x0500,0},
	{0x06,0x0600,BASETS},
	{0x07,0x0700,BASETS+1},
	{0x08,0x0800,BASETS+2},
	{0x09,0x0900,0},
	{0x0A,0x0A00,BASETS},

	{0x0B,0x0B00,0},
	{0x0C,0x0C00,0},
	{0x0D,0x0D00,0},
};


int main()
{
	ClientsSetup clSetup = {120, 0x10000};
	SnapshotsSetup shSetup;
	SnapshotsResourceDemand demand;
	TIMESTAMP snapshotTs=0, discardedTs=0, persTs=0;
	SnapshotsOnCheckpointParams onCheckpointParams;
	int clientId=0;

	ClInitialise();
	ShInitialise();

	ShQueryResourceDemand(&demand);
	ClReserveStateBlocks(&shSetup.clientStateTicket, demand.clientStateSize);
	shSetup.freeBlock=FreeBlock;
	shSetup.getTimestamp=GetTimestamp;
	shSetup.onDiscardSnapshot=OnDiscardSnapshot;

	ClStartup(&clSetup);
	ShStartup(&shSetup);

	ShDbgDump(0);

	ShAdvanceSnapshots(&snapshotTs, &discardedTs);

	ShDbgDump(0);

	ClRegisterClient(&clientId,0);
	ClSetCurrentClientId(clientId);
	ShOnRegisterClient(1);
	ClDbgDump(0);

	ShAdvanceSnapshots(&snapshotTs, &discardedTs);
	
	ShOnBeginCheckpoint(&persTs);
	ShDbgDump(0);
	ShOnCheckpoint(&onCheckpointParams,SaveListsProc);
	ShDbgDump(0);
	ShOnCompleteCheckpoint();

	ShAdvanceSnapshots(&snapshotTs, &discardedTs);

	ShAcceptRequestForGc(~(TIMESTAMP)0,sample,10);
	ShAcceptRequestForGc(BASETS,sample+10,3);

	ShDbgDump(0);
	ShOnBeginCheckpoint(&persTs);
	ShDbgDump(0);
	ShOnCheckpoint(&onCheckpointParams,SaveListsProc);
	ShDbgDump(0);
	ShOnCompleteCheckpoint();
	ShDbgDump(0);

	ShOnUnregisterClient();
	ClSetCurrentClientId(-1);
	ClUnregisterClient(clientId);

	ShDbgDump(0);
	ClDbgDump(0);

	ShAdvanceSnapshots(&snapshotTs, &discardedTs);
	ShOnBeginCheckpoint(&persTs);
	ShOnCheckpoint(&onCheckpointParams,SaveListsProc);
	ShOnCompleteCheckpoint();

	ShDbgDump(0);

	ShShutdown();

	ShDeinitialise();
	ClDeinitialise();

	return 0;
}
