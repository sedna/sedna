#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WU_INCLUDED
#define WU_INCLUDED

int WuInit(int is_rcv_mode);

int WuRelease();

int WuNotifyCheckpointActivatedAndWaitForSnapshotAdvanced();

int WuNotifyCheckpointFinished();


#endif
