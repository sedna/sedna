#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WU_INCLUDED
#define WU_INCLUDED

#include "wutypes.h"

int WuInit(int is_rcv_mode);

int WuRelease();

int WuNotifyCheckpointActivatedAndWaitForSnapshotAdvanced();

int WuNotifyCheckpointFinished();

int WuGetTimestamp(TIMESTAMP *ts);

int WuSetTimestamp(TIMESTAMP ts);


#endif
