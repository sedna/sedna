#if (_MSC_VER > 1000)
#pragma once
#endif

#ifndef WU_INCLUDED
#define WU_INCLUDED

#include "wutypes.h"

int WuGetTimestamp(TIMESTAMP *ts);

int WuSetTimestamp(TIMESTAMP ts);

int WuInit(int is_rcv_mode);

int WuRelease();

int WuNotifyCheckpointActivatedAndWaitForSnapshotAdvanced();

int WuNotifyCheckpointFinished();

/* reports errors with exceptions instead of error codes */ 

void WuInitExn(int is_rcv_mode);

void WuReleaseExn();

void WuNotifyCheckpointActivatedAndWaitForSnapshotAdvancedExn();

void WuNotifyCheckpointFinishedExn();

#endif
