/*
 * File:  physrcv.h - Physical recovery on sm
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Only the user interface is specified here. For further information refer to physrcv.cpp file.
 *
 */

#ifndef _LL_PHYS_RCV_
#define _LL_PHYS_RCV_

// Recover physical state of a database on the moment of persistent snapshot.
// Returns:
// 		lsn of the oldest record needed for logical record;
LSN llRecoverPhysicalState();

// Recover persistent heap
void llRcvRestorePh();

// Retrieves info from hot-backup record
// Parameters:
// 		RecBuf - pointer to record in memory
void llRecoverHbRec(void *RecBuf);

#endif
