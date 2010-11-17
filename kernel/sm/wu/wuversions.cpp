#define __WUDANG_SOURCES__

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <list>
#include <algorithm>
#include "common/u/uhash_map.h"
#include "wuaux.h"
#include "wudock.h"
#include "wuerr.h"
#include "wuversions.h"
#include "wuclients.h"
#include "wusnapshots.h"
#include "common/base.h"
#include "sm/bufmgr/bm_core.h"

#define VE_SNAPSHOTS_COUNT						3
#define VE_BUFSZ							1024

typedef SnRequestForGc VeFunction;

#define VE_FUNCTION_ALLOCATE_BLOCK				1
#define VE_FUNCTION_CREATE_VERSION				2
#define VE_FUNCTION_CREATE_VERSION_PERS			3
#define VE_FUNCTION_FREE_BLOCK					4


#define VE_RESTRICTION_NONE						-1
#define VE_RESTRICTION_OLD_VERSION				-2
#define VE_RESTRICTION_DEAD_BLOCK				-3

struct VeRestriction
{
	XPTR xptr;
	TIMESTAMP deletorTs;
	union
	{
		int type;		/* type<0 */
		int creatorId;	/* creatorId>=0 */
	};
	VeRestriction *next;
};

typedef std::list<VeFunction> VeFunctionList;

struct XptrHashFunc
{
	size_t operator() (XPTR xptr) const
	{
		return (size_t)(xptr >> 16);
	}
};

typedef U_HASH_MAP_W_CUSTOM_HASH_FN(XPTR, VeRestriction, XptrHashFunc) VeRestrictionHash;

typedef U_HASH_MAP_W_CUSTOM_HASH_FN(XPTR, XPTR, XptrHashFunc) VeFlushingDependenciesHash;

struct VeClientState
{
	int isCompleted;
    int isRolledBack;
	VeFunctionList *functionList;
	VeRestriction *restrictionList;
};

/* global state */

static int isInitialized = 0;
static VeSetup setup = {};
static VeRestrictionHash *restrictionHash = NULL;
static VeFlushingDependenciesHash *flushingDependenciesHash = NULL;

/* functions from other modules */
static int ImpGetCurrentStateBlock (VeClientState **ptr)
{
	return ClGetCurrentStateBlock((void**)ptr, setup.clientStateTicket);
}

static int ImpGetSnapshotTimestamps (TIMESTAMP *curSnapshotTs, TIMESTAMP *persSnapshotTs)
{
	return SnGetSnapshotTimestamps(curSnapshotTs, persSnapshotTs);
}

static int ImpGetTransactionStatusAndType (int *statusAndType)
{
	return SnGetTransactionStatusAndType(statusAndType);
}

static int ImpGetTransactionSnapshotTs (TIMESTAMP *timestamp)
{
	return SnGetTransactionSnapshotTs(timestamp);
}

static int ImpGetTransactionTs (TIMESTAMP *timestamp)
{
	return SnGetTransactionTs(timestamp);
}

static int ImpExpandDfvHeader (const TIMESTAMP tsIn[],
							   size_t szIn,
							   TIMESTAMP tsOut[],
							   int idOut[],
							   size_t *szOut,
							   TIMESTAMP *anchorTs,
                               bool isTotalAnchor)
{
	size_t szOutLocal = 0;
	TIMESTAMP tsOutBuf[VE_VERSIONS_COUNT+2] = {};
	int idOutBuf[VE_VERSIONS_COUNT+2] = {};

	if (!szOut) szOut = &szOutLocal;
	if (*szOut == 0 && tsOut == NULL && idOut == NULL)
	{
		*szOut = VE_VERSIONS_COUNT+2;
		tsOut = tsOutBuf;
		idOut = idOutBuf;
	}
	return SnExpandDfvHeader(tsIn, szIn, tsOut, idOut, szOut, anchorTs, isTotalAnchor);
}

static int ImpDamageSnapshots(TIMESTAMP timestampMax)
{
	return SnDamageSnapshots(timestampMax);
}

static int ImpSubmitRequestForGc(TIMESTAMP currentSnapshotTs,
								 SnRequestForGc *buf, size_t count)
{
	return SnSubmitRequestForGc(currentSnapshotTs, buf, count);
}

static int ImpPutBlockToBuffer(XPTR xptr, int *bufferId)
{
	assert (setup.putBlockToBuffer);
	return setup.putBlockToBuffer(xptr, bufferId);
}

static int ImpFindBlockInBuffers(XPTR xptr, int *bufferId)
{
	assert (setup.findBlockInBuffers);
	return setup.findBlockInBuffers(xptr, bufferId);
}

static int ImpLocateHeader(int bufferId, VersionsHeader **header)
{
	assert (setup.locateVersionsHeader);
	return setup.locateVersionsHeader(bufferId, header);
}

static int ImpAllocateBlock(XPTR *xptr, int *bufferId)
{
	assert (setup.allocateBlock);
	return setup.allocateBlock(xptr, bufferId);
}

static int ImpFreeBlock(XPTR xptr)
{
	assert(setup.freeBlock);
	return setup.freeBlock(xptr);
}

static int ImpMarkBufferDirty(int bufferId)
{
	assert (setup.markBufferDirty);
	return setup.markBufferDirty(bufferId);
}

static int ImpFlushBuffer(int bufferId, bool sync)
{
	assert (setup.flushBuffer);
	return setup.flushBuffer(bufferId, sync);
}

static int ImpAllocateBlockAndCopyData(XPTR *xptr, int *bufferId, int srcBuf)
{
	assert (setup.allocateBlockAndCopyData);
	return setup.allocateBlockAndCopyData(xptr, bufferId, srcBuf);
}

static int ImpGrantExclusiveAccessToBuffer(int bufferId)
{
	assert (setup.grantExclusiveAccessToBuffer);
	return setup.grantExclusiveAccessToBuffer(bufferId);
}

static int ImpRevokeExclusiveAccessToBuffer(int bufferId)
{
	assert (setup.revokeExclusiveAccessToBuffer);
	return setup.revokeExclusiveAccessToBuffer(bufferId);
}

static int ImpOnPersVersionRelocating(LXPTR lxptr, XPTR xptr, int event)
{
	assert (setup.onPersVersionRelocating);
	return setup.onPersVersionRelocating(lxptr, xptr, event);
}

/* utility functions */

static
int LookupRestriction(XPTR xptr, VeRestriction *restriction)
{
	assert(restriction);
	restriction->xptr = 0;
	restriction->deletorTs = INVALID_TIMESTAMP;
	restriction->type = VE_RESTRICTION_NONE;
	restriction->next = NULL;
	if (setup.flags & VE_SETUP_DISABLE_CORRECTNESS_CHECKS_FLAG)
	{
		/* no restrictions */
	}
	else
	{
		assert(restrictionHash);
		VeRestrictionHash::iterator iter = restrictionHash->find(xptr);
		if (iter != restrictionHash->end())
		{
			*restriction = iter->second;
			restriction->next = NULL;
		}
	}
	return 1;
}

static
int UpdateRestriction(const VeRestriction *newRestriction, VeRestriction **restrictionList)
{
	int success = 0;
	assert(newRestriction && restrictionList);
	if (setup.flags & VE_SETUP_DISABLE_CORRECTNESS_CHECKS_FLAG)
	{
		/* no restrictions */
		success = 1;
	}
	else if (newRestriction->creatorId<0 &&
			 newRestriction->type != VE_RESTRICTION_OLD_VERSION &&
			 newRestriction->type != VE_RESTRICTION_DEAD_BLOCK)
	{
		WuSetLastErrorMacro(WUERR_BAD_PARAMS);
	}
	else
	{
		assert(restrictionHash);
		VeRestriction restriction = *newRestriction;
		VeRestrictionHash::iterator iter = restrictionHash->find(restriction.xptr);

		if (iter == restrictionHash->end())
		{
			/* no existing restriction with this xptr, update restrictionList */
			restriction.next = *restrictionList;
			iter = restrictionHash->insert(
				VeRestrictionHash::value_type(restriction.xptr, restriction)).first;
			*restrictionList = &(iter->second);
			success = 1;
		}
		else if (restriction.type != VE_RESTRICTION_DEAD_BLOCK)
		{
			/* only DEAD_BLOCK restrictions can override other restrictions */
			WuSetLastErrorMacro(WUERR_GENERAL_ERROR);
		}
		else
		{
			/* restriction found, update it inplace, don't change restrictionList */
			restriction.next = iter->second.next;
			iter->second = restriction;
			success = 1;
		}
	}
	return success;
}

static
int DismissRestrictionList(VeRestriction *head, int isCleaningUp)
{
	if (setup.flags & VE_SETUP_DISABLE_CORRECTNESS_CHECKS_FLAG)
	{
		/* no restrictions */
	}
	else
	{
		assert(restrictionHash);
		while (head)
		{
			VeRestriction *inspected = head;
			int isRemoving = isCleaningUp || inspected->creatorId>=0;

			head = head->next;
			inspected->next = NULL;
			if (isRemoving)
			{
				restrictionHash->erase(restrictionHash->find(inspected->xptr));
			}
		}
	}
	return 1;
}

static
int UpdateRestrictionsOnFreeBlock(XPTR xptr)
{
	int success = 0;
	if (setup.flags & VE_SETUP_DISABLE_CORRECTNESS_CHECKS_FLAG)
	{
		/* no restrictions */
		success = 1;
	}
	else
	{
		assert(restrictionHash);
		VeRestrictionHash::iterator iter = restrictionHash->find(xptr);
		if (iter == restrictionHash->end() ||
			(iter->second.type != VE_RESTRICTION_OLD_VERSION &&
			 iter->second.type != VE_RESTRICTION_DEAD_BLOCK))
		{
			/* we have an internal error */
			WuSetLastErrorMacro(WUERR_GENERAL_ERROR);
		}
		else
		{
			restrictionHash->erase(iter);
			success = 1;
		}
	}
	return success;
}

void AnalyzePopularFailure(const char *fn, XPTR xptr, XPTR lxptr, VersionsHeader *hdr);

static
int ValidateHeader(VersionsHeader *hdr)
{
	int success=0, i=0;

	assert(hdr);
	if (hdr->xptr[0]==0 || !IsValidTimestamp(hdr->creatorTs[0]))
	{
		/* must have at least one valid entry */
	}
	else
	{
		for (i=1; i<VE_VERSIONS_COUNT; ++i)
		{
			if (hdr->xptr[i]==0 ||
				!IsValidTimestamp(hdr->creatorTs[i]) /*||
				hdr->creatorTs[i]>=hdr->creatorTs[i-1]*/) break;
		}
		while (i<VE_VERSIONS_COUNT && hdr->xptr[i]==0 && hdr->creatorTs[i]==INVALID_TIMESTAMP) ++i;
		success = (i == VE_VERSIONS_COUNT);
	}
	return success;
}

/*
static
int ResetFlushingDependencies()
{
	assert (flushingDependenciesHash);
	flushingDependenciesHash->clear();
	return 1;
}
*/

static
int UpdateFlushingDependency(XPTR trigger, XPTR target)
{
	assert (flushingDependenciesHash);

	if (target)
	{
		(*flushingDependenciesHash)[trigger] = target;
        wulog(("WULOG: introducing new flushing dependency, flush xptr = %"PRI_XPTR" only after xptr = %"PRI_XPTR, trigger, target));
	}
	else
	{
		flushingDependenciesHash->erase(trigger);
	}
	return 1;
}

static
int LookupFlushingDependency(XPTR trigger, XPTR *target)
{
	VeFlushingDependenciesHash::iterator iter;

	assert (flushingDependenciesHash && target);
	*target = 0;
	iter = flushingDependenciesHash->find(trigger);
	if (iter != flushingDependenciesHash->end()) *target = iter->second;

	return 1;
}

static
int IsPersSnapshotVersion(VersionsHeader *hdr, int *bIsPers)
{
	int success = 0;
	TIMESTAMP persSnapshotTs = INVALID_TIMESTAMP;
	TIMESTAMP tss[VE_VERSIONS_COUNT + 2];
	int ids[VE_VERSIONS_COUNT + 2];
	size_t tssNum = VE_VERSIONS_COUNT + 2;
	int i = 0;

	assert(hdr && bIsPers);
	*bIsPers = 0;
	if (ImpGetSnapshotTimestamps(NULL, &persSnapshotTs) &&
		ImpExpandDfvHeader(hdr->creatorTs, VE_VERSIONS_COUNT, tss, ids, &tssNum, NULL, false))
	{
		i = 0; while (i<(int)tssNum && tss[i]!=persSnapshotTs) ++i;
		*bIsPers = (i < (int)tssNum && ids[i]==0);
		success = 1;
	}
	return success;
}

static int OnFlushBuffer(XPTR xptr)
{
	int success = 0, bufferId=-1, isPers = 0;
	XPTR target = 0;
	VersionsHeader *header = NULL;

	if (!LookupFlushingDependency(xptr, &target))
	{
		/* internal error */
	}
	else if (!target)
	{
		/* no buffer must be flushed ahead of this one  */
		success = 1;
	}
	else if (!ImpFindBlockInBuffers(target, &bufferId))
	{
		/*
		 * the block to be flushed ahead of the current one not in buffers?
		 * fine for us. But we need to get rid of flush-dependency, since it's
		 * essentially has been fulfilled.
		 */
	    wulog(("WULOG: dependency xptr = %"PRI_XPTR" from xptr = %"PRI_XPTR" isn't in buffers", target, xptr));
		success = (WuGetLastError() == WUERR_BLOCK_NOT_IN_BUFFERS) &&
			UpdateFlushingDependency(xptr, 0);
	}
	else
	{
	    /*
         * We want to make sure we're implementing some kind of 'strict'
         * barrier here. We want recovery version (the one flushed on
         * dependency) to be flushed exactly at this time with respect to other
         * blocks. So we make sure that all blocks are flushed before this
         * version, and then guarantee flush of the version itself.
         */
	    wulog(("WULOG: flushed xptr = %"PRI_XPTR" on dependency from xptr = %"PRI_XPTR, target, xptr));
		success = UpdateFlushingDependency(xptr, 0) && ImpFlushBuffer(bufferId, true);
	}
	if (success)
	{
		success = 0;
		if (ImpFindBlockInBuffers(xptr, &bufferId) &&
			ImpLocateHeader(bufferId, &header) &&
			IsPersSnapshotVersion(header, &isPers))
		{
			if (isPers)
			{
				/* call AK function */
				success = ImpOnPersVersionRelocating(header->xptr[0], xptr, 2);
			}
			success = 1;
		}
	}
	return success;
}

static
void ResetHeader(VersionsHeader *hdr)
{
	int i=0;

	assert(hdr);
	for (i=0; i<VE_VERSIONS_COUNT; ++i)
	{
		hdr->xptr[i] = 0;
		hdr->creatorTs[i] = INVALID_TIMESTAMP;
	}
}

/*	Finds apropriate version of the block identified by lxptr.
	The result depends on the calling client transaction settings,
	including: whether the transaction is running on snapshot,
	the snapshot timestamp (if using snapshot) and the timestamp
	assigned to transaction (if NOT using snapshot).

	The caller have to pass buffers of sufficient size via tsOutBuf and
	idOutBuf params, VE_VERSIONS_COUNT+2 elements per each buffer
	is sufficient. POutSz param should point to a variable initialised
	with the buffer size (in elements). When function returns the
	variable is assigned the number of elements actually
	stored (in one buffer).

	The function is primarily designed as a base for VePutBlockToBuffer
	from the module's public API. Extra parameters correspond to
	SnExpandDfvHeader out parameters. Since SnExpandDfvHeader is rather
	heavy it makes sence to reuse the results obtained from it.
	VeCreateBlockVersion is also implemented on top of PutBlockVersionToBuffer. */
static
int PutBlockVersionToBuffer(LXPTR lxptr, int *pBufferId,
							TIMESTAMP tsOutBuf[],
							int idOutBuf[],
							size_t *pOutSz,
							TIMESTAMP *anchorTs)
{
	int success = 0;
	VeClientState *state = NULL;
	VersionsHeader *versionHeader = NULL;
	VeRestriction restriction = {};
	int trnStatusAndType = 0, bufferId = -1;

	assert(pBufferId); *pBufferId=-1;
	assert(tsOutBuf && pOutSz && anchorTs);
	/* get state data and validate as much as possible */
	if (!ImpGetCurrentStateBlock(&state) ||
		!ImpGetTransactionStatusAndType(&trnStatusAndType) ||
		!LookupRestriction(lxptr, &restriction))
	{
		/* error! */
	}
    // give rolledback transaction read blocks after physical rollback (for ft-indexes sake)
	else if (trnStatusAndType == SN_COMPLETED_TRANSACTION || (state->isCompleted && !state->isRolledBack))
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else if (restriction.type == VE_RESTRICTION_OLD_VERSION)
	{
		WuSetLastErrorMacro(WUERR_GENERAL_ERROR);
	}
	else if (!ImpPutBlockToBuffer(lxptr, &bufferId) ||
			 !ImpLocateHeader(bufferId, &versionHeader))
	{
		/* error! */
	}
	else if (!ValidateHeader(versionHeader) || versionHeader->xptr[0]!=lxptr)
	{
		WuSetLastErrorMacro(WUERR_PERS_DATA_VALIDATION_FAILED);
        AnalyzePopularFailure(__FUNCTION__, lxptr, lxptr, versionHeader);
	}
	/* generic validation passed, now interpret version header */
	else
	{
		int okStatus = 0;

		if (restriction.type == VE_RESTRICTION_DEAD_BLOCK)
		{
		    /* DEAD_BLOCK restriction is not a failure -- we may be reading some snapshot using the version as a navigator */
			TIMESTAMP tsInBuf[VE_VERSIONS_COUNT + 1];
			unsigned i=0;

			/* adding deletorTs allows to catch access on deleted version */
			tsInBuf[0] = restriction.deletorTs;
			memcpy(tsInBuf+1, versionHeader->creatorTs, sizeof(TIMESTAMP)*VE_VERSIONS_COUNT);

			if (ImpExpandDfvHeader(tsInBuf, VE_VERSIONS_COUNT+1, tsOutBuf, idOutBuf, pOutSz, anchorTs, false))
			{
				okStatus = 1;
				for (i=0; i<*pOutSz; ++i) idOutBuf[i]+=-1; /* see comment above on deletorTs */
			}
		}
		else
		{
			okStatus = ImpExpandDfvHeader(versionHeader->creatorTs, VE_VERSIONS_COUNT,
										  tsOutBuf, idOutBuf, pOutSz, anchorTs, false);
		}

		if (!okStatus) {}
		/* updater branch */
		else if (trnStatusAndType == SN_UPDATER_TRANSACTION)
		{
			TIMESTAMP transactionTs = INVALID_TIMESTAMP;
			if (!ImpGetTransactionTs(&transactionTs)) {}
			else if ((restriction.creatorId>=0 &&
					 restriction.creatorId!=ClGetCurrentClientId(setup.clientStateTicket)) ||
					 (tsOutBuf[0] == SN_WORKING_VERSION_TIMESTAMP &&
					 versionHeader->creatorTs[0] != transactionTs))
			{
				WuSetLastErrorMacro(WUERR_WORKING_VERSION_CREATED_BY_ALLY);
			}
			else if (idOutBuf[0]==-1)
			{
				WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
			}
			else
			{
				int isGrantingExclAccess =
					(tsOutBuf[0] == SN_WORKING_VERSION_TIMESTAMP ||
					setup.flags & VE_SETUP_DISABLE_VERSIONS_FLAG);

				success =
					(!isGrantingExclAccess || ImpGrantExclusiveAccessToBuffer(bufferId));
			}
		}
		/* query branch */
		else
		{
			TIMESTAMP snapshotTs = INVALID_TIMESTAMP;
			assert(trnStatusAndType == SN_READ_ONLY_TRANSACTON);
			if (!ImpGetTransactionSnapshotTs(&snapshotTs)) {}
			else
			{
				unsigned i=0;
				while (i<*pOutSz && tsOutBuf[i]!=snapshotTs) ++i;
				if (i==*pOutSz || idOutBuf[i]==-1)
				{
					WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
				}
				else
				{
					XPTR olderXptr = versionHeader->xptr[idOutBuf[i]];
					if (!ImpPutBlockToBuffer(olderXptr, &bufferId) ||
						!ImpLocateHeader(bufferId, &versionHeader))
					{
						/* error! */
					}
					else if (!ValidateHeader(versionHeader) || versionHeader->xptr[0]!=lxptr)
					{
						WuSetLastErrorMacro(WUERR_PERS_DATA_VALIDATION_FAILED);
					}
					else
					{
						success = 1;
					}
				}
			}
		}
	}
	if (success) *pBufferId = bufferId;
	return success;
}

int ComposeHeader(VersionsHeader *pHeaderOut,
				  const VersionsHeader *pHeaderIn,
				  XPTR xptr,
				  TIMESTAMP creatorTs,
				  const TIMESTAMP tsBuf[],
				  const int idBuf[],
				  size_t sz)
{
	int success = 0, prevId = -1;
	unsigned i=0, o=1, l=VE_VERSIONS_COUNT;

/*
	if (pHeaderIn->creatorTs[0]>=creatorTs)
	{
/		WuSetLastErrorMacro(WUERR_BAD_TIMESTAMP);
	}
	else
*/
	{
		ResetHeader(pHeaderOut);

		/* copy all unique entries */
		while (i<sz && o<l)
		{
			if (idBuf[i]!=prevId)
			{
				pHeaderOut->creatorTs[o] = pHeaderIn->creatorTs[idBuf[i]];
				pHeaderOut->xptr[o] = pHeaderIn->xptr[idBuf[i]];
				prevId = idBuf[i];
				++o;
			}
			++i;
		}

		pHeaderOut->creatorTs[0] = creatorTs;
		pHeaderOut->xptr[0] = xptr;
		success = (i==sz || ImpDamageSnapshots(tsBuf[i]));
	}
	return success;
}

int OnCreateVersion(LXPTR lxptr,
					int bufferId,
					XPTR oldVerXptr,
					int oldVerBufferId,
					int isOldVerPers)
{
	int success = 0;

	if (isOldVerPers)
	{
		success = UpdateFlushingDependency(lxptr, oldVerXptr) &&
				  ImpOnPersVersionRelocating(lxptr, oldVerXptr, 1);
	}
	else success = 1;

	return success;
}

int OnFreeBlock(LXPTR lxptr, TIMESTAMP *pAnchorTs)
{
	int success = 0, bufferId = -1;
	VersionsHeader *header = NULL;

	if (!ImpFindBlockInBuffers(lxptr, &bufferId))
	{
		/*	block not in buffers - unable to make an assumption when it is safe to
			discard block, thus not changing pAnchorTs */
		success = (WuGetLastError() == WUERR_BLOCK_NOT_IN_BUFFERS);
	}
	else
	{
        // here we must change pAnchorTs according to the oldest needed version
        // since this version must be accessible by navigation, and bogus version
        // must not be purged before that; it is done through isTotalAnchor parameter

        /*	we are going to update pAnchorTs */
		success = ImpLocateHeader(bufferId, &header) &&
				  ImpExpandDfvHeader(header->creatorTs, VE_VERSIONS_COUNT, NULL, NULL, NULL, pAnchorTs, true);
	}

	return success;
}

int OnTransactionCommit(VeClientState *state, TIMESTAMP currentSnapshotTs)
{
	SnRequestForGc buf[VE_BUFSZ], *ibuf=buf, *ebuf=buf+VE_BUFSZ;
	VeFunctionList::iterator i;
	int success = 0, bufferId = 0;

	wulog(("WULOG: Versions: Starting transaction commit..."));
	if (state->functionList == NULL)
	{
		/* transaction runs on read-only snapshot */
		success=1;
	}
	else
	{
		for (i = state->functionList->begin(); i != state->functionList->end(); ++i)
		{
			if (ibuf==ebuf)
			{
				/* flush buffer */
				if (!ImpSubmitRequestForGc(currentSnapshotTs, buf, VE_BUFSZ)) break;
				ibuf = buf;
			}

			*ibuf=*i;
			/*	QUICK & DIRTY
				need to revoke exclusive access to buffer form trn
				this should normally be a part of buffer manager, consider
			*/
			if (ibuf->type == VE_FUNCTION_ALLOCATE_BLOCK ||
				ibuf->type == VE_FUNCTION_CREATE_VERSION ||
				ibuf->type == VE_FUNCTION_CREATE_VERSION_PERS)
			{
				if (ImpFindBlockInBuffers(ibuf->lxptr, &bufferId))
				{
					if (!ImpRevokeExclusiveAccessToBuffer(bufferId)) break;
				}
				else
				{
					if (WUERR_BLOCK_NOT_IN_BUFFERS != WuGetLastError()) break;
				}
			}
			/*	END OF QUICK & DIRTY
			*/
			switch (i->type)
			{
			case VE_FUNCTION_ALLOCATE_BLOCK:
				ibuf->type = SN_REQUEST_NOP; break;

			case VE_FUNCTION_CREATE_VERSION:
			case VE_FUNCTION_CREATE_VERSION_PERS:
				ibuf->type = SN_REQUEST_ADD_NORMAL_VERSION; break;

			case VE_FUNCTION_FREE_BLOCK:
				ibuf->type = SN_REQUEST_ADD_BOGUS_VERSION; break;
			default:
				assert(0);
			}
			++ibuf;
		}
		success =
			(i == state->functionList->end() && ImpSubmitRequestForGc(currentSnapshotTs, buf, ibuf-buf));
	}
    wulog(("WULOG: Versions: Completed transaction commit..."));
	return success;
}

// fixes xptr-to-flush-to on rollback to make old relocated version come back
// it is guaranteed that xptr-version is in buffers, but not lxptr-version!!!
static int FixFlushXptr(int oldVerBufferId, XPTR xptr, XPTR lxptr)
{
    ramoffs old_offs, dummy; // for buffer_table methods

    assert((*phys_xptrs)[oldVerBufferId] == WuExternaliseXptr(xptr));
    assert(!buffer_table.find(WuExternaliseXptr(xptr), &dummy) && dummy == RamoffsFromBufferId(oldVerBufferId));

    // first, we change flush-xptr to make buffer flush on new location
    (*phys_xptrs)[oldVerBufferId] = WuExternaliseXptr(lxptr);

    // the we must alter buffer_table since it contains phys_xptr --> buf_offset map
    if (!buffer_table.replace(WuExternaliseXptr(lxptr), RamoffsFromBufferId(oldVerBufferId), &old_offs)) // lxptr and xptr in buffers
    {
        buffer_table.replace(WuExternaliseXptr(xptr), old_offs, &dummy);

        /*
         * In fact, we don't need to do this since this version will be
         * discarded shortly after that. But it messes our logs and makes me
         * really uncomfortable...
         *
         * Actually, if we didn't change physXptr here, we should be careful
         * because of flushing dependency here,
         * since we have two blocks with the same physXptr, but this should
         * be fixed by nullifying dependency later (see rollback code).
         */
        (*phys_xptrs)[BufferIdFromRamoffs(old_offs)] = WuExternaliseXptr(xptr);

        wulog(("WULOG: Rollback: cancelling creation of a new version: version header for returned LC-version..."));
        wulogheader(oldVerBufferId);

        wulog(("WULOG: Rollback: cancelling creation of a new version: version header for discarded old version..."));
        wulogheader(BufferIdFromRamoffs(old_offs));
    }
    else // lxptr isn't in memory, but xptr is -- just insert the new record and delete the old one
    {
        buffer_table.insert(WuExternaliseXptr(lxptr), RamoffsFromBufferId(oldVerBufferId));
        buffer_table.remove(WuExternaliseXptr(xptr));

        wulog(("WULOG: Rollback: cancelling creation of a new version: version header for returned LC-version (discarded version isn't in buffers)..."));
        wulogheader(oldVerBufferId);
    }

    return 1;
}

int OnTransactionRollback(VeClientState *state)
{
    VeFunctionList::reverse_iterator i;
    int success = 1, bufferId = 0;
    ramoffs offs;

    wulog(("WULOG: Versions: Starting transaction rollback..."));
    if (state->functionList == NULL)
    {
        /* transaction runs on read-only snapshot -- nothing to do for rollback */
        success = 1;
    }
    else
    {
        for (i = state->functionList->rbegin(); i != state->functionList->rend() && success; i++)
        {
            switch (i->type)
            {
                case VE_FUNCTION_ALLOCATE_BLOCK:
                    // on rollback we just delete the block (safe, since transaction will unmap it anyway)
                    wulog(("WULOG: Rollback: deleted newly allocated block: lxptr = %"PRI_XPTR, i->lxptr));
                    ImpFreeBlock(i->lxptr);
                    break;

                case VE_FUNCTION_CREATE_VERSION:
                case VE_FUNCTION_CREATE_VERSION_PERS:
                    // the worst case: we should delete new version and relocate the old one back
                    // CAREFUL: we don't want to overwrite persistent version here: consider situation when (lxptr; xptr) pair has been flushed to disk
                    //          we relocate xptr on lxptr, delete xptr and then someone reuse xptr --> persistent version is lost --> recovery would fail

                    wulog(("WULOG: Rollback: cancelling creation of a new version: lxptr = %"PRI_XPTR", xptr = %"PRI_XPTR, i->lxptr, i->xptr));

                    // first, put old version in buffers
                    try
                    {
                        success = 0;
                        put_block_to_buffer(-1, WuExternaliseXptr(i->xptr), &offs, true);
                        bufferId = BufferIdFromRamoffs(offs);
                        success = 1;
                    }
                    WU_CATCH_EXCEPTIONS()
                    if (!success) break;

                    // old version is now in buffers: move it to a new physical xpt == lxptr
                    if (!FixFlushXptr(bufferId, i->xptr, i->lxptr))
                    {
                        success = 0;
                        break;
                    }

                    /*
                     * if we had replaced persistent version, make persistent
                     * version flush to avoid relocating it on recovery
                     * this will guarantee that the situation described in the
                     * header of the case will be impossible
                     *
                     * also, we want to nullify flushing dependency since there
                     * is no meaning in flushing 'xptr' --> it'll be deleted in
                     * a moment.
                     */
                    if (i->type == VE_FUNCTION_CREATE_VERSION_PERS)
                    {
			XPTR target;
			if (!LookupFlushingDependency(i->lxptr, &target))
			{
				success = 0;
				break;
			}

			/*
			 * If we still have live dependency, that means the
			 * relocated persistent version is still sitting
			 * at lxptr address, which is fine. So, we don't need
			 * to flush it.
			 */
			if (target)
			{
				// delete dependency
				if (!UpdateFlushingDependency(i->lxptr, 0))
				{
					success = 0;
					break;
				}
			}
			else
			{
				/*
				 * otherwise, we must flush it in sync-mode to
				 * be able to recover later, since its current
				 * physical block will be deleted momentarily.
				 */
				if (!ImpMarkBufferDirty(bufferId) ||
						!ImpFlushBuffer(bufferId, true))
				{
					success = 0;
					break;
				}
			}
                    }
                    else
                    {
			/*
			 * here, we don't care of the exact time of
			 * flushing of a non-persistent new version.
			 * we relocate it and make it dirty.
			 *
			 * persistent version is still safe and sound in its
			 * current place.
			 */
			if (!ImpMarkBufferDirty(bufferId))
			{
				success = 0;
				break;
			}
                    }

                    // delete old version
                    ImpFreeBlock(i->xptr);
                    break;

                case VE_FUNCTION_FREE_BLOCK:
                    // ignore this record on rollback since block'll just stay untouched
                    // we need to revoke exclusive access to this buffer, though
                    wulog(("WULOG: Rollback: returned deleted block: lxptr = %"PRI_XPTR, i->lxptr));

                    if (ImpFindBlockInBuffers(i->lxptr, &bufferId))
                    {
                        if (!ImpRevokeExclusiveAccessToBuffer(bufferId))
                            success = 0;
                    }
                    else
                    {
                        if (WUERR_BLOCK_NOT_IN_BUFFERS != WuGetLastError())
                            success = 0;
                    }

                    break;
                default:
                    WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
                    success = 0;
                    break;
            }
        }

        success = (i == state->functionList->rend());
    }

    wulog(("WULOG: Versions: Completed transaction rollback..."));
    return success;
}

/* public API */

void VeQueryResourceDemand(VeResourceDemand *resourceDemand)
{
	assert(resourceDemand);
	resourceDemand->clientStateSize = sizeof (VeClientState);
}

int VeInitialize()
{
	int success = 0;
	if (isInitialized)
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else
	{
		restrictionHash = NULL;
		isInitialized = 1;
		success = 1;
	}
	return success;
}

void VeDeinitialize()
{
	restrictionHash = NULL;
	flushingDependenciesHash = NULL;
	isInitialized = 0;
}

int VeStartup(VeSetup *psetup)
{
	assert(psetup);
	setup = *psetup;
	if (setup.flags & VE_SETUP_DISABLE_CORRECTNESS_CHECKS_FLAG)
	{
		restrictionHash = NULL;
	}
	else
	{
		restrictionHash = new VeRestrictionHash;
	}
	flushingDependenciesHash = new VeFlushingDependenciesHash;
	return 1;
}

int VeShutdown()
{
	delete restrictionHash; restrictionHash = NULL;
	delete flushingDependenciesHash; flushingDependenciesHash = NULL;
	return 1;
}

int VeOnRegisterClient()
{
	int success = 0, statusAndType = 0;
	VeClientState *state = NULL;

	if (!ImpGetCurrentStateBlock(&state) ||
		!ImpGetTransactionStatusAndType(&statusAndType))
	{
		/* error! */
	}
	else if (statusAndType == SN_READ_ONLY_TRANSACTON && (setup.flags & VE_SETUP_DISABLE_VERSIONS_FLAG))
	{
		WuSetLastErrorMacro(WUERR_VERSIONS_DISABLED);
	}
	else
	{
		state->isCompleted = 0;
        state->isRolledBack = 0;
		state->functionList = NULL;
		state->restrictionList = NULL;
		if (statusAndType == SN_UPDATER_TRANSACTION)
		{
			state->functionList = new VeFunctionList;
		}
		success = 1;
	}
	return success;
}

int VeOnUnregisterClient()
{
	int success=0;
	VeClientState *state = NULL;

	if (!ImpGetCurrentStateBlock(&state) ||
		!DismissRestrictionList(state->restrictionList, 1))
	{
		/* error! */
	}
	else
	{
		delete state->functionList;
		state->functionList = NULL;
		state->restrictionList = NULL;
		success = 1;
	}
	return success;
}

int VePutBlockToBuffer(LXPTR lxptr, int *pBufferId)
{
	size_t outSz = VE_VERSIONS_COUNT+2;
	TIMESTAMP anchorTs = INVALID_TIMESTAMP;
	TIMESTAMP tsOutBuf[VE_VERSIONS_COUNT+2] = {};
	int idOutBuf[VE_VERSIONS_COUNT+2] = {};

	return PutBlockVersionToBuffer(lxptr, pBufferId,
								   tsOutBuf, idOutBuf, &outSz, &anchorTs);
}

int VeAllocateBlock(LXPTR *pLxptr, int *pBufferId)
{
	int success = 0, trnStatusAndType = 0, bufferId = -1;
	VeClientState *state = NULL;
	VersionsHeader *versionsHeader = NULL;
	XPTR xptr = 0;
	TIMESTAMP transactionTs = INVALID_TIMESTAMP;

	assert(pLxptr && pBufferId); *pLxptr = 0; *pBufferId = -1;
	if (!ImpGetCurrentStateBlock(&state) ||
		!ImpGetTransactionStatusAndType(&trnStatusAndType) ||
		!ImpGetTransactionTs(&transactionTs))
	{
		/* error! */
	}
	else if (trnStatusAndType == SN_COMPLETED_TRANSACTION || state->isCompleted)
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else if (trnStatusAndType == SN_READ_ONLY_TRANSACTON)
	{
		WuSetLastErrorMacro(WUERR_SNAPSHOTS_ARE_READ_ONLY);
	}
	else if (!ImpAllocateBlock(&xptr, &bufferId) ||
			 !ImpLocateHeader(bufferId, &versionsHeader))
	{
		/* error! */
	}
	else
	{

		assert(trnStatusAndType == SN_UPDATER_TRANSACTION);
		ResetHeader(versionsHeader);
		versionsHeader->creatorTs[0] = transactionTs;
		versionsHeader->xptr[0] = xptr;

		if (!ImpGrantExclusiveAccessToBuffer(bufferId) ||
			!ImpMarkBufferDirty(bufferId))
		{
			/* error! */
		}
		else if (setup.flags & VE_SETUP_DISABLE_VERSIONS_FLAG)
		{
			success = 1;
		}
		else
		{
			VeFunction function = {};
			VeRestriction restriction = {};

			function.anchorTs = INVALID_TIMESTAMP;
			function.lxptr = xptr;
			function.xptr = xptr;
			function.type = VE_FUNCTION_ALLOCATE_BLOCK;

			restriction.creatorId = ClGetCurrentClientId(setup.clientStateTicket);
			restriction.deletorTs = INVALID_TIMESTAMP;
			restriction.xptr = xptr;

			state->functionList->push_back(function);
			success = UpdateRestriction(&restriction, &state->restrictionList);
		}
	}
	if (success)
	{

        wulog(("WULOG: Allocated new block: lxptr = %"PRI_XPTR", ts = %"PRIx64, xptr, transactionTs));
		*pBufferId = bufferId;
		*pLxptr = xptr;
	}
	return success;
}

// Makes old version flush to a new location and new version flush to the logical xptr
static int FixFlushXptrs(int oldVerBufferId, int newVerBufferId, XPTR newBlock, XPTR lxptr)
{
    ramoffs dummy; // to store result from XptrHash::replace

    assert((*phys_xptrs)[oldVerBufferId] == WuExternaliseXptr(lxptr));

    // first, we change flush-xptrs to make buffers flush on new locations
    (*phys_xptrs)[newVerBufferId] = WuExternaliseXptr(lxptr);
    (*phys_xptrs)[oldVerBufferId] = WuExternaliseXptr(newBlock);

    // the we must alter buffer_table since it contains phys_xptr --> buf_offset map
    buffer_table.replace(WuExternaliseXptr(newBlock), RamoffsFromBufferId(oldVerBufferId), &dummy);
    buffer_table.replace(WuExternaliseXptr(lxptr), RamoffsFromBufferId(newVerBufferId), &dummy);

    return 1;
}

/* Determine if we've got persistent version relocating based on SnExpandDfvHeader output */
static int
VeIsVersionPersistent(int *res, TIMESTAMP tsOut[], int idOut[], size_t outSz, TIMESTAMP persTs)
{
    size_t i = 0;

    // point i to persistent version
    while (i < outSz && tsOut[i] != persTs) ++i;

    // check if we've got relocation -- persistent version equals LC one
    *res = (i < outSz && idOut[0] == idOut[i]);

    return 1;
}

int VeCreateBlockVersion(LXPTR lxptr, int *pBufferId)
{
	int success = 0, trnStatusAndType = 0, bufferId = -1;
	VeClientState *state = NULL;

	assert(pBufferId); *pBufferId = -1;
	if (!ImpGetCurrentStateBlock(&state) ||
		!ImpGetTransactionStatusAndType(&trnStatusAndType))
	{
		/* error! */
	}
	else if (trnStatusAndType == SN_READ_ONLY_TRANSACTON)
	{
		WuSetLastErrorMacro(WUERR_SNAPSHOTS_ARE_READ_ONLY);
	}
	else if (trnStatusAndType == SN_COMPLETED_TRANSACTION || state->isCompleted)
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else if (setup.flags & VE_SETUP_DISABLE_VERSIONS_FLAG)
	{
		WuSetLastErrorMacro(WUERR_VERSIONS_DISABLED);
	}
	else
	{
		TIMESTAMP persTs = INVALID_TIMESTAMP, anchorTs = INVALID_TIMESTAMP, trnTs = INVALID_TIMESTAMP;
		TIMESTAMP tsOutBuf[VE_VERSIONS_COUNT+2] = {};
		int idOutBuf[VE_VERSIONS_COUNT+2] = {};
		size_t outSz = VE_VERSIONS_COUNT+2;
		VersionsHeader *pOldHeader = NULL, *pNewHeader = NULL, header = {};
		XPTR newBlock = 0;
		int newBlockBufferId = -1;
        int isPers = 0;

		if (!ImpGetSnapshotTimestamps(NULL, &persTs) ||
			!ImpGetTransactionTs(&trnTs) ||
			!PutBlockVersionToBuffer(lxptr, &bufferId,
									 tsOutBuf, idOutBuf, &outSz, &anchorTs) ||
			!ImpLocateHeader(bufferId, &pOldHeader))
		{
			/* error! */
		}
		/* First determine if we're relocating persistent version and only THEN filter out "damaged" versions
		 * The order is important since we might get damaged persstent snapshot -- we don't want it filtered out
		 * prematurely
		 */
		else if (!VeIsVersionPersistent(&isPers, tsOutBuf, idOutBuf, outSz, persTs) ||
		         !SnFilterOutDamaged(tsOutBuf, idOutBuf, &outSz))
		{
            /* error! */
		}
		else if (tsOutBuf[0] == SN_WORKING_VERSION_TIMESTAMP)
		{
			WuSetLastErrorMacro(WUERR_WORKING_VERSION_ALREADY_CREATED);
		}
		else if (!ComposeHeader(&header, pOldHeader, lxptr, trnTs, tsOutBuf, idOutBuf, outSz) ||
				 !ImpAllocateBlockAndCopyData(&newBlock, &newBlockBufferId, bufferId) ||
				 !ImpLocateHeader(newBlockBufferId, &pNewHeader))
		{
			/* error! */
		}
		else
		{
			VeRestriction restrictMaster = {}, restrictSlave = {};

			/* store a link to older version in the header */
			header.xptr[1] = newBlock;
			/* prepare restrictions */
			restrictMaster.creatorId = ClGetCurrentClientId(setup.clientStateTicket);
			restrictMaster.xptr = lxptr;
            restrictSlave.type = VE_RESTRICTION_OLD_VERSION;
			restrictSlave.xptr = newBlock;
			/* Old version stays right here; new version is in the newBlock */
			*pNewHeader = header;
			*pBufferId = newBlockBufferId;
			if (!FixFlushXptrs(bufferId, newBlockBufferId, newBlock, lxptr))
			{
                /* error! */
			}
			else if (!ImpMarkBufferDirty(bufferId) ||
				!UpdateRestriction(&restrictMaster, &state->restrictionList) ||
				!UpdateRestriction(&restrictSlave, &state->restrictionList) ||
				!OnCreateVersion(lxptr, bufferId, newBlock, newBlockBufferId, isPers) ||
				!ImpGrantExclusiveAccessToBuffer(newBlockBufferId))
			{
				/* error! */
			}
			else
			{
				VeFunction function = {};

				function.anchorTs = anchorTs;
				function.lxptr = lxptr;
				function.xptr = newBlock;
				function.type = (isPers? VE_FUNCTION_CREATE_VERSION_PERS: VE_FUNCTION_CREATE_VERSION);
				state->functionList->push_back(function);

				success = 1;
			}

	        wulog(("WULOG: Created new version for block: lxptr = %"PRI_XPTR", xptr = %"PRI_XPTR, lxptr, newBlock));
	        wulogheader(newBlockBufferId);
            wulogheader(bufferId);
		}
	}
	if (!success) { *pBufferId = -1; }
	return success;
}

int VeFreeBlock(LXPTR lxptr)
{
	int success = 0, trnStatusAndType = 0;
	TIMESTAMP transactionTs = INVALID_TIMESTAMP;
	VeClientState *state = NULL;
	VeRestriction restriction = {};

	if (!ImpGetCurrentStateBlock(&state) ||
		!ImpGetTransactionStatusAndType(&trnStatusAndType) ||
		!ImpGetTransactionTs(&transactionTs) ||
		!LookupRestriction(lxptr, &restriction))
	{
		/* error! */
	}
	else if (trnStatusAndType == SN_COMPLETED_TRANSACTION || state->isCompleted)
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else if (trnStatusAndType == SN_READ_ONLY_TRANSACTON)
	{
		WuSetLastErrorMacro(WUERR_SNAPSHOTS_ARE_READ_ONLY);
	}
	else
	{
		TIMESTAMP anchorTs = TIMESTAMP_MIN;

		assert(trnStatusAndType == SN_UPDATER_TRANSACTION);
		if (restriction.creatorId > 0 &&
			restriction.creatorId!=ClGetCurrentClientId(setup.clientStateTicket))
		{
			WuSetLastErrorMacro(WUERR_WORKING_VERSION_CREATED_BY_ALLY);
		}
		else if (restriction.type == VE_RESTRICTION_DEAD_BLOCK ||
				 restriction.type == VE_RESTRICTION_OLD_VERSION)
		{
			WuSetLastErrorMacro(WUERR_NO_APROPRIATE_VERSION);
		}
		else if (setup.flags & VE_SETUP_DISABLE_VERSIONS_FLAG)
		{
			success = ImpFreeBlock(lxptr);
		}
		else if (!OnFreeBlock(lxptr, &anchorTs))
		{
			/* error! */
		}
		else
		{
			VeFunction function = {};

			function.type = VE_FUNCTION_FREE_BLOCK;
			function.lxptr = lxptr;
			function.xptr = lxptr;
			function.anchorTs = anchorTs;

			restriction.type = VE_RESTRICTION_DEAD_BLOCK;
			restriction.deletorTs = transactionTs;
			restriction.xptr = lxptr;
			restriction.next = NULL;

			if (!UpdateRestriction(&restriction, &state->restrictionList))
			{
				/* error! */
			}
			else
			{
				state->functionList->push_back(function);
				success = 1;
			}

            wulog(("WULOG: Predeletion of block: lxptr = %"PRI_XPTR", anchorTs = %"PRIx64, lxptr, anchorTs));
		}
	}
	return success;
}

int VeFreeBlockLowAndUpdateRestrictions(XPTR xptr)
{
	return UpdateRestrictionsOnFreeBlock(xptr) && ImpFreeBlock(xptr);
}

int VeClearTransactionRestrictions(int how)
{
	int success = 0;
	VeClientState *state = NULL;

	if (how != VE_ROLLBACK_TRANSACTION && how != VE_COMMIT_TRANSACTION)
	{
		WuSetLastErrorMacro(WUERR_BAD_PARAMS);
	}
	else if (ImpGetCurrentStateBlock(&state) &&
			 DismissRestrictionList(state->restrictionList, how == VE_ROLLBACK_TRANSACTION))
	{
		state->restrictionList = NULL;
		success = 1;
	}
	return success;
}

int VeOnTransactionEnd(int how, TIMESTAMP currentSnapshotTs)
{
	int success = 0;
	VeClientState *state = NULL;

	if (how == VE_COMMIT_TRANSACTION ? !IsValidTimestamp(currentSnapshotTs) : (how != VE_ROLLBACK_TRANSACTION))
	{
		WuSetLastErrorMacro(WUERR_BAD_PARAMS);
	}
	else if (!ImpGetCurrentStateBlock(&state))
	{
		/* error! */
	}
	else if (state->isCompleted)
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else if (setup.flags & VE_SETUP_DISABLE_VERSIONS_FLAG &&
			 how == VE_ROLLBACK_TRANSACTION)
	{
		WuSetLastErrorMacro(WUERR_VERSIONS_DISABLED);
	}
	else if (!VeClearTransactionRestrictions(how))
	{
		/* error! */
	}
	else
	{
		if (setup.flags & VE_SETUP_DISABLE_VERSIONS_FLAG)
		{
			success = 1;
		}
		else if (how == VE_COMMIT_TRANSACTION)
		{
			success = OnTransactionCommit(state, currentSnapshotTs);
		}
		else
		{
			assert(how == VE_ROLLBACK_TRANSACTION);
			success = OnTransactionRollback(state);
		}
		if (success)
        {
            state->isCompleted = 1;
            if (how == VE_ROLLBACK_TRANSACTION)
            {
                state->isRolledBack = 1;
            }
        }
	}
	return success;
}

int VeOnFlushBuffer(XPTR xptr)
{
	return OnFlushBuffer(xptr);
}

int VeOnCheckpoint()
{
	/* we shouldn't reset dependencies here because this might mess up
	the following total buffer flush on checkpoint

	return ResetFlushingDependencies();
	*/
    return 1;
}

void VeDbgDump(int reserved)
{
	fprintf(stderr,"VeDbgDump (not implemented)\n\n");
}
