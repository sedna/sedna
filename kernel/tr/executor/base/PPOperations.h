/*
 * File:  PPOperations.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPOPERATIONS_H
#define _PPOPERATIONS_H

#include "PPQueryRoot.h"
#include "PPInsertTo.h"
#include "PPInsertFollowing.h"
#include "PPInsertBefore.h"
#include "PPDeleteDeep.h"
#include "PPDeleteUndeep.h"
#include "PPRename.h"
#include "PPBulkLoad.h"
#include "PPCreateIndex.h"
#include "PPDropIndex.h"
#include "PPCreateMetadata.h"
#include "PPDropMetadata.h"
#include "PPReplace.h"
#include "PPRetrieveDS.h"
#include "PPRetrieveMetadata.h"
#include "PPSLStub.h"
#include "PPIf.h"
#include "PPAbsPath.h"
#include "PPIndexScan.h"
#ifdef SE_ENABLE_FTSEARCH
#include "PPFtIndexScan.h"
#include "PPFtScan.h"
#include "PPFtHighlight.h"
#include "PPCreateFtIndex.h"
#include "PPDropFtIndex.h"
#endif
#include "PPReturn.h"
#include "PPSelect.h"
#include "PPLet.h"
#include "PPOrderBy.h"
#include "PPVariable.h"
#include "PPConst.h"
#include "PPNil.h"
#include "PPSequenceTypes.h"
#include "PPDDO.h"
#include "PPSequence.h"
#include "PPBooleanOps.h"
#include "PPAccessors.h"
#include "PPFnAccessors.h"
#include "PPSequenceOps.h"
#include "PPNodeOps.h"
#include "PPStore.h"
#include "PPAxisChild.h"
#include "PPAxisAttribute.h"
#include "PPAxisFP.h"
#include "PPAxisSibling.h"
#include "PPAxisAncestor.h"
#include "PPAxisParent.h"
#include "PPAxisDescendant.h"
#include "PPAxisSelf.h"
#include "PPCalculate.h"
#include "PPConstructors.h"
#include "PPAggrFuncs.h"
#include "PPGeneralComparison.h"
#include "PPConGen.h"
#include "PPSubsMatch.h"
#include "PPPatMatch.h"
#include "PPNodeComparison.h"
#include "PPFunCall.h"
#include "PPSpaceSequence.h"
#include "PPError.h"
#include "PPTest.h"
#include "PPDocInCol.h"
#include "PPTuple.h"
#include "PPPred.h"
#include "PPScan.h"
#include "PPUnion.h"
#include "PPIntersect.h"
#include "PPExcept.h"
#include "PPADFilter.h"
#include "PPFnDateTimeFuncs.h"
#include "PPDAFilter.h"
#include "PPUp.h"
#include "PPStringFuncs.h"
#include "PPRange.h"
#include "PPCheckpoint.h"
#include "PPFilterEL.h"
#include "PPFnDeepEqual.h"
#ifdef SQL_CONNECTION
#include "PPSQL.h"
#endif

#endif

