#ifndef _OPERATION_HEADERS_H_
#define _OPERATION_HEADERS_H_

#ifdef SE_ENABLE_DTSEARCH
#include "tr/executor/xqops/PPFtScan.h"
#endif

#ifdef SE_ENABLE_FTSEARCH
#include "tr/executor/root/PPCreateFtIndex.h"
#include "tr/executor/root/PPDropFtIndex.h"
#include "tr/executor/xqops/PPFtHighlight.h"
#include "tr/executor/xqops/PPFtIndexDict.h"
#include "tr/executor/xqops/PPFtIndexScan.h"
#endif


#include "tr/executor/root/PPCreateTrigger.h"
#include "tr/executor/root/PPDropTrigger.h"

#include "tr/executor/root/PPBulkLoad.h"
#include "tr/executor/root/PPCreateIndex.h"
#include "tr/executor/root/PPCreateMetadata.h"
#include "tr/executor/root/PPDeleteDeep.h"
#include "tr/executor/root/PPDeleteUndeep.h"
#include "tr/executor/root/PPDropFtIndex.h"
#include "tr/executor/root/PPDropIndex.h"
#include "tr/executor/root/PPDropMetadata.h"
#include "tr/executor/root/PPDropModule.h"
#include "tr/executor/root/PPInsertBefore.h"
#include "tr/executor/root/PPInsertFollowing.h"
#include "tr/executor/root/PPInsertTo.h"
#include "tr/executor/root/PPLoadModule.h"
#include "tr/executor/root/PPQueryRoot.h"
#include "tr/executor/root/PPRename.h"
#include "tr/executor/root/PPReplace.h"
#include "tr/executor/root/PPRetrieveDS.h"
#include "tr/executor/root/PPRetrieveMetadata.h"
#include "tr/executor/root/PPSecurity.h"

#include "tr/executor/xqops/PPAbsPath.h"
#include "tr/executor/xqops/PPAccessors.h"
#include "tr/executor/xqops/PPAggrFuncs.h"
#include "tr/executor/xqops/PPAxisStep.h"
#include "tr/executor/xqops/PPBooleanOps.h"
#include "tr/executor/xqops/PPCalculate.h"
#include "tr/executor/xqops/PPCheckpoint.h"
#include "tr/executor/xqops/PPConst.h"
#include "tr/executor/xqops/PPConstructors.h"
#include "tr/executor/xqops/PPDDO.h"
#include "tr/executor/xqops/PPDocInCol.h"
#include "tr/executor/xqops/PPError.h"
#include "tr/executor/xqops/PPExcept.h"
#include "tr/executor/xqops/PPExplain.h"
#include "tr/executor/xqops/PPExtFunCall.h"
#include "tr/executor/xqops/PPFilterEL.h"
#include "tr/executor/xqops/PPFnAccessors.h"
#include "tr/executor/xqops/PPFnDateTimeFuncs.h"
#include "tr/executor/xqops/PPFnDeepEqual.h"
#include "tr/executor/xqops/PPFnDocAvailable.h"
#include "tr/executor/xqops/PPFnGetProperty.h"
#include "tr/executor/xqops/PPFunCall.h"
#include "tr/executor/xqops/PPGeneralComparison.h"
#include "tr/executor/xqops/PPIf.h"
#include "tr/executor/xqops/PPIndexScan.h"
#include "tr/executor/xqops/PPFnIndexKeys.h"
#include "tr/executor/xqops/PPIntersect.h"
#include "tr/executor/xqops/PPLast.h"
#include "tr/executor/xqops/PPLet.h"
#include "tr/executor/xqops/PPNil.h"
#include "tr/executor/xqops/PPNodeComparison.h"
#include "tr/executor/xqops/PPNodeOps.h"
#include "tr/executor/xqops/PPNumericFuncs.h"
#include "tr/executor/xqops/PPOrderBy.h"
#include "tr/executor/xqops/PPPatMatch.h"
#include "tr/executor/xqops/PPPred.h"
#include "tr/executor/xqops/PPQName.h"
#include "tr/executor/xqops/PPRange.h"
#include "tr/executor/xqops/PPReturn.h"
#include "tr/executor/xqops/PPSelect.h"
#include "tr/executor/xqops/PPSeqChecker.h"
#include "tr/executor/xqops/PPSequence.h"
#include "tr/executor/xqops/PPSequenceOps.h"
#include "tr/executor/xqops/PPSequenceTypes.h"
#include "tr/executor/xqops/PPSpaceSequence.h"
#include "tr/executor/xqops/PPStore.h"
#include "tr/executor/xqops/PPStringFuncs.h"
#include "tr/executor/xqops/PPStringsCompare.h"
#include "tr/executor/xqops/PPSubsMatch.h"
#include "tr/executor/xqops/PPSXptr.h"
#include "tr/executor/xqops/PPTest.h"
#include "tr/executor/xqops/PPTuple.h"
#include "tr/executor/xqops/PPUnion.h"
#include "tr/executor/xqops/PPUriFuncs.h"
#include "tr/executor/xqops/PPVarDecl.h"
#include "tr/executor/xqops/PPVariable.h"

#include "tr/executor/xqops/PPXptr.h"

#include "tr/executor/xqops/PPSQL.h"


#endif /* _OPERATION_HEADERS_H_ */
