#ifndef _L2P_FUNCTIONS_
#define _L2P_FUNCTIONS_

#include "tr/executor/base/PPBase.h"

PPOpIn l2pFnAbs(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnAdjustDateTimeToTimezone(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnAdjustDateToTimezone(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnAdjustTimeToTimezone(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnAvg(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnBaseUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnBoolean(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnCeiling(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnCodepointEqual(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnCodepointsToString(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnCollection(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnCompare(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnConcat(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnContains(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnCount(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnCurrentDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnCurrentDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnCurrentTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnData(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnDayFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnDayFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnDaysFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnDeepEqual(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnDefaultCollation(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnDistinctValues(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnDoc(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnDocAvailable(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnColAvailable(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnDocumentUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnEmpty(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnEncodeForUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnEndsWith(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnError(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnEscapeHtmlUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnExactlyOne(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnExists(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnFalse(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnFilterEntryLevel(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnFloor(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnFtHighlight(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnFtHighlightBlocks(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnFtIndexScan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnFtIndexDict(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnFtScan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnFtWIndexScan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnHoursFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnHoursFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnHoursFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnId(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnIdref(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnImplicitTimezone(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnIndexOf(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnIndexScan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnIndexScanBetween(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnIndexKeys(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnInscopePrefixes(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnInsertBefore(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnIriToUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnIsAncestor(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnItemAt(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnLang(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnLast(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnLocalName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnLocalNameFromQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnLowercase(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnMatches(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnMax(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnMin(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnMinutesFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnMinutesFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnMinutesFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnMonthFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnMonthFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnMonthsFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnNamespaceUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnNamespaceUriForPrefix(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnNamespaceUriFromQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnNilled(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnNodeName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnNormalizeSpace(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnNormalizeUnicode(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnNot(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnNumber(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnNoneOrMore(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnPosition(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnPrefixFromQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnRemove(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnReplace(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnResolveQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnResolveUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnReverse(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnRoot(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnRound(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnRoundHalfToEven(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSecondsFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSecondsFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSecondsFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnStartsWith(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnStaticBaseUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnString(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnstringJoin(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnStringLength(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnStringToCodepoints(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnStringValue(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSubsequence(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSubstring(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSubstringAfter(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSubstringBefore(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSum(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnTest(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnTimezoneFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnTimezoneFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnTimezoneFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnTokenize(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnTrace(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnTranslate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnTrue(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnTypedValue(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnUnordered(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnUppercase(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnYearFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnYearFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnYearsFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnZeroOrOne(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSqlClose(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSqlCommit(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSqlConnect(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSqlExecUpdate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSqlExecute(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSqlPrepare(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSqlRollback(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSeCheckpoint(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnSeGetProperty(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);
PPOpIn l2pFnCryptoSha1(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);

#endif
