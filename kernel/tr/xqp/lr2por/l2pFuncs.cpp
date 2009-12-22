#include "l2pFuncs.h"
#include "tr/executor/base/PPOperations.h"

PPOpIn l2pFnAbs(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPNumericFuncs::value_func func = &PPNumericFuncs::fn_abs;

    res = PPOpIn(new PPNumericFuncs(dyn_cxt, opi, params[0], func), 1);

    return res;
}

PPOpIn l2pFnAdjustDateTimeToTimezone(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc2Params::adjustDateTimeToTimezone;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFnDateTimeFunc2Params(dyn_cxt, opi, params[0], params[1], ftype), 1);
    }
    else
    {
        xmlscm_type xtype = xs_dateTime;

        res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);
    }

    return res;
}

PPOpIn l2pFnAdjustDateToTimezone(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc2Params::adjustDateToTimezone;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFnDateTimeFunc2Params(dyn_cxt, opi, params[0], params[1], ftype), 1);
    }
    else
    {
        xmlscm_type xtype = xs_date;

        res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);
    }

    return res;
}

PPOpIn l2pFnAdjustTimeToTimezone(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc2Params::adjustTimeToTimezone;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFnDateTimeFunc2Params(dyn_cxt, opi, params[0], params[1], ftype), 1);
    }
    else
    {
        xmlscm_type xtype = xs_time;

        res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);
    }

    return res;
}

PPOpIn l2pFnAvg(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnSumAvg(dyn_cxt, opi, 1, params[0]), 1);

    return res;
}

PPOpIn l2pFnBaseUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    // we always get 1 parameter here since sema guarantees this
    res = PPOpIn(new PPFnBaseURI(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnBoolean(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnBoolean(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnCeiling(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPNumericFuncs::value_func func = &PPNumericFuncs::fn_ceiling;

    res = PPOpIn(new PPNumericFuncs(dyn_cxt, opi, params[0], func), 1);

    return res;
}

PPOpIn l2pFnCodepointEqual(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnCompare(dyn_cxt, opi, params[0], params[1], true), 1);

    return res;
}

PPOpIn l2pFnCodepointsToString(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnCodePoints2String(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnCollection(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using fn:collection from general fuctions generator!");
}

PPOpIn l2pFnCompare(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFnCompare(dyn_cxt, opi, params[0], params[1]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnCompare(dyn_cxt, opi, params[0], params[1], params[2]), 1);
    }

    return res;
}

PPOpIn l2pFnConcat(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnConcat(dyn_cxt, opi, params), 1);

    return res;
}

PPOpIn l2pFnContains(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    // sema guarantees exactly two parameters here
    res = PPOpIn(PPSubsMatch::PPFnContains(dyn_cxt, opi, params[0], params[1]), 1);

    return res;
}

PPOpIn l2pFnCount(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnCount(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnCurrentDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int type = PPFnDateTimeFuncNoParam::currentDate;

    res = PPOpIn(new PPFnDateTimeFuncNoParam(dyn_cxt, opi, type), 1);

    return res;
}

PPOpIn l2pFnCurrentDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int type = PPFnDateTimeFuncNoParam::currentDateTime;

    res = PPOpIn(new PPFnDateTimeFuncNoParam(dyn_cxt, opi, type), 1);

    return res;
}

PPOpIn l2pFnCurrentTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int type = PPFnDateTimeFuncNoParam::currentTime;

    res = PPOpIn(new PPFnDateTimeFuncNoParam(dyn_cxt, opi, type), 1);

    return res;
}

PPOpIn l2pFnData(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnData(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc2Params::dateTime;

    res = PPOpIn(new PPFnDateTimeFunc2Params(dyn_cxt, opi, params[0], params[1], ftype), 1);

    return res;
}

PPOpIn l2pFnDayFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::dayFromDate;
    xmlscm_type xtype = xs_date;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnDayFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::dayFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnDaysFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::daysFromDuration;
    xmlscm_type xtype = xs_duration;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnDeepEqual(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFnDeepEqual(dyn_cxt, opi, params[0], params[1]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnDeepEqual(dyn_cxt, opi, params[0], params[1], params[2]), 1);
    }

    return res;
}

PPOpIn l2pFnDefaultCollation(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnDefaultCollation(dyn_cxt, opi), 1);

    return res;
}

PPOpIn l2pFnDistinctValues(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 1)
    {
        res = PPOpIn(new PPFnDistinctValues(dyn_cxt, opi, params[0]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnDistinctValues(dyn_cxt, opi, params[0], params[1]), 1);
    }

    return res;
}

PPOpIn l2pFnDoc(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using fn:doc from general fuctions generator!");
}

PPOpIn l2pFnDocAvailable(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnDocAvailable(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnDocument(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using fn:document from general fuctions generator!");
}

PPOpIn l2pFnDocumentUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnDocumentURI(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnEmpty(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnEmpty(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnEncodeForUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnEndsWith(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnError(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}
PPOpIn l2pFnEscapeHtmlUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}
PPOpIn l2pFnExactlyOne(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnExists(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}
PPOpIn l2pFnFalse(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnFilterEntryLevel(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnFloor(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPNumericFuncs::value_func func = &PPNumericFuncs::fn_floor;

    res = PPOpIn(new PPNumericFuncs(dyn_cxt, opi, params[0], func), 1);

    return res;
}

PPOpIn l2pFnFtHighlight(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnFtHighlightBlocks(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnFtindexScan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnFtscan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnWindexScan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnHoursFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::hoursFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnHoursFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::hoursFromDuration;
    xmlscm_type xtype = xs_duration;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnHoursFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::hoursFromTime;
    xmlscm_type xtype = xs_time;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnId(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnIdref(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnImplicitTimezone(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int type = PPFnDateTimeFuncNoParam::implicitTimezone;

    res = PPOpIn(new PPFnDateTimeFuncNoParam(dyn_cxt, opi, type), 1);

    return res;
}

PPOpIn l2pFnIndexOf(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnIndexScan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnIndexScanBetween(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnInscopePrefixes(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnInsertBefore(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnIriToUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnIsAncestor(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnItemAt(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnLang(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}
PPOpIn l2pFnLast(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnLocalName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnLocalNameFromQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnLowercase(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnMatches(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnMax(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnMin(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnMinutesFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::minutesFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnMinutesFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::minutesFromDuration;
    xmlscm_type xtype = xs_duration;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnMinutesFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::minutesFromTime;
    xmlscm_type xtype = xs_time;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnMonthFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::monthFromDate;
    xmlscm_type xtype = xs_date;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnMonthFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::monthFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnMonthsFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::monthsFromDuration;
    xmlscm_type xtype = xs_duration;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnNamespaceUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnNamespaceUriForPrefix(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnNamespaceUriFromQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnNilled(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnNodeKind(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnNodeName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnNormalizeSpace(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnNormalizeUnicode(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnNot(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnNumber(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnNoneOrMore(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnPosition(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}
PPOpIn l2pFnPrefixFromQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnRemove(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnReplace(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnResolveQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnResolveUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnReverse(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnRoot(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnRound(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPNumericFuncs::value_func func = &PPNumericFuncs::fn_round;

    res = PPOpIn(new PPNumericFuncs(dyn_cxt, opi, params[0], func), 1);

    return res;
}

PPOpIn l2pFnRoundHalfToEven(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSecondsFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::secondsFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnSecondsFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::secondsFromDuration;
    xmlscm_type xtype = xs_duration;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnSecondsFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::secondsFromTime;
    xmlscm_type xtype = xs_time;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnStartsWith(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnStaticBaseUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnString(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnstringJoin(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnStringLength(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnStringToCodepoints(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnStringValue(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSubsequence(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSubstring(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSubstringAfter(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSubstringBefore(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSum(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnTest(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnTimezoneFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::timezoneFromDate;
    xmlscm_type xtype = xs_date;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnTimezoneFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::timezoneFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnTimezoneFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::timezoneFromTime;
    xmlscm_type xtype = xs_time;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnTokenize(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnTrace(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnTranslate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnTrue(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnTypedValue(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnUnordered(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnUppercase(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnYearFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::yearFromDate;
    xmlscm_type xtype = xs_date;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnYearFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::yearFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnYearsFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    int ftype = PPFnDateTimeFunc::yearsFromDuration;
    xmlscm_type xtype = xs_duration;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnZeroOrOne(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSqlClose(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSqlCommit(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSqlConnect(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSqlExecUpdate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSqlExecute(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSqlPrepare(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSqlRollback(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSeCheckpoint(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}

PPOpIn l2pFnSeGetProperty(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
}
