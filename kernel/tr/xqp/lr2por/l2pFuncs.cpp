/*
 * File:  l2pFuncs.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "l2pFuncs.h"
#include "tr/executor/base/PPOperations.h"
#include "tr/tr_globals.h"

#include "tr/executor/xqops/PPNumericFuncs.h"
#include "tr/executor/xqops/PPFnDateTimeFuncs.h"
#include "tr/executor/xqops/PPSubsMatch.h"
#include "tr/executor/xqops/PPStringFuncs.h"
#include "tr/executor/xqops/PPUriFuncs.h"
#include "tr/executor/xqops/PPAggrFuncs.h"
#include "tr/executor/xqops/PPFnAccessors.h"
#include "tr/executor/xqops/PPStringsCompare.h"
#include "tr/executor/xqops/PPSequenceOps.h"
#include "tr/executor/xqops/PPDocInCol.h"
#include "tr/executor/xqops/PPError.h"
#include "tr/executor/xqops/PPBooleanOps.h"
#include "tr/executor/xqops/PPFilterEL.h"

#ifdef SE_ENABLE_DTSEARCH
#include "tr/executor/xqops/PPFtScan.h"
#endif

#ifdef SE_ENABLE_FTSEARCH
#include "tr/executor/xqops/PPFtIndexScan.h"
#include "tr/executor/xqops/PPFtIndexDict.h"
#include "tr/executor/xqops/PPFtHighlight.h"
#endif /* SE_ENABLE_FTSEARCH */

#include "tr/executor/xqops/PPPatMatch.h"
#include "tr/executor/xqops/PPCheckpoint.h"
#include "tr/executor/xqops/PPFnGetProperty.h"
#include "tr/executor/xqops/PPFnDeepEqual.h"
#include "tr/executor/xqops/PPFnDocAvailable.h"
#include "tr/executor/xqops/PPNodeOps.h"
#include "tr/executor/xqops/PPQName.h"
#include "tr/executor/xqops/PPTest.h"
#include "tr/executor/xqops/PPNodeComparison.h"
#include "tr/executor/xqops/PPSQL.h"

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

    if (params.size() == 2)
    {
        PPFnDateTimeFunc2Params::dateTimeFuncs ftype = PPFnDateTimeFunc2Params::adjustDateTimeToTimezone;
        res = PPOpIn(new PPFnDateTimeFunc2Params(dyn_cxt, opi, params[0], params[1], ftype), 1);
    }
    else
    {
        PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::adjustDateTimeToTimezone;
        xmlscm_type xtype = xs_dateTime;

        res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);
    }

    return res;
}

PPOpIn l2pFnAdjustDateToTimezone(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 2)
    {
        PPFnDateTimeFunc2Params::dateTimeFuncs ftype = PPFnDateTimeFunc2Params::adjustDateToTimezone;
        res = PPOpIn(new PPFnDateTimeFunc2Params(dyn_cxt, opi, params[0], params[1], ftype), 1);
    }
    else
    {
        PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::adjustDateToTimezone;
        xmlscm_type xtype = xs_date;

        res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);
    }

    return res;
}

PPOpIn l2pFnAdjustTimeToTimezone(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 2)
    {
        PPFnDateTimeFunc2Params::dateTimeFuncs ftype = PPFnDateTimeFunc2Params::adjustTimeToTimezone;
        res = PPOpIn(new PPFnDateTimeFunc2Params(dyn_cxt, opi, params[0], params[1], ftype), 1);
    }
    else
    {
        PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::adjustTimeToTimezone;
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
    throw USER_EXCEPTION2(SE4001, "using fn:collection from general functions generator!");
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

    PPFnDateTimeFuncNoParam::dateTimeFuncs type = PPFnDateTimeFuncNoParam::currentDate;

    res = PPOpIn(new PPFnDateTimeFuncNoParam(dyn_cxt, opi, type), 1);

    return res;
}

PPOpIn l2pFnCurrentDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFuncNoParam::dateTimeFuncs type = PPFnDateTimeFuncNoParam::currentDateTime;

    res = PPOpIn(new PPFnDateTimeFuncNoParam(dyn_cxt, opi, type), 1);

    return res;
}

PPOpIn l2pFnCurrentTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFuncNoParam::dateTimeFuncs type = PPFnDateTimeFuncNoParam::currentTime;

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

    PPFnDateTimeFunc2Params::dateTimeFuncs ftype = PPFnDateTimeFunc2Params::dateTime;

    res = PPOpIn(new PPFnDateTimeFunc2Params(dyn_cxt, opi, params[0], params[1], ftype), 1);

    return res;
}

PPOpIn l2pFnDayFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::dayFromDate;
    xmlscm_type xtype = xs_date;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnDayFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::dayFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnDaysFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::daysFromDuration;
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
    PPOpIn res;

    // here we always get two parameters since fn:doc() with one arg becomes PPAbsPath in lr2por
    res = PPOpIn(new PPDocInCol(dyn_cxt, opi, params[1], params[0]), 1);

    return res;
}

PPOpIn l2pFnDocAvailable(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 1) {
        res = PPOpIn(new PPFnDocAvailable(dyn_cxt, opi, params[0]), 1);
    } else {
        res = PPOpIn(new PPFnDocAvailable(dyn_cxt, opi, params[0], params[1]), 1);
    }

    return res;
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
    PPOpIn res;

    res = PPOpIn(new PPFnUriEncoding(dyn_cxt, opi, params[0], PPFnUriEncoding::ENCODE_FOR_URI), 1);

    return res;
}

PPOpIn l2pFnEndsWith(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPFnStartsEndsWith::FunctionType type = PPFnStartsEndsWith::FN_ENDS_WITH;
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPFnStartsEndsWith(dyn_cxt, opi, params[0], params[1], params[2], type), 1);
    }
    else
    {
        res = PPOpIn(new PPFnStartsEndsWith(dyn_cxt, opi, params[0], params[1], type), 1);
    }

    return res;
}

PPOpIn l2pFnError(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn child_err, child_descr, child_obj, res;

    if (params.size() == 3)
        child_obj = params[2];

    if (params.size() >= 2)
        child_descr = params[1];

    if (params.size() >= 1)
        child_err = params[0];

    res = PPOpIn(new PPFnError(dyn_cxt, opi, child_err, child_descr, child_obj), 1);

    return res;
}

PPOpIn l2pFnEscapeHtmlUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnUriEncoding(dyn_cxt, opi, params[0], PPFnUriEncoding::ESCAPE_HTML_URI), 1);

    return res;
}

PPOpIn l2pFnExactlyOne(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnExactlyOne(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnExists(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnExists(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnFalse(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnFalse(dyn_cxt, opi), 1);

    return res;
}

PPOpIn l2pFnFilterEntryLevel(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFilterEL(dyn_cxt, opi, params[0]), 1);

    return res;
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
#ifdef SE_ENABLE_FTSEARCH
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPFtHighlight(dyn_cxt, opi, params[0], params[1], params[2], false), 1);
    }
    else
    {
        res = PPOpIn(new PPFtHighlight(dyn_cxt, opi, params[0], params[1], false), 1);
    }

    return res;
#else
    throw USER_EXCEPTION2(SE1002, "full-text search support is disabled");
#endif
}

PPOpIn l2pFnFtHighlightBlocks(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
#ifdef SE_ENABLE_FTSEARCH
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPFtHighlight(dyn_cxt, opi, params[0], params[1], params[2], true), 1);
    }
    else
    {
        res = PPOpIn(new PPFtHighlight(dyn_cxt, opi, params[0], params[1], true), 1);
    }

    return res;
#else
    throw USER_EXCEPTION2(SE1002, "full-text search support is disabled");
#endif
}

PPOpIn l2pFnFtIndexScan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
#ifdef SE_ENABLE_FTSEARCH
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPFtIndexScan(dyn_cxt, opi, params[0], params[1], params[2]), 1);
    }
    else
    {
        res = PPOpIn(new PPFtIndexScan(dyn_cxt, opi, params[0], params[1]), 1);
    }

    return res;

#else
    throw USER_EXCEPTION2(SE1002, "full-text search support disabled");
#endif
}

PPOpIn l2pFnFtIndexDict(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
#ifdef SE_ENABLE_FTSEARCH
    PPOpIn res;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFtIndexDict(dyn_cxt, opi, params[0], params[1]), 1);
    }
    else
    {
        res = PPOpIn(new PPFtIndexDict(dyn_cxt, opi, params[0]), 1);
    }

    return res;

#else
    throw USER_EXCEPTION2(SE1002, "full-text search support disabled");
#endif
}

PPOpIn l2pFnFtScan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
	//TODO: make PPFtScan for native indexes
#ifdef SE_ENABLE_DTSEARCH
    PPOpIn res;

    if (params.size() == 4)
    {
        res = PPOpIn(new PPFtScan(dyn_cxt, opi, params[0], params[1], params[2], params[3]), 1);
    }
    else
    {
        res = PPOpIn(new PPFtScan(dyn_cxt, opi, params[0], params[1], params[2]), 1);
    }

    return res;
#else
    throw USER_EXCEPTION2(SE1002, "full-text search support is disabled");
#endif
}

PPOpIn l2pFnFtWIndexScan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
#ifdef SE_ENABLE_FTSEARCH
	//TODO: remove PPFtIndexScan2
	if (tr_globals::is_ro_mode)
		throw USER_EXCEPTION2(SE1002, "dtSearch index search support is disabled in RO-mode");

    PPOpIn res;

    if (params.size() == 4)
    {
        res = PPOpIn(new PPFtIndexScan2(dyn_cxt, opi, params[0], params[1], params[2], params[3]), 1);
    }
    else if (params.size() == 3)
    {
        res = PPOpIn(new PPFtIndexScan2(dyn_cxt, opi, params[0], params[1], params[2]), 1);
    }
    else
    {
        res = PPOpIn(new PPFtIndexScan2(dyn_cxt, opi, params[0], params[1]), 1);
    }

    return res;

#else
    throw USER_EXCEPTION2(SE1002, "full-text search support disabled");
#endif
}

PPOpIn l2pFnHoursFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::hoursFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnHoursFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::hoursFromDuration;
    xmlscm_type xtype = xs_duration;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnHoursFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::hoursFromTime;
    xmlscm_type xtype = xs_time;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnId(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using fn:id from general functions generator!");
}

PPOpIn l2pFnIdref(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using fn:idref from general functions generator!");
}

PPOpIn l2pFnImplicitTimezone(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFuncNoParam::dateTimeFuncs type = PPFnDateTimeFuncNoParam::implicitTimezone;

    res = PPOpIn(new PPFnDateTimeFuncNoParam(dyn_cxt, opi, type), 1);

    return res;
}

PPOpIn l2pFnIndexOf(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPFnIndexOf(dyn_cxt, opi, params[0], params[1], params[2]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnIndexOf(dyn_cxt, opi, params[0], params[1]), 1);
    }

    return res;
}

PPOpIn l2pFnIndexScan(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using index-scan from general functions generator!");
}

PPOpIn l2pFnIndexKeys(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using index-keys from general functions generator!");
}

PPOpIn l2pFnIndexScanBetween(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using index-scan-between from general functions generator!");
}

PPOpIn l2pFnInscopePrefixes(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnInScopePrefixes(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnInsertBefore(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnInsertBefore(dyn_cxt, opi, params[0], params[1], params[2]), 1);

    return res;
}

PPOpIn l2pFnIriToUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnUriEncoding(dyn_cxt, opi, params[0], PPFnUriEncoding::IRI_TO_URI), 1);

    return res;
}

PPOpIn l2pFnIsAncestor(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(PPNodeComparison::PPANNodeComparison(dyn_cxt, opi, params[0], params[1]), 1);

    return res;
}

PPOpIn l2pFnItemAt(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnItemAt(dyn_cxt, opi, params[0], params[1]), 1);

    return res;
}

PPOpIn l2pFnLang(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using fn:lang() from general functions generator!");
}

PPOpIn l2pFnLast(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using fn:last() from general functions generator!");
}

PPOpIn l2pFnLocalName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnLocalName(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnLocalNameFromQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnLocalNameFromQName(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnLowercase(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnChangeCase(dyn_cxt, opi, params[0], false), 1);

    return res;
}

PPOpIn l2pFnMatches(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPPatMatch(dyn_cxt, opi, params[0], params[1], params[2], PPPatMatch::PM_MATCH), 1);
    }
    else
    {
        res = PPOpIn(new PPPatMatch(dyn_cxt, opi, params[0], params[1], PPPatMatch::PM_MATCH), 1);
    }

    return res;
}

PPOpIn l2pFnMax(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFnMaxMin(dyn_cxt, opi, 0, params[0], params[1]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnMaxMin(dyn_cxt, opi, 0, params[0]), 1);
    }

    return res;
}

PPOpIn l2pFnMin(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFnMaxMin(dyn_cxt, opi, 1, params[0], params[1]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnMaxMin(dyn_cxt, opi, 1, params[0]), 1);
    }

    return res;
}

PPOpIn l2pFnMinutesFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::minutesFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnMinutesFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::minutesFromDuration;
    xmlscm_type xtype = xs_duration;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnMinutesFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::minutesFromTime;
    xmlscm_type xtype = xs_time;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnMonthFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::monthFromDate;
    xmlscm_type xtype = xs_date;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnMonthFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::monthFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnMonthsFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::monthsFromDuration;
    xmlscm_type xtype = xs_duration;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnName(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnNamespaceUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnNamespaceUri(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnNamespaceUriForPrefix(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnNamespaceUriForPrefix(dyn_cxt, opi, params[0], params[1]), 1);

    return res;
}

PPOpIn l2pFnNamespaceUriFromQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnNamespaceUriFromQName(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnNilled(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnNilled(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnNodeName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnNodeName(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnNormalizeSpace(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnNormalizeSpace(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnNormalizeUnicode(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFnNormalizeUnicode(dyn_cxt, opi, params[0], params[1]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnNormalizeUnicode(dyn_cxt, opi, params[0]), 1);
    }

    return res;
}

PPOpIn l2pFnNot(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnNot(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnNumber(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnNumber(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnNoneOrMore(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnOneOrMore(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnPosition(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using fn:position() from general functions generator!");
}

PPOpIn l2pFnPrefixFromQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnPrefixFromQName(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnQName(dyn_cxt, opi, params[0], params[1]), 1);

    return res;
}

PPOpIn l2pFnRemove(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnRemove(dyn_cxt, opi, params[0], params[1]), 1);

    return res;
}

PPOpIn l2pFnReplace(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 4)
    {
        res = PPOpIn(new PPPatMatch(dyn_cxt, opi, params[0], params[1], params[2], params[3], PPPatMatch::PM_REPLACE), 1);
    }
    else
    {
        res = PPOpIn(new PPPatMatch(dyn_cxt, opi, params[0], params[1], params[2], PPPatMatch::PM_REPLACE), 1);
    }

    return res;
}

PPOpIn l2pFnResolveQName(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnResolveQName(dyn_cxt, opi, params[0], params[1]), 1);

    return res;
}

PPOpIn l2pFnResolveUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFnResolveUri(dyn_cxt, opi, params[0], params[1]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnResolveUri(dyn_cxt, opi, params[0]), 1);
    }

    return res;
}

PPOpIn l2pFnReverse(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnReverse(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnRoot(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnRoot(dyn_cxt, opi, params[0]), 1);

    return res;
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
    PPOpIn res;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFnRoundHalfToEven(dyn_cxt, opi, params[0], params[1]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnRoundHalfToEven(dyn_cxt, opi, params[0], 0), 1);
    }

    return res;
}

PPOpIn l2pFnSecondsFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::secondsFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnSecondsFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::secondsFromDuration;
    xmlscm_type xtype = xs_duration;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnSecondsFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::secondsFromTime;
    xmlscm_type xtype = xs_time;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnStartsWith(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPFnStartsEndsWith::FunctionType type = PPFnStartsEndsWith::FN_STARTS_WITH;
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPFnStartsEndsWith(dyn_cxt, opi, params[0], params[1], params[2], type), 1);
    }
    else
    {
        res = PPOpIn(new PPFnStartsEndsWith(dyn_cxt, opi, params[0], params[1], type), 1);
    }

    return res;
}

PPOpIn l2pFnStaticBaseUri(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnStaticBaseUri(dyn_cxt, opi), 1);

    return res;
}

PPOpIn l2pFnString(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnString(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnstringJoin(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnStringJoin(dyn_cxt, opi, params[0], params[1]), 1);

    return res;
}

PPOpIn l2pFnStringLength(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnStringLength(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnStringToCodepoints(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnString2CodePoints(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnStringValue(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPDmStringValue(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnSubsequence(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPFnSubsequence(dyn_cxt, opi, params[0], params[1], params[2]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnSubsequence(dyn_cxt, opi, params[0], params[1]), 1);
    }

    return res;
}

PPOpIn l2pFnSubstring(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPFnSubstring(dyn_cxt, opi, params[0], params[1], params[2]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnSubstring(dyn_cxt, opi, params[0], params[1]), 1);
    }

    return res;
}

PPOpIn l2pFnSubstringAfter(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPFnSubsBeforeAfter(dyn_cxt, opi, params[0], params[1], params[2], PPFnSubsBeforeAfter::FN_AFTER), 1);
    }
    else
    {
        res = PPOpIn(new PPFnSubsBeforeAfter(dyn_cxt, opi, params[0], params[1], PPFnSubsBeforeAfter::FN_AFTER), 1);
    }

    return res;
}

PPOpIn l2pFnSubstringBefore(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPFnSubsBeforeAfter(dyn_cxt, opi, params[0], params[1], params[2], PPFnSubsBeforeAfter::FN_BEFORE), 1);
    }
    else
    {
        res = PPOpIn(new PPFnSubsBeforeAfter(dyn_cxt, opi, params[0], params[1], PPFnSubsBeforeAfter::FN_BEFORE), 1);
    }

    return res;
}

PPOpIn l2pFnSum(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 2)
    {
        res = PPOpIn(new PPFnSumAvg(dyn_cxt, opi, 0, params[0], params[1]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnSumAvg(dyn_cxt, opi, 0, params[0]), 1);
    }

    return res;
}

PPOpIn l2pFnTest(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPTest(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnTimezoneFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::timezoneFromDate;
    xmlscm_type xtype = xs_date;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnTimezoneFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::timezoneFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnTimezoneFromTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::timezoneFromTime;
    xmlscm_type xtype = xs_time;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnTokenize(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPPatMatch(dyn_cxt, opi, params[0], params[1], params[2], PPPatMatch::PM_TOKENIZE), 1);
    }
    else
    {
        res = PPOpIn(new PPPatMatch(dyn_cxt, opi, params[0], params[1], PPPatMatch::PM_TOKENIZE), 1);
    }

    return res;
}

PPOpIn l2pFnTrace(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnTrace(dyn_cxt, opi, params[0], params[1]), 1);

    return res;
}

PPOpIn l2pFnTranslate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnTranslate(dyn_cxt, opi, params[0], params[1], params[2]), 1);

    return res;
}

PPOpIn l2pFnTrue(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnTrue(dyn_cxt, opi), 1);

    return res;
}

PPOpIn l2pFnTypedValue(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPDmTypedValue(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnUnordered(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    throw USER_EXCEPTION2(SE4001, "using fn:unordered() from general functions generator!");
}

PPOpIn l2pFnUppercase(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnChangeCase(dyn_cxt, opi, params[0], true), 1);

    return res;
}

PPOpIn l2pFnYearFromDate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::yearFromDate;
    xmlscm_type xtype = xs_date;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnYearFromDateTime(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::yearFromDateTime;
    xmlscm_type xtype = xs_dateTime;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnYearsFromDuration(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    PPFnDateTimeFunc::dateTimeFuncs ftype = PPFnDateTimeFunc::yearsFromDuration;
    xmlscm_type xtype = xs_duration;

    res = PPOpIn(new PPFnDateTimeFunc(dyn_cxt, opi, params[0], ftype, xtype), 1);

    return res;
}

PPOpIn l2pFnZeroOrOne(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnZeroOrOne(dyn_cxt, opi, params[0]), 1);

    return res;
}

PPOpIn l2pFnSqlClose(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
#ifdef SQL_CONNECTION
    PPOpIn res;

    res = PPOpIn(new PPFnSQLClose(dyn_cxt, opi, params[0]), 1);

    return res;
#else
    throw USER_EXCEPTION(SE2113);
#endif
}

PPOpIn l2pFnSqlCommit(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
#ifdef SQL_CONNECTION
    PPOpIn res;

    res = PPOpIn(new PPFnSQLCommit(dyn_cxt, opi, params[0]), 1);

    return res;
#else
    throw USER_EXCEPTION(SE2113);
#endif
}

PPOpIn l2pFnSqlConnect(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
#ifdef SQL_CONNECTION
    PPOpIn res;

    res = PPOpIn(new PPFnSQLConnect(dyn_cxt, opi, params), 1);

    return res;
#else
    throw USER_EXCEPTION(SE2113);
#endif
}

PPOpIn l2pFnSqlExecUpdate(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
#ifdef SQL_CONNECTION
    PPOpIn res;

    res = PPOpIn(new PPFnSQLExecute(dyn_cxt, opi, params, true), 1);

    return res;
#else
    throw USER_EXCEPTION(SE2113);
#endif
}

PPOpIn l2pFnSqlExecute(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
#ifdef SQL_CONNECTION
    PPOpIn res;

    res = PPOpIn(new PPFnSQLExecute(dyn_cxt, opi, params, false), 1);

    return res;
#else
    throw USER_EXCEPTION(SE2113);
#endif
}

PPOpIn l2pFnSqlPrepare(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
#ifdef SQL_CONNECTION
    PPOpIn res;

    if (params.size() == 3)
    {
        res = PPOpIn(new PPFnSQLPrepare(dyn_cxt, opi, params[0], params[1], params[2]), 1);
    }
    else
    {
        res = PPOpIn(new PPFnSQLPrepare(dyn_cxt, opi, params[0], params[1]), 1);
    }

    return res;
#else
    throw USER_EXCEPTION(SE2113);
#endif
}

PPOpIn l2pFnSqlRollback(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
#ifdef SQL_CONNECTION
    PPOpIn res;

    res = PPOpIn(new PPFnSQLRollback(dyn_cxt, opi, params[0]), 1);

    return res;
#else
    throw USER_EXCEPTION(SE2113);
#endif
}

PPOpIn l2pFnSeCheckpoint(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPCheckpoint(dyn_cxt, opi), 1);

    return res;
}

PPOpIn l2pFnSeGetProperty(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params)
{
    PPOpIn res;

    res = PPOpIn(new PPFnGetProperty(dyn_cxt, opi, params[0]), 1);

    return res;
}
