/*
 * File: XQFunction.cpp
 * The Institute for System Programming of the Russian Academy of Sciences
 * Copyright (C) 2013 ISP RAS
 */

#include "tr/xqp/XQFunction.h"
#include "tr/xqp/lr2por/l2pFuncs.h"

namespace sedna
{
    // several internal mergers to receive result specs from param specs

    // result determined entirely by the first param spec
    // example: fn:reverse, fn:subsequence, etc.
    xqExprInfo firstArgResult(const std::vector<xqExprInfo> &params)
    {
        return params[0];
    }

    // result is an atomic (e.g. boolean) or one-node
    xqExprInfo resultOne(const std::vector<xqExprInfo> &params)
    {
        xqExprInfo res = {true, true, true, true, false};
        return res;
    }

    // result is an atomic sequence (e.g. xs:integer*)
    // example: fn:data
    xqExprInfo atomicResultMany(const std::vector<xqExprInfo> &params)
    {
        xqExprInfo res = {true, true, false, true, false};
        return res;
    }

    // function may result in any sequence
    xqExprInfo resultUndefined(const std::vector<xqExprInfo> &params)
    {
        xqExprInfo res = {false, false, false, false, true};
        return res;
    }

    // function may result in any sequence, but without tmp-nodes
    xqExprInfo resultUndefinedWoConst(const std::vector<xqExprInfo> &params)
    {
        xqExprInfo res = {false, false, false, false, false};
        return res;
    }

    // standard and additional functions
    XQFunction xqueryFunctions[] =
    {
        {"", "abs",  1,  1, 0xFF, "!fn!abs", false, &resultOne, &l2pFnAbs},
        {"", "adjust-dateTime-to-timezone",  1,  2, 0xFF, "!fn!adjust-dateTime-to-timezone", false, &resultOne, &l2pFnAdjustDateTimeToTimezone},
        {"", "adjust-date-to-timezone",  1,  2, 0xFF, "!fn!adjust-date-to-timezone", false, &resultOne, &l2pFnAdjustDateToTimezone},
        {"", "adjust-time-to-timezone",  1,  2, 0xFF, "!fn!adjust-time-to-timezone", false, &resultOne, &l2pFnAdjustTimeToTimezone},
        {"", "avg",  1,  1, 0xFF, "!fn!avg", false, &resultOne, &l2pFnAvg},
        {"", "base-uri",  0,  1, 0xFF, "!fn!base-uri", false, &resultOne, &l2pFnBaseUri},
        {"", "boolean",  1,  1, 0xFF, "!fn!boolean", false, &resultOne, &l2pFnBoolean},
        {"", "ceiling",  1,  1, 0xFF, "!fn!ceiling", false, &resultOne, &l2pFnCeiling},
        {"", "codepoint-equal",  2,  2, 0xFF, "!fn!codepoint-equal", false, &resultOne, &l2pFnCodepointEqual},
        {"", "codepoints-to-string",  1,  1, 0xFF, "!fn!codepoints-to-string", false, &resultOne, &l2pFnCodepointsToString},
        {"", "collection",  1,  1, 0xFF, "!fn!collection", false, &atomicResultMany, &l2pFnCollection}, // assume result is ddoed, so I use this merger
        {"", "compare",  2,  3, 0xFF, "!fn!compare", false, &resultOne, &l2pFnCompare},
        {"", "concat",  2,  UINT32_MAX, 0xFF, "!fn!concat", false, &resultOne, &l2pFnConcat},
        {"", "contains",  2,  3, 0xFF, "!fn!contains", false, &resultOne, &l2pFnContains},
        {"", "count",  1,  1, 0xFF, "!fn!count", false, &resultOne, &l2pFnCount},
        {"", "current-date",  0,  0, 0xFF, "!fn!current-date", false, &resultOne, &l2pFnCurrentDate},
        {"", "current-dateTime",  0,  0, 0xFF, "!fn!current-dateTime", false, &resultOne, &l2pFnCurrentDateTime},
        {"", "current-time",  0,  0, 0xFF, "!fn!current-time", false, &resultOne, &l2pFnCurrentTime},
        {"", "data",  1, 1, 0, "!fn!data", true, &atomicResultMany, &l2pFnData},
        {"", "dateTime",  2,  2, 0xFF, "!fn!dateTime", false, &resultOne, &l2pFnDateTime},
        {"", "day-from-date",  1,  1, 0xFF, "!fn!day-from-date", false, &resultOne, &l2pFnDayFromDate},
        {"", "day-from-dateTime",  1,  1, 0xFF, "!fn!day-from-dateTime", false, &resultOne, &l2pFnDayFromDateTime},
        {"", "days-from-duration",  1,  1, 0xFF, "!fn!days-from-duration", false, &resultOne, &l2pFnDaysFromDuration},
        {"", "deep-equal",  2,  3, 0xFC, "!fn!deep-equal", true, &resultOne, &l2pFnDeepEqual},
        {"", "default-collation",  0,  0, 0xFF, "!fn!default-collation", false, &resultOne, &l2pFnDefaultCollation},
        {"", "distinct-values",  1,  2, 0xFE, "!fn!distinct-values", false, &atomicResultMany, &l2pFnDistinctValues},
        {"", "doc",  1,  2, 0xFF, "!fn!document", false, &resultOne, &l2pFnDoc},
        {"", "doc-available",  1,  2, 0xFF, "!fn!doc-available", false, &resultOne, &l2pFnDocAvailable},
        {"", "col-available",  1,  1, 0xFF, "!fn!col-available", false, &resultOne, &l2pFnColAvailable},
        {"", "document",  1,  2, 0xFF, "!fn!document", false, &resultOne, &l2pFnDoc},
        {"", "document-uri",  1,  1, 0xFF, "!fn!document-uri", false, &resultOne, &l2pFnDocumentUri},
        {"", "empty",  1,  1, 0xFF, "!fn!empty", false, &resultOne, &l2pFnEmpty},
        {"", "encode-for-uri",  1,  1, 0xFF, "!fn!encode-for-uri", false, &resultOne, &l2pFnEncodeForUri},
        {"", "ends-with",  2,  3, 0xFF, "!fn!ends-with", false, &resultOne, &l2pFnEndsWith},
        {"", "error",  0,  3, 0xFF, "!fn!error", false, &resultOne, &l2pFnError},
        {"", "escape-html-uri",  1,  1, 0xFF, "!fn!escape-html-uri", false, &resultOne, &l2pFnEscapeHtmlUri},
        {"", "exactly-one",  1,  1, 0xFF, "!fn!exactly-one", false, &resultOne, &l2pFnExactlyOne},
        {"", "exists",  1,  1, 0xFF, "!fn!exists", false, &resultOne, &l2pFnExists},
        {"", "false",  0,  0, 0xFF, "!fn!false", false, &resultOne, &l2pFnFalse},
        {"", "filter_entry_level",  1,  1, 0, "!fn!filter_entry_level", false, &firstArgResult, &l2pFnFilterEntryLevel},
        {"", "floor",  1,  1, 0xFF, "!fn!floor", false, &resultOne, &l2pFnFloor},
        {"", "fthighlight",  2,  3, 0xFF, "!fn!fthighlight", false, &atomicResultMany, &l2pFnFtHighlight},
        {"", "fthighlight-blocks",  2,  3, 0xFF, "!fn!fthighlight-blocks", false, &atomicResultMany, &l2pFnFtHighlightBlocks},
        {"", "ftindex-scan",  2,  3, 0xFF, "!fn!ftindex-scan", false, &resultUndefinedWoConst, &l2pFnFtIndexScan},
        {"", "ftindex-dict",  1,  2, 0xFF, "!fn!ftindex-dict", false, &atomicResultMany, &l2pFnFtIndexDict},
        {"", "ftscan",  3,  4, 0xFE, "!fn!ftscan", false, &firstArgResult, &l2pFnFtScan},
        {"", "ftwindex-scan",  2,  4, 0xFF, "!fn!windex-scan", false, &resultUndefinedWoConst, &l2pFnFtWIndexScan},
        {"", "hours-from-dateTime",  1,  1, 0xFF, "!fn!hours-from-dateTime", false, &resultOne, &l2pFnHoursFromDateTime},
        {"", "hours-from-duration",  1,  1, 0xFF, "!fn!hours-from-duration", false, &resultOne, &l2pFnHoursFromDuration},
        {"", "hours-from-time",  1,  1, 0xFF, "!fn!hours-from-time", false, &resultOne, &l2pFnHoursFromTime},
        {"", "id",  1,  2, 0xFF, "!fn!id", true, &resultUndefined, &l2pFnId},
        {"", "idref",  1,  2, 0xFF, "!fn!idref", true, &resultUndefined, &l2pFnIdref},
        {"", "implicit-timezone",  0,  0, 0xFF, "!fn!implicit-timezone", false, &resultOne, &l2pFnImplicitTimezone},
        {"", "index-of",  2,  3, 0xFE, "!fn!index-of", false, &atomicResultMany, &l2pFnIndexOf},
        {"", "index-scan",  3,  3, 0xFF, "!fn!index-scan", false, &resultUndefinedWoConst, &l2pFnIndexScan},
        {"", "index-keys",  1,  1, 0xFF, "!fn!index-keys", false, &atomicResultMany, &l2pFnIndexKeys},
        {"", "index-scan-between",  4,  4, 0xFF, "!fn!index-scan-between", false, &resultUndefinedWoConst, &l2pFnIndexScanBetween},
        {"", "in-scope-prefixes",  1,  1, 0xFF, "!fn!in-scope-prefixes", false, &atomicResultMany, &l2pFnInscopePrefixes},
        {"", "insert-before",  3,  3, 0xFA, "!fn!insert-before", false, &resultUndefined, &l2pFnInsertBefore},
        {"", "iri-to-uri",  1,  1, 0xFF, "!fn!iri-to-uri", false, &resultOne, &l2pFnIriToUri},
        {"", "is_ancestor",  2,  2, 0, "!fn!is_ancestor", false, &resultOne, &l2pFnIsAncestor},
        {"", "item-at",  2,  2, 0xFE, "!fn!item-at", false, &resultOne, &l2pFnItemAt},
        {"", "lang",  1,  2, 0xFF, "!fn!lang", true, &resultUndefined, &l2pFnLang},
        {"", "last",  0,  0, 0xFF, "!fn!last", false, &resultOne, &l2pFnLast},
        {"", "local-name",  0,  1, 0xFF, "!fn!local-name", false, &resultOne, &l2pFnLocalName},
        {"", "local-name-from-QName",  1,  1, 0xFF, "!fn!local-name-from-QName", false, &resultOne, &l2pFnLocalNameFromQName},
        {"", "lower-case",  1,  1, 0xFF, "!fn!lower-case", false, &resultOne, &l2pFnLowercase},
        {"", "matches",  2,  3, 0xFF, "!fn!matches", false, &resultOne, &l2pFnMatches},
        {"", "max",  1,  2, 0xFF, "!fn!max", false, &resultOne, &l2pFnMax},
        {"", "min",  1,  2, 0xFF, "!fn!min", false, &resultOne, &l2pFnMin},
        {"", "minutes-from-dateTime",  1,  1, 0xFF, "!fn!minutes-from-dateTime", false, &resultOne, &l2pFnMinutesFromDateTime},
        {"", "minutes-from-duration",  1,  1, 0xFF, "!fn!minutes-from-duration", false, &resultOne, &l2pFnMinutesFromDuration},
        {"", "minutes-from-time",  1,  1, 0xFF, "!fn!minutes-from-time", false, &resultOne, &l2pFnMinutesFromTime},
        {"", "month-from-date",  1,  1, 0xFF, "!fn!month-from-date", false, &resultOne, &l2pFnMonthFromDate},
        {"", "month-from-dateTime",  1,  1, 0xFF, "!fn!month-from-dateTime", false, &resultOne, &l2pFnMonthFromDateTime},
        {"", "months-from-duration",  1,  1, 0xFF, "!fn!months-from-duration", false, &resultOne, &l2pFnMonthsFromDuration},
        {"", "name",  0,  1, 0xFF, "!fn!name", false, &resultOne, &l2pFnName},
        {"", "namespace-uri",  0,  1, 0xFF, "!fn!namespace-uri", false, &resultOne, &l2pFnNamespaceUri},
        {"", "namespace-uri-for-prefix",  2,  2, 0xFF, "!fn!namespace-uri-for-prefix", false, &resultOne, &l2pFnNamespaceUriForPrefix},
        {"", "namespace-uri-from-QName",  1,  1, 0xFF, "!fn!namespace-uri-from-QName", false, &resultOne, &l2pFnNamespaceUriFromQName},
        {"", "nilled", 1, 1, 0xFF, "!fn!nilled", false, &resultOne, &l2pFnNilled},
        {"", "node-name", 1, 1, 0xFF, "!fn!node-name", false, &resultOne, &l2pFnNodeName},
        {"", "normalize-space",  0,  1, 0xFF, "!fn!normalize-space", false, &resultOne, &l2pFnNormalizeSpace},
        {"", "normalize-unicode",  1,  2, 0xFF, "!fn!normalize-unicode", false, &resultOne, &l2pFnNormalizeUnicode},
        {"", "not",  1,  1, 0xFF, "!fn!not", false, &resultOne, &l2pFnNot},
        {"", "number",  0,  1, 0xFF, "!fn!number", false, &resultOne, &l2pFnNumber},
        {"", "one-or-more",  1,  1, 0xFF, "!fn!one-or-more", false, &firstArgResult, &l2pFnNoneOrMore},
        {"", "position",  0,  0, 0xFF, "!fn!position", false, &resultOne, &l2pFnPosition},
        {"", "prefix-from-QName",  1,  1, 0xFF, "!fn!prefix-from-QName", false, &resultOne, &l2pFnPrefixFromQName},
        {"", "QName",  2,  2, 0xFF, "!fn!QName", false, &resultOne, &l2pFnQName},
        {"", "remove",  2,  2, 0xFE, "!fn!remove", false, &firstArgResult, &l2pFnRemove},
        {"", "replace",  3,  4, 0xFF, "!fn!replace", false, &resultOne, &l2pFnReplace},
        {"", "resolve-QName",  2,  2, 0xFF, "!fn!resolve-QName", false, &resultOne, &l2pFnResolveQName},
        {"", "resolve-uri",  1,  2, 0xFF, "!fn!resolve-uri", false, &resultOne, &l2pFnResolveUri},
        {"", "reverse",  1,  1, 0, "!fn!reverse", false, &firstArgResult, &l2pFnReverse},
        {"", "root",  0,  1, 0xFF, "!fn!root", false, &resultOne, &l2pFnRoot},
        {"", "round",  1,  1, 0xFF, "!fn!round", false, &resultOne, &l2pFnRound},
        {"", "round-half-to-even",  1,  2, 0xFF, "!fn!round-half-to-even", false, &resultOne, &l2pFnRoundHalfToEven},
        {"", "seconds-from-dateTime",  1,  1, 0xFF, "!fn!seconds-from-dateTime", false, &resultOne, &l2pFnSecondsFromDateTime},
        {"", "seconds-from-duration",  1,  1, 0xFF, "!fn!seconds-from-duration", false, &resultOne, &l2pFnSecondsFromDuration},
        {"", "seconds-from-time",  1,  1, 0xFF, "!fn!seconds-from-time", false, &resultOne, &l2pFnSecondsFromTime},
        {"", "starts-with",  2,  3, 0xFF, "!fn!starts-with", false, &resultOne, &l2pFnStartsWith},
        {"", "static-base-uri",  0,  0, 0xFF, "!fn!static-base-uri", false, &resultOne, &l2pFnStaticBaseUri},
        {"", "string",  0,  1, 0xFF, "!fn!string", false, &resultOne, &l2pFnString},
        {"", "string-join",  2,  2, 0xFF, "!fn!string-join", false, &resultOne, &l2pFnstringJoin},
        {"", "string-length",  0,  1, 0xFF, "!fn!string-length", false, &resultOne, &l2pFnStringLength},
        {"", "string-to-codepoints",  1,  1, 0xFF, "!fn!string-to-codepoints", false, &resultOne, &l2pFnStringToCodepoints},
        {"", "string-value",  1,  1, 0xFF, "!fn!string-value", false, &resultOne, &l2pFnStringValue},
        {"", "subsequence",  2,  3, 0xFE, "!fn!subsequence", false, &firstArgResult, &l2pFnSubsequence},
        {"", "substring",  2,  3, 0xFF, "!fn!substring", false, &resultOne, &l2pFnSubstring},
        {"", "substring-after",  2,  3, 0xFF, "!fn!substring-after", false, &resultOne, &l2pFnSubstringAfter},
        {"", "substring-before",  2,  3, 0xFF, "!fn!substring-before", false, &resultOne, &l2pFnSubstringBefore},
        {"", "sum",  1,  2, 0xFF, "!fn!sum", false, &resultOne, &l2pFnSum},
        {"", "test",  1,  1, 0xFF, "!fn!test", false, &resultOne, &l2pFnTest},
        {"", "timezone-from-date",  1,  1, 0xFF, "!fn!timezone-from-date", false, &resultOne, &l2pFnTimezoneFromDate},
        {"", "timezone-from-dateTime",  1,  1, 0xFF, "!fn!timezone-from-dateTime", false, &resultOne, &l2pFnTimezoneFromDateTime},
        {"", "timezone-from-time",  1,  1, 0xFF, "!fn!timezone-from-time", false, &resultOne, &l2pFnTimezoneFromTime},
        {"", "tokenize",  2,  3, 0xFF, "!fn!tokenize", false, &atomicResultMany, &l2pFnTokenize},
        {"", "trace",  2,  2, 0xFF, "!fn!trace", false, &firstArgResult, &l2pFnTrace},
        {"", "translate",  3,  3, 0xFF, "!fn!translate", false, &resultOne, &l2pFnTranslate},
        {"", "true",  0,  0, 0xFF, "!fn!true", false, &resultOne, &l2pFnTrue},
        {"", "typed-value",  1,  1, 0xFF, "!fn!typed-value", false, &resultOne, &l2pFnTypedValue},
        {"", "unordered",  1,  1, 0, "!fn!unordered", false, &firstArgResult, &l2pFnUnordered},
        {"", "upper-case",  1,  1, 0xFF, "!fn!upper-case", false, &resultOne, &l2pFnUppercase},
        {"", "year-from-date",  1,  1, 0xFF, "!fn!year-from-date", false, &resultOne, &l2pFnYearFromDate},
        {"", "year-from-dateTime",  1,  1, 0xFF, "!fn!year-from-dateTime", false, &resultOne, &l2pFnYearFromDateTime},
        {"", "years-from-duration",  1,  1, 0xFF, "!fn!years-from-duration", false, &resultOne, &l2pFnYearsFromDuration},
        {"", "zero-or-one",  1,  1, 0, "!fn!zero-or-one", false, &firstArgResult, &l2pFnZeroOrOne},
        {"", "", 0, 0, 0xFF, "", false, &resultOne} // dummy record to mark the end
    };

    XQFunction sqlFunctions[] =
    {
        {"", "close",  1,  1, 0xFF, "!fn!sql-close", false, &resultOne, &l2pFnSqlClose},
        {"", "commit",  1,  1, 0xFF, "!fn!sql-commit", false, &resultOne, &l2pFnSqlCommit},
        {"", "connect",  1,  4, 0xFF, "!fn!sql-connect", false, &resultOne, &l2pFnSqlConnect},
        {"", "exec-update",  2,  UINT32_MAX, 0xFF, "!fn!sql-exec-update", false, &resultOne, &l2pFnSqlExecUpdate},
        {"", "execute",  2,  UINT32_MAX, 0xFF, "!fn!sql-execute", false, &resultUndefinedWoConst, &l2pFnSqlExecute},
        {"", "prepare",  2,  3, 0xFF, "!fn!sql-prepare", false, &resultOne, &l2pFnSqlPrepare},
        {"", "rollback",  1,  1, 0xFF, "!fn!sql-rollback", false, &resultOne, &l2pFnSqlRollback},
        {"", "", 0, 0, 0xFF, "", false, &resultOne} // dummy record to mark the end
    };

    XQFunction seFunctions[] =
    {
        {"", "checkpoint",  0,  0, 0xFF, "!se!checkpoint", false, &resultOne, &l2pFnSeCheckpoint},
        {"", "get-property",  1,  1, 0xFF, "!se!get-property", false, &resultOne, &l2pFnSeGetProperty},
        {"", "", 0, 0, 0xFF, "", false, &resultOne} // dummy record to mark the end
    };

    XQFunction cryptoFunctions[] =
    {
        {"", "sha1",  1,  1, 0xFF, "!crypto!sha1", false, &resultOne, &l2pFnCryptoSha1},
        {"", "", 0, 0, 0xFF, "", false, &resultOne} // dummy record to mark the end
    };
}
