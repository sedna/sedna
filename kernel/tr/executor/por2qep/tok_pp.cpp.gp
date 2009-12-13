%{
#include "tok_pp.h"
%}

struct PPName
{
	char * name;
	TOK_PP token;
};

%%
PPADFilter,	 TOK_PPADFILTER
PPAJoin,	 TOK_PPAJOIN
PPANNodeComparison,	 TOK_PPANNODECOMPARISON
PPASemiJoin,	 TOK_PPASEMIJOIN
PPAbsPath,	 TOK_PPABSPATH
PPAttribute,	 TOK_PPATTRIBUTE
PPAxisAncestor,	 TOK_PPAXISANCESTOR
PPAxisAncestorOrSelf,	 TOK_PPAXISANCESTORORSELF
PPAxisAttribute,	 TOK_PPAXISATTRIBUTE
PPAxisChild,	 TOK_PPAXISCHILD
PPAxisDescendant,	 TOK_PPAXISDESCENDANT
PPAxisDescendantAttr,	 TOK_PPAXISDESCENDANTATTR
PPAxisDescendantOrSelf,	 TOK_PPAXISDESCENDANTORSELF
PPAxisFollowing,	 TOK_PPAXISFOLLOWING
PPAxisFollowingSibling,	 TOK_PPAXISFOLLOWINGSIBLING
PPAxisParent,	 TOK_PPAXISPARENT
PPAxisPreceding,	 TOK_PPAXISPRECEDING
PPAxisPrecedingSibling,	 TOK_PPAXISPRECEDINGSIBLING
PPAxisSelf,	 TOK_PPAXISSELF
PPBaseURIDecl,	 TOK_PPBASEURIDECL
PPBoundarySpaceDecl,	 TOK_PPBOUNDARYSPACEDECL
PPBulkLoad,	 TOK_PPBULKLOAD
PPCalculate,	 TOK_PPCALCULATE
PPCast,	 TOK_PPCAST
PPCastable,	 TOK_PPCASTABLE
PPCheckpoint,	 TOK_PPCHECKPOINT
PPComment,	 TOK_PPCOMMENT
PPConst,	 TOK_PPCONST
PPConstructionDecl,	 TOK_PPCONSTRUCTIONDECL
PPCopyNamespacesDecl,	 TOK_PPCOPYNAMESPACESDECL
PPCreateCollection,	 TOK_PPCREATECOLLECTION
PPCreateDocument,	 TOK_PPCREATEDOCUMENT
PPCreateDocumentInCollection,	 TOK_PPCREATEDOCUMENTINCOLLECTION
PPCreateFtIndex,	 TOK_PPCREATEFTINDEX
PPCreateIndex,	 TOK_PPCREATEINDEX
PPCreateTrigger,	 TOK_PPCREATETRIGGER
PPDAFilter,	 TOK_PPDAFILTER
PPDDO,	 TOK_PPDDO
PPDebug,	 TOK_PPDEBUG
PPDefNSDeclElem,	 TOK_PPDEFNSDECLELEM
PPDefNSDeclFun,	 TOK_PPDEFNSDECLFUN
PPDefaultCollationDecl,	 TOK_PPDEFAULTCOLLATIONDECL
PPDeleteDeep,	 TOK_PPDELETEDEEP
PPDeleteUndeep,	 TOK_PPDELETEUNDEEP
PPDmNodeKind,	 TOK_PPDMNODEKIND
PPDmStringValue,	 TOK_PPDMSTRINGVALUE
PPDmTypedValue,	 TOK_PPDMTYPEDVALUE
PPDocInCol,	 TOK_PPDOCINCOL
PPDocument,	 TOK_PPDOCUMENT
PPDropCollection,	 TOK_PPDROPCOLLECTION
PPDropDocument,	 TOK_PPDROPDOCUMENT
PPDropDocumentInCollection,	 TOK_PPDROPDOCUMENTINCOLLECTION
PPDropFtIndex,	 TOK_PPDROPFTINDEX
PPDropIndex,	 TOK_PPDROPINDEX
PPDropModule,	 TOK_PPDROPMODULE
PPDropTrigger,	 TOK_PPDROPTRIGGER
PPEQNodeComparison,	 TOK_PPEQNODECOMPARISON
PPElement,	 TOK_PPELEMENT
PPEmptyOrderDecl,	 TOK_PPEMPTYORDERDECL
PPExcept,	 TOK_PPEXCEPT
PPExtFunCall,	 TOK_PPEXTFUNCALL
PPFEL,	 TOK_PPFEL
PPFnAbs,	 TOK_PPFNABS
PPFnAdjustDateTimeToTimezone,	 TOK_PPFNADJUSTDATETIMETOTIMEZONE
PPFnAdjustDateToTimezone,	 TOK_PPFNADJUSTDATETOTIMEZONE
PPFnAdjustTimeToTimezone,	 TOK_PPFNADJUSTTIMETOTIMEZONE
PPFnAvg,	 TOK_PPFNAVG
PPFnBaseURI,	 TOK_PPFNBASEURI
PPFnBoolean,	 TOK_PPFNBOOLEAN
PPFnCeiling,	 TOK_PPFNCEILING
PPFnCodepointEqual,	 TOK_PPFNCODEPOINTEQUAL
PPFnCodepointsToString,	 TOK_PPFNCODEPOINTSTOSTRING
PPFnCompare,	 TOK_PPFNCOMPARE
PPFnConcat,	 TOK_PPFNCONCAT
PPFnContains,	 TOK_PPFNCONTAINS
PPFnCount,	 TOK_PPFNCOUNT
PPFnCurrentDate,	 TOK_PPFNCURRENTDATE
PPFnCurrentDateTime,	 TOK_PPFNCURRENTDATETIME
PPFnCurrentTime,	 TOK_PPFNCURRENTTIME
PPFnData,	 TOK_PPFNDATA
PPFnDateTime,	 TOK_PPFNDATETIME
PPFnDayFromDate,	 TOK_PPFNDAYFROMDATE
PPFnDayFromDateTime,	 TOK_PPFNDAYFROMDATETIME
PPFnDaysFromDuration,	 TOK_PPFNDAYSFROMDURATION
PPFnDeepEqual,	 TOK_PPFNDEEPEQUAL
PPFnDefaultCollation,	 TOK_PPFNDEFAULTCOLLATION
PPFnDistinctValues,	 TOK_PPFNDISTINCTVALUES
PPFnDocAvailable,	 TOK_PPFNDOCAVAILABLE
PPFnDocumentURI,	 TOK_PPFNDOCUMENTURI
PPFnEmpty,	 TOK_PPFNEMPTY
PPFnEncodeForUri,	 TOK_PPFNENCODEFORURI
PPFnEndsWith,	 TOK_PPFNENDSWITH
PPFnError,	 TOK_PPFNERROR
PPFnEscapeHtmlUri,	 TOK_PPFNESCAPEHTMLURI
PPFnExactlyOne,	 TOK_PPFNEXACTLYONE
PPFnExists,	 TOK_PPFNEXISTS
PPFnFalse,	 TOK_PPFNFALSE
PPFnFloor,	 TOK_PPFNFLOOR
PPFnHoursFromDateTime,	 TOK_PPFNHOURSFROMDATETIME
PPFnHoursFromDuration,	 TOK_PPFNHOURSFROMDURATION
PPFnHoursFromTime,	 TOK_PPFNHOURSFROMTIME
PPFnImplicitTimezone,	 TOK_PPFNIMPLICITTIMEZONE
PPFnInScopePrefixes,	 TOK_PPFNINSCOPEPREFIXES
PPFnIndexOf,	 TOK_PPFNINDEXOF
PPFnInsertBefore,	 TOK_PPFNINSERTBEFORE
PPFnIriToUri,	 TOK_PPFNIRITOURI
PPFnItemAt,	 TOK_PPFNITEMAT
PPFnLocalName,	 TOK_PPFNLOCALNAME
PPFnLocalNameFromQName,	 TOK_PPFNLOCALNAMEFROMQNAME
PPFnLowerCase,	 TOK_PPFNLOWERCASE
PPFnMax,	 TOK_PPFNMAX
PPFnMin,	 TOK_PPFNMIN
PPFnMinutesFromDateTime,	 TOK_PPFNMINUTESFROMDATETIME
PPFnMinutesFromDuration,	 TOK_PPFNMINUTESFROMDURATION
PPFnMinutesFromTime,	 TOK_PPFNMINUTESFROMTIME
PPFnMonthFromDate,	 TOK_PPFNMONTHFROMDATE
PPFnMonthFromDateTime,	 TOK_PPFNMONTHFROMDATETIME
PPFnMonthsFromDuration,	 TOK_PPFNMONTHSFROMDURATION
PPFnName,	 TOK_PPFNNAME
PPFnNamespaceUri,	 TOK_PPFNNAMESPACEURI
PPFnNamespaceUriForPrefix,	 TOK_PPFNNAMESPACEURIFORPREFIX
PPFnNamespaceUriFromQName,	 TOK_PPFNNAMESPACEURIFROMQNAME
PPFnNilled,	 TOK_PPFNNILLED
PPFnNodeName,	 TOK_PPFNNODENAME
PPFnNormalizeSpace,	 TOK_PPFNNORMALIZESPACE
PPFnNot,	 TOK_PPFNNOT
PPFnNumber,	 TOK_PPFNNUMBER
PPFnOneOrMore,	 TOK_PPFNONEORMORE
PPFnPrefixFromQName,	 TOK_PPFNPREFIXFROMQNAME
PPFnQName,	 TOK_PPFNQNAME
PPFnRemove,	 TOK_PPFNREMOVE
PPFnResolveQName,	 TOK_PPFNRESOLVEQNAME
PPFnResolveUri,	 TOK_PPFNRESOLVEURI
PPFnReverse,	 TOK_PPFNREVERSE
PPFnRoot,	 TOK_PPFNROOT
PPFnRound,	 TOK_PPFNROUND
PPFnRoundHalfToEven,	 TOK_PPFNROUNDHALFTOEVEN
PPFnSQLClose,	 TOK_PPFNSQLCLOSE
PPFnSQLCommit,	 TOK_PPFNSQLCOMMIT
PPFnSQLConnect,	 TOK_PPFNSQLCONNECT
PPFnSQLExecUpdate,	 TOK_PPFNSQLEXECUPDATE
PPFnSQLExecute,	 TOK_PPFNSQLEXECUTE
PPFnSQLPrepare,	 TOK_PPFNSQLPREPARE
PPFnSQLRollback,	 TOK_PPFNSQLROLLBACK
PPFnSecondsFromDateTime,	 TOK_PPFNSECONDSFROMDATETIME
PPFnSecondsFromDuration,	 TOK_PPFNSECONDSFROMDURATION
PPFnSecondsFromTime,	 TOK_PPFNSECONDSFROMTIME
PPFnStartsWith,	 TOK_PPFNSTARTSWITH
PPFnStaticBaseUri,	 TOK_PPFNSTATICBASEURI
PPFnString,	 TOK_PPFNSTRING
PPFnStringJoin,	 TOK_PPFNSTRINGJOIN
PPFnStringLength,	 TOK_PPFNSTRINGLENGTH
PPFnStringToCodepoints,	 TOK_PPFNSTRINGTOCODEPOINTS
PPFnSubsequence,	 TOK_PPFNSUBSEQUENCE
PPFnSubstring,	 TOK_PPFNSUBSTRING
PPFnSubstringAfter,	 TOK_PPFNSUBSTRINGAFTER
PPFnSubstringBefore,	 TOK_PPFNSUBSTRINGBEFORE
PPFnSum,	 TOK_PPFNSUM
PPFnTimezoneFromDate,	 TOK_PPFNTIMEZONEFROMDATE
PPFnTimezoneFromDateTime,	 TOK_PPFNTIMEZONEFROMDATETIME
PPFnTimezoneFromTime,	 TOK_PPFNTIMEZONEFROMTIME
PPFnTrace,	 TOK_PPFNTRACE
PPFnTranslate,	 TOK_PPFNTRANSLATE
PPFnTrue,	 TOK_PPFNTRUE
PPFnUpperCase,	 TOK_PPFNUPPERCASE
PPFnYearFromDate,	 TOK_PPFNYEARFROMDATE
PPFnYearFromDateTime,	 TOK_PPFNYEARFROMDATETIME
PPFnYearsFromDuration,	 TOK_PPFNYEARSFROMDURATION
PPFnZeroOrOne,	 TOK_PPFNZEROORONE
PPFtHighlight,	 TOK_PPFTHIGHLIGHT
PPFtHighlight2,	 TOK_PPFTHIGHLIGHT2
PPFtIndexScan,	 TOK_PPFTINDEXSCAN
PPFtScan,	 TOK_PPFTSCAN
PPFunCall,	 TOK_PPFUNCALL
PPFunDecl,	 TOK_PPFUNDECL
PPGTNodeComparison,	 TOK_PPGTNODECOMPARISON
PPGeneralCompEQ,	 TOK_PPGENERALCOMPEQ
PPGeneralCompGE,	 TOK_PPGENERALCOMPGE
PPGeneralCompGT,	 TOK_PPGENERALCOMPGT
PPGeneralCompLE,	 TOK_PPGENERALCOMPLE
PPGeneralCompLT,	 TOK_PPGENERALCOMPLT
PPGeneralCompNE,	 TOK_PPGENERALCOMPNE
PPGlobalVariable,	 TOK_PPGLOBALVARIABLE
PPIf,		 TOK_PPIF
PPIndexScan,	 TOK_PPINDEXSCAN
PPInsertBefore,	 TOK_PPINSERTBEFORE
PPInsertFollowing,	 TOK_PPINSERTFOLLOWING
PPInsertTo,	 TOK_PPINSERTTO
PPInstanceOf,	 TOK_PPINSTANCEOF
PPIntersect,	 TOK_PPINTERSECT
PPLTNodeComparison,	 TOK_PPLTNODECOMPARISON
PPLet,		 TOK_PPLET
PPLoadModule,	 TOK_PPLOADMODULE
PPNSDecl,	 TOK_PPNSDECL
PPNamespace,	 TOK_PPNAMESPACE
PPNil,		 TOK_PPNIL
PPOptionDecl,	 TOK_PPOPTIONDECL
PPOrderBy,	 TOK_PPORDERBY
PPOrderingModeDecl,	 TOK_PPORDERINGMODEDECL
PPPI,		 TOK_PPPI
PPPatMatch,	 TOK_PPPATMATCH
PPPred1,	 TOK_PPPRED1
PPPred2,	 TOK_PPPRED2
PPQueryRoot,	 TOK_PPQUERYROOT
PPRange,	 TOK_PPRANGE
PPRename,	 TOK_PPRENAME
PPReplace,	 TOK_PPREPLACE
PPRetrieveDSForCollection,	 TOK_PPRETRIEVEDSFORCOLLECTION
PPRetrieveDSForDocument,	 TOK_PPRETRIEVEDSFORDOCUMENT
PPRetrieveMetadata,	 TOK_PPRETRIEVEMETADATA
PPReturn,	 TOK_PPRETURN
PPSLet,		 TOK_PPSLET
PPSTuple,	 TOK_PPSTUPLE
PPScan,		 TOK_PPSCAN
PPSelect,	 TOK_PPSELECT
PPSequence,	 TOK_PPSEQUENCE
PPSpaceSequence,	 TOK_PPSPACESEQUENCE
PPStore,	 TOK_PPSTORE
PPTest,		 TOK_PPTEST
PPText,		 TOK_PPTEXT
PPTreat,	 TOK_PPTREAT
PPTuple,	 TOK_PPTUPLE
PPTypeswitch,	 TOK_PPTYPESWITCH
PPUnion,	 TOK_PPUNION
PPUp,		 TOK_PPUP
PPVarDecl,	 TOK_PPVARDECL
PPVariable,	 TOK_PPVARIABLE
PPXptr,		 TOK_PPXPTR
PPxsQName,	 TOK_PPXSQNAME
%%

TOK_PP ResolvePPName(const char * str, unsigned int len)
{
	const PPName * p;
	p=PPNamesResolver::in_word_set((char*)str,len);
	return p?TOK_PPUNKNOWN:p->token;
}
