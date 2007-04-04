/* C++ code produced by gperf version 3.0.2 */
/* Command-line: gperf -Z PPNamesResolver -tpcCI -LC++ -s1 -m200 tok_pp.cpp.gp  */
/* Computed positions: -k'3,5,8,11,14,17,$' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif


#include "tok_pp.h"
struct PPName
{
	char * name;
	TOK_PP token;
};
#include <string.h>

#define TOTAL_KEYWORDS 229
#define MIN_WORD_LENGTH 4
#define MAX_WORD_LENGTH 28
#define MIN_HASH_VALUE 7
#define MAX_HASH_VALUE 356
/* maximum key range = 350, duplicates = 0 */

class PPNamesResolver
{
private:
  static inline unsigned int hash (const char *str, unsigned int len);
public:
  static const struct PPName *in_word_set (const char *str, unsigned int len);
};

inline unsigned int
PPNamesResolver::hash (register const char *str, register unsigned int len)
{
  static const unsigned short asso_values[] =
    {
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357,   0,
       10, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357,  17,  77,   5,  33,  58,
        0,  34, 151,  72,   5, 357, 147,  78,  86,  85,
       81,  95,  44,   6,  74, 121,  11,   2,   0, 127,
        0, 357, 357, 357, 357, 357, 357,  28, 213,  58,
       71,   0, 100,  15,  21,   1, 357,   4,  17,  95,
        2,   7,  58,   7,   1,   0,   0,  76,   4,  10,
       78,  55,   2, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357, 357, 357, 357, 357,
      357, 357, 357, 357, 357, 357
    };
  register int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[16]];
      /*FALLTHROUGH*/
      case 16:
      case 15:
      case 14:
        hval += asso_values[(unsigned char)str[13]];
      /*FALLTHROUGH*/
      case 13:
      case 12:
      case 11:
        hval += asso_values[(unsigned char)str[10]];
      /*FALLTHROUGH*/
      case 10:
      case 9:
      case 8:
        hval += asso_values[(unsigned char)str[7]];
      /*FALLTHROUGH*/
      case 7:
      case 6:
      case 5:
        hval += asso_values[(unsigned char)str[4]];
      /*FALLTHROUGH*/
      case 4:
      case 3:
        hval += asso_values[(unsigned char)str[2]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

const struct PPName *
PPNamesResolver::in_word_set (register const char *str, register unsigned int len)
{
  static const struct PPName wordlist[] =
    {
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"PPXptr",		 TOK_PPXPTR},
      {""},
      {"PPFnFalse",	 TOK_PPFNFALSE},
      {""},
      {"PPCast",	 TOK_PPCAST},
      {"PPSLet",		 TOK_PPSLET},
      {""},
      {"PPConst",	 TOK_PPCONST},
      {""},
      {"PPFnCount",	 TOK_PPFNCOUNT},
      {"PPFnFloor",	 TOK_PPFNFLOOR},
      {"PPFtScan",	 TOK_PPFTSCAN},
      {"PPFnContains",	 TOK_PPFNCONTAINS},
      {"PPStore",	 TOK_PPSTORE},
      {"PPFnCurrentDate",	 TOK_PPFNCURRENTDATE},
      {"PPCreateTrigger",	 TOK_PPCREATETRIGGER},
      {"PPFnSQLClose",	 TOK_PPFNSQLCLOSE},
      {"PPFnAbs",	 TOK_PPFNABS},
      {"PPSequence",	 TOK_PPSEQUENCE},
      {"PPFnCurrentDateTime",	 TOK_PPFNCURRENTDATETIME},
      {"PPFnSQLConnect",	 TOK_PPFNSQLCONNECT},
      {"PPADFilter",	 TOK_PPADFILTER},
      {""},
      {"PPFnStringJoin",	 TOK_PPFNSTRINGJOIN},
      {"PPSelect",	 TOK_PPSELECT},
      {"PPFnString",	 TOK_PPFNSTRING},
      {"PPAJoin",	 TOK_PPAJOIN},
      {"PPAxisAttribute",	 TOK_PPAXISATTRIBUTE},
      {"PPFnSubstring",	 TOK_PPFNSUBSTRING},
      {"PPAxisAncestor",	 TOK_PPAXISANCESTOR},
      {"PPASemiJoin",	 TOK_PPASEMIJOIN},
      {""},
      {"PPFnAvg",	 TOK_PPFNAVG},
      {"PPFnSecondsFromTime",	 TOK_PPFNSECONDSFROMTIME},
      {""},
      {"PPScan",		 TOK_PPSCAN},
      {"PPFnSubstringAfter",	 TOK_PPFNSUBSTRINGAFTER},
      {"PPDAFilter",	 TOK_PPDAFILTER},
      {"PPFunCall",	 TOK_PPFUNCALL},
      {""},
      {"PPAbsPath",	 TOK_PPABSPATH},
      {""},
      {"PPFnCodepointsToString",	 TOK_PPFNCODEPOINTSTOSTRING},
      {"PPCalculate",	 TOK_PPCALCULATE},
      {"PPAxisPreceding",	 TOK_PPAXISPRECEDING},
      {"PPFnRoot",	 TOK_PPFNROOT},
      {"PPRange",	 TOK_PPRANGE},
      {"PPRename",	 TOK_PPRENAME},
      {"PPFnReverse",	 TOK_PPFNREVERSE},
      {"PPReturn",	 TOK_PPRETURN},
      {""}, {""},
      {"PPAxisPrecedingSibling",	 TOK_PPAXISPRECEDINGSIBLING},
      {"PPAxisParent",	 TOK_PPAXISPARENT},
      {"PPFnRemove",	 TOK_PPFNREMOVE},
      {"PPAxisDescendant",	 TOK_PPAXISDESCENDANT},
      {"PPFnCeiling",	 TOK_PPFNCEILING},
      {"PPAxisFollowing",	 TOK_PPAXISFOLLOWING},
      {"PPFnStartsWith",	 TOK_PPFNSTARTSWITH},
      {"PPDropCollection",	 TOK_PPDROPCOLLECTION},
      {"PPFnResolveUri",	 TOK_PPFNRESOLVEURI},
      {"PPFnExists",	 TOK_PPFNEXISTS},
      {"PPElement",	 TOK_PPELEMENT},
      {"PPDropTrigger",	 TOK_PPDROPTRIGGER},
      {"PPFnSecondsFromDateTime",	 TOK_PPFNSECONDSFROMDATETIME},
      {"PPAxisFollowingSibling",	 TOK_PPAXISFOLLOWINGSIBLING},
      {"PPFnConcat",	 TOK_PPFNCONCAT},
      {"PPFnCompare",	 TOK_PPFNCOMPARE},
      {"PPFnError",	 TOK_PPFNERROR},
      {"PPDropModule",	 TOK_PPDROPMODULE},
      {"PPCheckpoint",	 TOK_PPCHECKPOINT},
      {""},
      {"PPCreateDocument",	 TOK_PPCREATEDOCUMENT},
      {"PPTest",		 TOK_PPTEST},
      {"PPTreat",	 TOK_PPTREAT},
      {"PPFnTrue",	 TOK_PPFNTRUE},
      {"PPIntersect",	 TOK_PPINTERSECT},
      {"PPAxisDescendantAttr",	 TOK_PPAXISDESCENDANTATTR},
      {""},
      {"PPFunDecl",	 TOK_PPFUNDECL},
      {"PPFnMin",	 TOK_PPFNMIN},
      {"PPPred1",	 TOK_PPPRED1},
      {"PPInsertTo",	 TOK_PPINSERTTO},
      {"PPSTuple",	 TOK_PPSTUPLE},
      {"PPFnEndsWith",	 TOK_PPFNENDSWITH},
      {""},
      {"PPFnNot",	 TOK_PPFNNOT},
      {"PPFnName",	 TOK_PPFNNAME},
      {"PPFnResolveQName",	 TOK_PPFNRESOLVEQNAME},
      {"PPVarDecl",	 TOK_PPVARDECL},
      {"PPFnData",	 TOK_PPFNDATA},
      {"PPPred2",	 TOK_PPPRED2},
      {""},
      {"PPFnDefaultCollation",	 TOK_PPFNDEFAULTCOLLATION},
      {"PPDocument",	 TOK_PPDOCUMENT},
      {"PPFnStaticBaseUri",	 TOK_PPFNSTATICBASEURI},
      {"PPConstructionDecl",	 TOK_PPCONSTRUCTIONDECL},
      {""},
      {"PPFnZeroOrOne",	 TOK_PPFNZEROORONE},
      {"PPExtFunCall",	 TOK_PPEXTFUNCALL},
      {"PPCreateCollection",	 TOK_PPCREATECOLLECTION},
      {"PPFnSum",	 TOK_PPFNSUM},
      {"PPFnBoolean",	 TOK_PPFNBOOLEAN},
      {"PPFnSubstringBefore",	 TOK_PPFNSUBSTRINGBEFORE},
      {"PPComment",	 TOK_PPCOMMENT},
      {"PPPatMatch",	 TOK_PPPATMATCH},
      {"PPFnImplicitTimezone",	 TOK_PPFNIMPLICITTIMEZONE},
      {"PPRetrieveDSForDocument",	 TOK_PPRETRIEVEDSFORDOCUMENT},
      {""},
      {"PPFnCurrentTime",	 TOK_PPFNCURRENTTIME},
      {"PPFnTranslate",	 TOK_PPFNTRANSLATE},
      {"PPRetrieveDSForCollection",	 TOK_PPRETRIEVEDSFORCOLLECTION},
      {"PPFnSQLCommit",	 TOK_PPFNSQLCOMMIT},
      {"PPDeleteDeep",	 TOK_PPDELETEDEEP},
      {"PPFnSecondsFromDuration",	 TOK_PPFNSECONDSFROMDURATION},
      {"PPFnEmpty",	 TOK_PPFNEMPTY},
      {"PPDocInCol",	 TOK_PPDOCINCOL},
      {"PPExcept",	 TOK_PPEXCEPT},
      {"PPNil",		 TOK_PPNIL},
      {"PPFnRound",	 TOK_PPFNROUND},
      {"PPFnCodepointEqual",	 TOK_PPFNCODEPOINTEQUAL},
      {"PPAxisSelf",	 TOK_PPAXISSELF},
      {"PPFnMonthFromDate",	 TOK_PPFNMONTHFROMDATE},
      {"PPFnTimezoneFromDate",	 TOK_PPFNTIMEZONEFROMDATE},
      {"PPUnion",	 TOK_PPUNION},
      {""},
      {"PPFnMonthFromDateTime",	 TOK_PPFNMONTHFROMDATETIME},
      {"PPFnTimezoneFromDateTime",	 TOK_PPFNTIMEZONEFROMDATETIME},
      {""},
      {"PPFnSQLExecute",	 TOK_PPFNSQLEXECUTE},
      {"PPFnDistinctValues",	 TOK_PPFNDISTINCTVALUES},
      {"PPFnDaysFromDuration",	 TOK_PPFNDAYSFROMDURATION},
      {"PPTuple",	 TOK_PPTUPLE},
      {"PPFnDateTime",	 TOK_PPFNDATETIME},
      {"PPFnTrace",	 TOK_PPFNTRACE},
      {"PPDmStringValue",	 TOK_PPDMSTRINGVALUE},
      {"PPFnDayFromDate",	 TOK_PPFNDAYFROMDATE},
      {"PPFnSQLRollback",	 TOK_PPFNSQLROLLBACK},
      {"PPFtIndexScan",	 TOK_PPFTINDEXSCAN},
      {"PPInsertFollowing",	 TOK_PPINSERTFOLLOWING},
      {"PPAxisAncestorOrSelf",	 TOK_PPAXISANCESTORORSELF},
      {"PPFnDayFromDateTime",	 TOK_PPFNDAYFROMDATETIME},
      {"PPDmTypedValue",	 TOK_PPDMTYPEDVALUE},
      {"PPQueryRoot",	 TOK_PPQUERYROOT},
      {"PPBaseURIDecl",	 TOK_PPBASEURIDECL},
      {"PPLet",		 TOK_PPLET},
      {""}, {""},
      {"PPFnSubsequence",	 TOK_PPFNSUBSEQUENCE},
      {"PPDropDocument",	 TOK_PPDROPDOCUMENT},
      {"PPPI",		 TOK_PPPI},
      {"PPText",		 TOK_PPTEXT},
      {"PPFnSQLPrepare",	 TOK_PPFNSQLPREPARE},
      {"PPFnIriToUri",	 TOK_PPFNIRITOURI},
      {"PPNSDecl",	 TOK_PPNSDECL},
      {"PPFnUpperCase",	 TOK_PPFNUPPERCASE},
      {"PPFnMax",	 TOK_PPFNMAX},
      {"PPIndexScan",	 TOK_PPINDEXSCAN},
      {"PPCreateDocumentInCollection",	 TOK_PPCREATEDOCUMENTINCOLLECTION},
      {""},
      {"PPCreateIndex",	 TOK_PPCREATEINDEX},
      {"PPFnStringToCodepoints",	 TOK_PPFNSTRINGTOCODEPOINTS},
      {"PPReplace",	 TOK_PPREPLACE},
      {"PPCreateFtIndex",	 TOK_PPCREATEFTINDEX},
      {"PPFnTimezoneFromTime",	 TOK_PPFNTIMEZONEFROMTIME},
      {"PPFnInsertBefore",	 TOK_PPFNINSERTBEFORE},
      {"PPFnRoundHalfToEven",	 TOK_PPFNROUNDHALFTOEVEN},
      {"PPOptionDecl",	 TOK_PPOPTIONDECL},
      {"PPDropDocumentInCollection",	 TOK_PPDROPDOCUMENTINCOLLECTION},
      {"PPIf",		 TOK_PPIF},
      {"PPFnItemAt",	 TOK_PPFNITEMAT},
      {"PPGeneralCompGE",	 TOK_PPGENERALCOMPGE},
      {"PPFnYearFromDate",	 TOK_PPFNYEARFROMDATE},
      {""},
      {"PPFnMinutesFromTime",	 TOK_PPFNMINUTESFROMTIME},
      {"PPBulkLoad",	 TOK_PPBULKLOAD},
      {"PPUp",		 TOK_PPUP},
      {"PPFnNilled",	 TOK_PPFNNILLED},
      {"PPFnExactlyOne",	 TOK_PPFNEXACTLYONE},
      {"PPInsertBefore",	 TOK_PPINSERTBEFORE},
      {""},
      {"PPFnLowerCase",	 TOK_PPFNLOWERCASE},
      {"PPSpaceSequence",	 TOK_PPSPACESEQUENCE},
      {"PPFnOneOrMore",	 TOK_PPFNONEORMORE},
      {""},
      {"PPAxisChild",	 TOK_PPAXISCHILD},
      {"PPFnNodeName",	 TOK_PPFNNODENAME},
      {"PPGeneralCompGT",	 TOK_PPGENERALCOMPGT},
      {""},
      {"PPRetrieveMetadata",	 TOK_PPRETRIEVEMETADATA},
      {"PPFnDeepEqual",	 TOK_PPFNDEEPEQUAL},
      {"PPFnMonthsFromDuration",	 TOK_PPFNMONTHSFROMDURATION},
      {"PPFnQName",	 TOK_PPFNQNAME},
      {"PPFtHighlight",	 TOK_PPFTHIGHLIGHT},
      {"PPDefNSDeclFun",	 TOK_PPDEFNSDECLFUN},
      {"PPDropFtIndex",	 TOK_PPDROPFTINDEX},
      {"PPFnEncodeForUri",	 TOK_PPFNENCODEFORURI},
      {"PPDmNodeKind",	 TOK_PPDMNODEKIND},
      {""},
      {"PPFnStringLength",	 TOK_PPFNSTRINGLENGTH},
      {"PPFnHoursFromDateTime",	 TOK_PPFNHOURSFROMDATETIME},
      {"PPDDO",	 TOK_PPDDO},
      {"PPDropIndex",	 TOK_PPDROPINDEX},
      {"PPFnSQLExecUpdate",	 TOK_PPFNSQLEXECUPDATE},
      {"PPLoadModule",	 TOK_PPLOADMODULE},
      {"PPFnMinutesFromDateTime",	 TOK_PPFNMINUTESFROMDATETIME},
      {"PPFnYearsFromDuration",	 TOK_PPFNYEARSFROMDURATION},
      {""}, {""},
      {"PPFnLocalName",	 TOK_PPFNLOCALNAME},
      {""}, {""},
      {"PPCopyNamespacesDecl",	 TOK_PPCOPYNAMESPACESDECL},
      {""},
      {"PPFtHighlight2",	 TOK_PPFTHIGHLIGHT2},
      {""}, {""},
      {"PPFnAdjustDateToTimezone",	 TOK_PPFNADJUSTDATETOTIMEZONE},
      {""},
      {"PPANNodeComparison",	 TOK_PPANNODECOMPARISON},
      {"PPFnInScopePrefixes",	 TOK_PPFNINSCOPEPREFIXES},
      {"PPCastable",	 TOK_PPCASTABLE},
      {""},
      {"PPGeneralCompNE",	 TOK_PPGENERALCOMPNE},
      {"PPDefaultCollationDecl",	 TOK_PPDEFAULTCOLLATIONDECL},
      {"PPFnBaseURI",	 TOK_PPFNBASEURI},
      {"PPTypeswitch",	 TOK_PPTYPESWITCH},
      {"PPEmptyOrderDecl",	 TOK_PPEMPTYORDERDECL},
      {"PPVariable",	 TOK_PPVARIABLE},
      {""},
      {"PPFnHoursFromDuration",	 TOK_PPFNHOURSFROMDURATION},
      {""},
      {"PPGeneralCompEQ",	 TOK_PPGENERALCOMPEQ},
      {""},
      {"PPAttribute",	 TOK_PPATTRIBUTE},
      {"PPFnDocumentURI",	 TOK_PPFNDOCUMENTURI},
      {"PPGTNodeComparison",	 TOK_PPGTNODECOMPARISON},
      {"PPFnHoursFromTime",	 TOK_PPFNHOURSFROMTIME},
      {""}, {""}, {""}, {""},
      {"PPFnAdjustDateTimeToTimezone",	 TOK_PPFNADJUSTDATETIMETOTIMEZONE},
      {"PPNamespace",	 TOK_PPNAMESPACE},
      {"PPDeleteUndeep",	 TOK_PPDELETEUNDEEP},
      {"PPFnNamespaceUri",	 TOK_PPFNNAMESPACEURI},
      {"PPAxisDescendantOrSelf",	 TOK_PPAXISDESCENDANTORSELF},
      {""}, {""}, {""},
      {"PPFnYearFromDateTime",	 TOK_PPFNYEARFROMDATETIME},
      {""}, {""},
      {"PPFnNamespaceUriFromQName",	 TOK_PPFNNAMESPACEURIFROMQNAME},
      {""},
      {"PPFnMinutesFromDuration",	 TOK_PPFNMINUTESFROMDURATION},
      {""},
      {"PPFnNormalizeSpace",	 TOK_PPFNNORMALIZESPACE},
      {"PPFnAdjustTimeToTimezone",	 TOK_PPFNADJUSTTIMETOTIMEZONE},
      {""},
      {"PPEQNodeComparison",	 TOK_PPEQNODECOMPARISON},
      {"PPDebug",	 TOK_PPDEBUG},
      {""}, {""},
      {"PPInstanceOf",	 TOK_PPINSTANCEOF},
      {""},
      {"PPFnEscapeHtmlUri",	 TOK_PPFNESCAPEHTMLURI},
      {""}, {""}, {""},
      {"PPxsQName",	 TOK_PPXSQNAME},
      {""}, {""},
      {"PPFnDocAvailable",	 TOK_PPFNDOCAVAILABLE},
      {"PPBoundarySpaceDecl",	 TOK_PPBOUNDARYSPACEDECL},
      {""},
      {"PPFnIndexOf",	 TOK_PPFNINDEXOF},
      {""}, {""}, {""}, {""},
      {"PPGlobalVariable",	 TOK_PPGLOBALVARIABLE},
      {""}, {""},
      {"PPGeneralCompLE",	 TOK_PPGENERALCOMPLE},
      {""},
      {"PPDefNSDeclElem",	 TOK_PPDEFNSDECLELEM},
      {""}, {""}, {""},
      {"PPOrderBy",	 TOK_PPORDERBY},
      {""},
      {"PPFEL",	 TOK_PPFEL},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"PPGeneralCompLT",	 TOK_PPGENERALCOMPLT},
      {""}, {""},
      {"PPFnNumber",	 TOK_PPFNNUMBER},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"PPFnLocalNameFromQName",	 TOK_PPFNLOCALNAMEFROMQNAME},
      {""}, {""},
      {"PPFnPrefixFromQName",	 TOK_PPFNPREFIXFROMQNAME},
      {""}, {""}, {""}, {""},
      {"PPOrderingModeDecl",	 TOK_PPORDERINGMODEDECL},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"PPFnNamespaceUriForPrefix",	 TOK_PPFNNAMESPACEURIFORPREFIX},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"PPLTNodeComparison",	 TOK_PPLTNODECOMPARISON}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strncmp (str + 1, s + 1, len - 1) && s[len] == '\0')
            return &wordlist[key];
        }
    }
  return 0;
}


TOK_PP ResolvePPName(const char * str, unsigned int len)
{
	const PPName * p;
	p=PPNamesResolver::in_word_set((char*)str,len);
	return p?TOK_PPUNKNOWN:p->token;
}

