/*
 * File:  PPOperations.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPOPERATIONS_H
#define _PPOPERATIONS_H

class PPBulkLoad;
class PPCreateIndex;
class PPCreateDocument;
class PPCreateCollection;
class PPCreateDocumentInCollection;
class PPDeleteDeep;
class PPDeleteUndeep;
class PPDropIndex;
class PPDropDocument;
class PPDropCollection;
class PPDropDocumentInCollection;
class PPDropModule;
class PPInsertBefore;
class PPInsertFollowing;
class PPInsertTo;
class PPLoadModule;
class PPQueryRoot;
class PPSubQuery;
class PPRename;
class PPReplace;
class PPRetrieveDS;
class PPRetrieveMetadata;
class PPCreateUser;
class PPDropUser;
class PPAlterUser;
class PPCreateRole;
class PPDropRole;
class PPGrantRole;
class PPGrantRevokePriv;
class PPRevokeRole;

#ifdef SE_ENABLE_DTSEARCH
class PPFtScan;
#endif
#ifdef SE_ENABLE_FTSEARCH
class PPCreateFtIndex;
class PPDropFtIndex;

class PPFtHighlight;
class PPFtIndexDict;
class PPFtIndexScan;
class PPFtIndexScan2;
#endif

#ifdef SE_ENABLE_TRIGGERS
class PPCreateTrigger;
class PPDropTrigger;
#endif

#ifdef SQL_CONNECTION
class PPFnSQLBase;
class PPFnSQLConnect;
class PPFnSQLExecute;
class PPFnSQLPrepare;
class PPFnSQLClose;
class PPFnSQLCommit;
class PPFnSQLRollback;
#endif

class PPAbsPath;
class PPDmStringValue;
class PPDmTypedValue;
class PPFnMaxMin;
class PPFnSumAvg;
class PPFnCount;
class PPAxisStep;
class PPFnTrue;
class PPFnFalse;
class PPFnNot;
class PPFnBoolean;
class PPCalculate;
class PPCheckpoint;
class PPConst;
class PPConstructor;
class PPElementConstructor;
class PPAttributeConstructor;
class PPNamespaceConstructor;
class PPCommentConstructor;
class PPTextConstructor;
class PPDocumentConstructor;
class PPPIConstructor;
class PPDDO;
class PPDocInCol;
class PPFnError;
class PPFnIndexKeys;
class PPFnTrace;
class PPExcept;
class PPExplain;
class PPExtFunCall;
class PPFilterEL;
class PPFnNodeName;
class PPFnNilled;
class PPFnString;
class PPFnData;
class PPFnBaseURI;
class PPFnDocumentURI;
class PPFnStaticBaseUri;
class PPFnDefaultCollation;
class PPFnDateTimeFuncNoParam;
class PPFnDateTimeFunc;
class PPFnDateTimeFunc;
class PPFnDeepEqual;
class PPFnDocAvailable;
class PPFnColAvailable;
class PPFnGetProperty;
class PPFtScan;
class PPFunCall;
class PPGeneralComparison;
class PPLMGeneralComparison;
class PPNEQGeneralComparison;
class PPEQLGeneralComparison;
class PPIf;
class PPIndexScan;
class PPIntersect;
class PPLast;
class PPLet;
class PPNil;
class PPNodeComparison;
class PPFnName;
class PPFnLocalName;
class PPFnNamespaceUri;
class PPFnNumber;
class PPFnRoot;
class PPNumericFuncs;
class PPFnRoundHalfToEven;
class PPOrderBy;
class PPSTuple;
class PPSLet;
class PPPatMatch;
class PPPred;
class PPPred;
class PPFnResolveQName;
class PPFnQName;
class PPFnPrefixFromQName;
class PPFnLocalNameFromQName;
class PPFnNamespaceUriFromQName;
class PPFnNamespaceUriForPrefix;
class PPFnInScopePrefixes;
class PPRange;
class PPReturn;
class PPSelect;
class PPSeqChecker;
class PPSequence;
class PPFnEmpty;
class PPFnExists;
class PPFnItemAt;
class PPFnDistinctValues;
class PPFnIndexOf;
class PPFnReverse;
class PPFnSubsequence;
class PPFnRemove;
class PPFnInsertBefore;
class PPFnZeroOrOne;
class PPFnOneOrMore;
class PPFnExactlyOne;
class PPCast;
class PPCastable;
class PPInstanceOf;
class PPTreat;
class PPTypeswitch;
class PPSpaceSequence;
class PPStore;
class PPFnConcat;
class PPFnStringJoin;
class PPFnStartsEndsWith;
class PPFnStringLength;
class PPFnNormalizeSpace;
class PPFnString;
class PPFnCodePoints;
class PPFnTranslate;
class PPFnChangeCase;
class PPFnSubsBeforeAfter;
class PPFnSubstring;
class PPFnNormalizeUnicode;
class PPFnCompare;
class PPSubsMatch;
class PPSXptr;
class PPTest;
class PPTuple;
class PPUnion;
class PPFnUriEncoding;
class PPFnResolveUri;
class PPVarDecl;
class PPVariable;
class PPGlobalVariable;
class PPXptr;

class PPPred1;
class PPPred2;

class PPFnCodePoints2String;
class PPFnString2CodePoints;
class PPFnDateTimeFunc2Params;
class CalcOp;


class LeafEffectBoolOp;
class LeafAtomOp;
class BinaryOpOr;
class BinaryOpAnd;
class BinaryOpCollation;
class BinaryOp;
class UnaryOp;

#endif

