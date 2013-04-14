/*
 * File:  PPVisitor.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPVISITOR_H
#define _PPVISITOR_H

#include "common/sedna.h"
#include "tr/executor/base/PPOperations.h"
#include "tr/executor/xqops/PPExtFunCall.h"

class PPVisitor {

public:

    virtual ~PPVisitor() {};

    virtual void push(PPIterator* op)                    = 0;
    virtual void push(CalcOp* op)                        = 0;
    virtual void push(PPQueryEssence* op)                = 0;
    virtual void pop()                                   = 0;

    /* Accessor function and operations */
    virtual void visit(PPDmStringValue* op)              = 0;
    virtual void visit(PPDmTypedValue* op)               = 0;
    virtual void visit(PPFnNodeName* op)                 = 0;
    virtual void visit(PPFnNilled* op)                   = 0;
    virtual void visit(PPFnString* op)                   = 0;
    virtual void visit(PPFnData* op)                     = 0;
    virtual void visit(PPFnBaseURI* op)                  = 0;
    virtual void visit(PPFnDocumentURI* op)              = 0;
    virtual void visit(PPFnStaticBaseUri* op)            = 0;
    virtual void visit(PPFnDefaultCollation* op)         = 0;

    /* Aggregate functions */
    virtual void visit(PPFnCount* op)                    = 0;
    virtual void visit(PPFnMaxMin* op)                   = 0;
    virtual void visit(PPFnSumAvg* op)                   = 0;

    /* XPath expressions */
    virtual void visit(PPAbsPath* op)                    = 0;
    virtual void visit(PPAxisStep* op)                   = 0;
    virtual void visit(PPPred1* op)                      = 0;
    virtual void visit(PPPred2* op)                      = 0;

    /* Boolean functions and operators */
    virtual void visit(PPFnTrue* op)                     = 0;
    virtual void visit(PPFnFalse* op)                    = 0;
    virtual void visit(PPFnNot* op)                      = 0;
    virtual void visit(PPFnBoolean* op)                  = 0;

    /* Arithmetic expressions */
    virtual void visit(PPCalculate* op)                  = 0;
    virtual void visit(UnaryOp* op)                      = 0;
    virtual void visit(BinaryOp* op)                     = 0;
    virtual void visit(BinaryOpCollation* op)            = 0;
    virtual void visit(BinaryOpAnd* op)                  = 0;
    virtual void visit(BinaryOpOr* op)                   = 0;
    virtual void visit(LeafAtomOp* op)                   = 0;
    virtual void visit(LeafEffectBoolOp* op)             = 0;

    /* Constructor expressions */
    virtual void visit(PPElementConstructor* op)         = 0;
    virtual void visit(PPAttributeConstructor* op)       = 0;
    virtual void visit(PPNamespaceConstructor* op)       = 0;
    virtual void visit(PPCommentConstructor* op)         = 0;
    virtual void visit(PPTextConstructor* op)            = 0;
    virtual void visit(PPDocumentConstructor* op)        = 0;
    virtual void visit(PPPIConstructor* op)              = 0;

    /* Error and trace functions */
    virtual void visit(PPFnError* op)                    = 0;
    virtual void visit(PPFnTrace* op)                    = 0;

    /* Sequence functions and operations */
    virtual void visit(PPExcept* op)                     = 0;
    virtual void visit(PPUnion* op)                      = 0;
    virtual void visit(PPIntersect* op)                  = 0;
    virtual void visit(PPFnDeepEqual* op)                = 0;
    virtual void visit(PPFnDocAvailable* op)             = 0;
    virtual void visit(PPFnColAvailable* op)             = 0;
    virtual void visit(PPRange* op)                      = 0;
    virtual void visit(PPSequence* op)                   = 0;
    virtual void visit(PPSpaceSequence* op)              = 0;
    virtual void visit(PPFnEmpty* op)                    = 0;
    virtual void visit(PPFnExists* op)                   = 0;
    virtual void visit(PPFnItemAt* op)                   = 0;
    virtual void visit(PPFnDistinctValues* op)           = 0;
    virtual void visit(PPFnIndexOf* op)                  = 0;
    virtual void visit(PPFnReverse* op)                  = 0;
    virtual void visit(PPFnSubsequence* op)              = 0;
    virtual void visit(PPFnRemove* op)                   = 0;
    virtual void visit(PPFnInsertBefore* op)             = 0;
    virtual void visit(PPFnZeroOrOne* op)                = 0;
    virtual void visit(PPFnOneOrMore* op)                = 0;
    virtual void visit(PPFnExactlyOne* op)               = 0;

    /* Date time functions */
    virtual void visit(PPFnDateTimeFuncNoParam* op)      = 0;
    virtual void visit(PPFnDateTimeFunc* op)             = 0;
    virtual void visit(PPFnDateTimeFunc2Params* op)      = 0;

    /* Sedna specific and non classified operations */
    virtual void visit(PPFilterEL* op)                   = 0;
    virtual void visit(PPCheckpoint* op)                 = 0;
    virtual void visit(PPTest* op)                       = 0;
    virtual void visit(PPConst* op)                      = 0;
    virtual void visit(PPDDO* op)                        = 0;
    virtual void visit(PPSXptr* op)                      = 0;
    virtual void visit(PPDocInCol* op)                   = 0;
    virtual void visit(PPExtFunCall* op)                 = 0;
    virtual void visit(PPFnGetProperty* op)              = 0;
    virtual void visit(PPFnIndexKeys* op)                = 0;
    virtual void visit(PPIndexScan* op)                  = 0;
    virtual void visit(PPLast* op)                       = 0;
    virtual void visit(PPNil* op)                        = 0;
    virtual void visit(PPSelect* op)                     = 0;
    virtual void visit(PPSeqChecker* op)                 = 0;
    virtual void visit(PPStore* op)                      = 0;
    virtual void visit(PPTuple* op)                      = 0;
    virtual void visit(PPVarDecl* op)                    = 0;
    virtual void visit(PPVariable* op)                   = 0;
    virtual void visit(PPGlobalVariable* op)             = 0;
    virtual void visit(PPXptr* op)                       = 0;

    /* Full text search functions */
#ifdef SE_ENABLE_DTSEARCH
    virtual void visit(PPFtScan* op)                     = 0;
#endif
#ifdef SE_ENABLE_FTSEARCH
    virtual void visit(PPFtHighlight* op)                = 0;
    virtual void visit(PPFtIndexScan* op)                = 0;
    virtual void visit(PPFtIndexScan2* op)               = 0;
    virtual void visit(PPFtIndexDict* op)                = 0;
#endif

    /* Function call */
    virtual void visit(PPFunCall* op)                    = 0;

    /* General and node comparison */
    virtual void visit(PPGeneralComparison* op)          = 0;
    virtual void visit(PPLMGeneralComparison* op)        = 0;
    virtual void visit(PPNEQGeneralComparison* op)       = 0;
    virtual void visit(PPEQLGeneralComparison* op)       = 0;
    virtual void visit(PPNodeComparison* op)             = 0;

    /* Conditional expressions */
    virtual void visit(PPIf* op)                         = 0;

    /* FLOWR expression */
    virtual void visit(PPLet* op)                        = 0;
    virtual void visit(PPOrderBy* op)                    = 0;
    virtual void visit(PPSTuple* op)                     = 0;
    virtual void visit(PPSLet* op)                       = 0;
    virtual void visit(PPReturn* op)                     = 0;

    /* Node operations and functions */
    virtual void visit(PPFnName* op)                     = 0;
    virtual void visit(PPFnLocalName* op)                = 0;
    virtual void visit(PPFnNamespaceUri* op)             = 0;
    virtual void visit(PPFnNumber* op)                   = 0;
    virtual void visit(PPFnRoot* op)                     = 0;

    /* Numeric functions */
    virtual void visit(PPNumericFuncs* op)               = 0;
    virtual void visit(PPFnRoundHalfToEven* op)          = 0;

    /* Regular expressions functions */
    virtual void visit(PPPatMatch* op)                   = 0;

    /* QName functions */
    virtual void visit(PPFnResolveQName* op)             = 0;
    virtual void visit(PPFnQName* op)                    = 0;
    virtual void visit(PPFnPrefixFromQName* op)          = 0;
    virtual void visit(PPFnLocalNameFromQName* op)       = 0;
    virtual void visit(PPFnNamespaceUriFromQName* op)    = 0;
    virtual void visit(PPFnNamespaceUriForPrefix* op)    = 0;
    virtual void visit(PPFnInScopePrefixes* op)          = 0;

    /* Expressions on sequencet types*/
    virtual void visit(PPCast* op)                       = 0;
    virtual void visit(PPCastable* op)                   = 0;
    virtual void visit(PPTreat* op)                      = 0;
    virtual void visit(PPTypeswitch* op)                 = 0;
    virtual void visit(PPInstanceOf* op)                 = 0;

    /* SQL connection */
#ifdef SQL_CONNECTION
    virtual void visit(PPFnSQLConnect* op)               = 0;
    virtual void visit(PPFnSQLExecute* op)               = 0;
    virtual void visit(PPFnSQLPrepare* op)               = 0;
    virtual void visit(PPFnSQLClose* op)                 = 0;
    virtual void visit(PPFnSQLCommit* op)                = 0;
    virtual void visit(PPFnSQLRollback* op)              = 0;
#endif

    /* String functions */
    virtual void visit(PPFnConcat* op)                   = 0;
    virtual void visit(PPFnStringJoin* op)               = 0;
    virtual void visit(PPFnStartsEndsWith* op)           = 0;
    virtual void visit(PPFnStringLength* op)             = 0;
    virtual void visit(PPFnNormalizeSpace* op)           = 0;
    virtual void visit(PPFnNormalizeUnicode* op)         = 0;
    virtual void visit(PPFnString2CodePoints* op)        = 0;
    virtual void visit(PPFnCodePoints2String* op)        = 0;
    virtual void visit(PPFnTranslate* op)                = 0;
    virtual void visit(PPFnChangeCase* op)               = 0;
    virtual void visit(PPFnSubsBeforeAfter* op)          = 0;
    virtual void visit(PPFnSubstring* op)                = 0;
    virtual void visit(PPFnCompare* op)                  = 0;
    virtual void visit(PPSubsMatch* op)                  = 0;

    /* URI functions */
    virtual void visit(PPFnUriEncoding* op)              = 0;
    virtual void visit(PPFnResolveUri* op)               = 0;

   /* Root operations */
    virtual void visit(PPQueryRoot* op)                  = 0;
    virtual void visit(PPSubQuery* op)                   = 0;
    virtual void visit(PPBulkLoad* op)                   = 0;
    virtual void visit(PPCreateIndex* op)                = 0;
    virtual void visit(PPCreateDocument* op)             = 0;
    virtual void visit(PPCreateCollection* op)           = 0;
    virtual void visit(PPCreateDocumentInCollection* op) = 0;

#ifdef SE_ENABLE_TRIGGERS
    virtual void visit(PPCreateTrigger* op)              = 0;
    virtual void visit(PPDropTrigger* op)                = 0;
#endif

    virtual void visit(PPDeleteDeep* op)                 = 0;
    virtual void visit(PPDeleteUndeep* op)               = 0;

#ifdef SE_ENABLE_FTSEARCH
    virtual void visit(PPCreateFtIndex* op)              = 0;
    virtual void visit(PPDropFtIndex* op)                = 0;
#endif

    virtual void visit(PPDropIndex* op)                  = 0;
    virtual void visit(PPDropDocument* op)               = 0;
    virtual void visit(PPDropCollection* op)             = 0;
    virtual void visit(PPDropDocumentInCollection* op)   = 0;
    virtual void visit(PPLoadModule* op)                 = 0;
    virtual void visit(PPDropModule* op)                 = 0;
    virtual void visit(PPInsertTo* op)                   = 0;
    virtual void visit(PPInsertBefore* op)               = 0;
    virtual void visit(PPInsertFollowing* op)            = 0;
    virtual void visit(PPRename* op)                     = 0;
    virtual void visit(PPReplace* op)                    = 0;
    virtual void visit(PPRetrieveDS* op)                 = 0;
    virtual void visit(PPRetrieveMetadata* op)           = 0;
    virtual void visit(PPCreateUser* op)                 = 0;
    virtual void visit(PPDropUser* op)                   = 0;
    virtual void visit(PPAlterUser* op)                  = 0;
    virtual void visit(PPCreateRole* op)                 = 0;
    virtual void visit(PPDropRole* op)                   = 0;
    virtual void visit(PPGrantRole* op)                  = 0;
    virtual void visit(PPGrantRevokePriv* op)            = 0;
    virtual void visit(PPRevokeRole* op)                 = 0;
};

#endif /* _PPVISITOR_H */
