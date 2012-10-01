/*
 * File:  PPExplainVisitor.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPEXPLAIN_VISITOR_H
#define __PPEXPLAIN_VISITOR_H

#include <stack>

#include "common/sedna.h"
#include "common/xptr/xptr.h"

#include "tr/executor/base/visitor/PPVisitor.h"


typedef std::pair<xptr, xptr> xptr_pair;


class PPExplainVisitor : public PPVisitor {

private:
    dynamic_context* cxt;
    xptr parent, left, left_inside;
    /* Var_dsc -> name map */
    var_map_id_name var_names;
    /*
     * Turns on profile mode. In this mode visitor additionally
     * collects various profile statistics from physical operations.
     */
    bool profiler_mode;
    /* Maintains stack of indirection pointers */
    std::stack<xptr_pair> pointers;

    void push();
    void insertOperationElement(const char* name,
                                xptr& left,
                                const xptr& parent,
                                const PPIterator* op = NULL,
                                const PPQueryEssence* qep = NULL);

public:
    PPExplainVisitor(dynamic_context* _cxt_,
                     xptr root,
                     var_map_id_name _var_names_,
                     bool _profiler_mode_ = false);

    virtual ~PPExplainVisitor();

    virtual void push(PPIterator* op)       { push(); }
    virtual void push(CalcOp* op)           { push(); }
    virtual void push(PPQueryEssence* op)   { push(); }
    virtual void pop();

    virtual void visit(PPDmStringValue* op);
    virtual void visit(PPDmTypedValue* op);
    virtual void visit(PPFnNodeName* op);
    virtual void visit(PPFnNilled* op);
    virtual void visit(PPFnString* op);
    virtual void visit(PPFnData* op);
    virtual void visit(PPFnBaseURI* op);
    virtual void visit(PPFnDocumentURI* op);
    virtual void visit(PPFnStaticBaseUri* op);
    virtual void visit(PPFnDefaultCollation* op);
    virtual void visit(PPFnCount* op);
    virtual void visit(PPFnMaxMin* op);
    virtual void visit(PPFnSumAvg* op);
    virtual void visit(PPAbsPath* op);
    virtual void visit(PPAxisStep* op);
    virtual void visit(PPPred1* op);
    virtual void visit(PPPred2* op);
    virtual void visit(PPFnTrue* op);
    virtual void visit(PPFnFalse* op);
    virtual void visit(PPFnNot* op);
    virtual void visit(PPFnBoolean* op);
    virtual void visit(PPCalculate* op);
    virtual void visit(UnaryOp* op);
    virtual void visit(BinaryOp* op);
    virtual void visit(BinaryOpCollation* op);
    virtual void visit(BinaryOpAnd* op);
    virtual void visit(BinaryOpOr* op);
    virtual void visit(LeafAtomOp* op);
    virtual void visit(LeafEffectBoolOp* op);
    virtual void visit(PPElementConstructor* op);
    virtual void visit(PPAttributeConstructor* op);
    virtual void visit(PPNamespaceConstructor* op);
    virtual void visit(PPCommentConstructor* op);
    virtual void visit(PPTextConstructor* op);
    virtual void visit(PPDocumentConstructor* op);
    virtual void visit(PPPIConstructor* op);
    virtual void visit(PPFnError* op);
    virtual void visit(PPFnTrace* op);
    virtual void visit(PPExcept* op);
    virtual void visit(PPUnion* op);
    virtual void visit(PPIntersect* op);
    virtual void visit(PPFnDeepEqual* op);
    virtual void visit(PPFnDocAvailable* op);
    virtual void visit(PPRange* op);
    virtual void visit(PPSequence* op);
    virtual void visit(PPSpaceSequence* op);
    virtual void visit(PPFnEmpty* op);
    virtual void visit(PPFnExists* op);
    virtual void visit(PPFnItemAt* op);
    virtual void visit(PPFnDistinctValues* op);
    virtual void visit(PPFnIndexOf* op);
    virtual void visit(PPFnReverse* op);
    virtual void visit(PPFnSubsequence* op);
    virtual void visit(PPFnRemove* op);
    virtual void visit(PPFnInsertBefore* op);
    virtual void visit(PPFnZeroOrOne* op);
    virtual void visit(PPFnOneOrMore* op);
    virtual void visit(PPFnExactlyOne* op);
    virtual void visit(PPFnDateTimeFuncNoParam* op);
    virtual void visit(PPFnDateTimeFunc* op);
    virtual void visit(PPFnDateTimeFunc2Params* op);
    virtual void visit(PPCheckpoint* op);
    virtual void visit(PPFilterEL* op);
    virtual void visit(PPTest* op);
    virtual void visit(PPConst* op);
    virtual void visit(PPDDO* op);
    virtual void visit(PPSXptr* op);
    virtual void visit(PPDocInCol* op);
    virtual void visit(PPExtFunCall* op);
    virtual void visit(PPFnGetProperty* op);
    virtual void visit(PPIndexScan* op);
    virtual void visit(PPLast* op);
    virtual void visit(PPNil* op);
    virtual void visit(PPSelect* op);
    virtual void visit(PPSeqChecker* op);
    virtual void visit(PPStore* op);
    virtual void visit(PPTuple* op);
    virtual void visit(PPVarDecl* op);
    virtual void visit(PPVariable* op);
    virtual void visit(PPGlobalVariable* op);
    virtual void visit(PPXptr* op);

#ifdef SE_ENABLE_DTSEARCH
    virtual void visit(PPFtScan* op);
#endif
#ifdef SE_ENABLE_FTSEARCH
    virtual void visit(PPFtHighlight* op);
    virtual void visit(PPFtIndexScan* op);
    virtual void visit(PPFtIndexScan2* op);
    virtual void visit(PPFtIndexDict* op);
#endif

    virtual void visit(PPFunCall* op);
    virtual void visit(PPGeneralComparison* op);
    virtual void visit(PPLMGeneralComparison* op);
    virtual void visit(PPNEQGeneralComparison* op);
    virtual void visit(PPEQLGeneralComparison* op);
    virtual void visit(PPNodeComparison* op);
    virtual void visit(PPIf* op);
    virtual void visit(PPLet* op);
    virtual void visit(PPOrderBy* op);
    virtual void visit(PPSTuple* op);
    virtual void visit(PPSLet* op);
    virtual void visit(PPReturn* op);
    virtual void visit(PPFnName* op);
    virtual void visit(PPFnLocalName* op);
    virtual void visit(PPFnNamespaceUri* op);
    virtual void visit(PPFnNumber* op);
    virtual void visit(PPFnRoot* op);
    virtual void visit(PPNumericFuncs* op);
    virtual void visit(PPFnRoundHalfToEven* op);
    virtual void visit(PPPatMatch* op);
    virtual void visit(PPFnResolveQName* op);
    virtual void visit(PPFnQName* op);
    virtual void visit(PPFnPrefixFromQName* op);
    virtual void visit(PPFnLocalNameFromQName* op);
    virtual void visit(PPFnNamespaceUriFromQName* op);
    virtual void visit(PPFnNamespaceUriForPrefix* op);
    virtual void visit(PPFnInScopePrefixes* op);
    virtual void visit(PPCast* op);
    virtual void visit(PPCastable* op);
    virtual void visit(PPTreat* op);
    virtual void visit(PPTypeswitch* op);
    virtual void visit(PPInstanceOf* op);

#ifdef SQL_CONNECTION
    virtual void visit(PPFnSQLConnect* op);
    virtual void visit(PPFnSQLExecute* op);
    virtual void visit(PPFnSQLPrepare* op);
    virtual void visit(PPFnSQLClose* op);
    virtual void visit(PPFnSQLCommit* op);
    virtual void visit(PPFnSQLRollback* op);
#endif /* SQL_CONNECTION */

    virtual void visit(PPFnConcat* op);
    virtual void visit(PPFnStringJoin* op);
    virtual void visit(PPFnStartsEndsWith* op);
    virtual void visit(PPFnStringLength* op);
    virtual void visit(PPFnNormalizeSpace* op);
    virtual void visit(PPFnNormalizeUnicode* op);
    virtual void visit(PPFnString2CodePoints* op);
    virtual void visit(PPFnCodePoints2String* op);
    virtual void visit(PPFnTranslate* op);
    virtual void visit(PPFnChangeCase* op);
    virtual void visit(PPFnSubsBeforeAfter* op);
    virtual void visit(PPFnSubstring* op);
    virtual void visit(PPFnCompare* op);
    virtual void visit(PPSubsMatch* op);
    virtual void visit(PPFnUriEncoding* op);
    virtual void visit(PPFnResolveUri* op);
    virtual void visit(PPQueryRoot* op);
    virtual void visit(PPSubQuery* op);
    virtual void visit(PPBulkLoad* op);

#ifdef SE_ENABLE_FTSEARCH
    virtual void visit(PPCreateFtIndex* op);
    virtual void visit(PPDropFtIndex* op);
#endif

    virtual void visit(PPCreateIndex* op);
    virtual void visit(PPCreateDocument* op);
    virtual void visit(PPCreateCollection* op);
    virtual void visit(PPCreateDocumentInCollection* op);

#ifdef SE_ENABLE_TRIGGERS
    virtual void visit(PPCreateTrigger* op);
    virtual void visit(PPDropTrigger* op);
#endif

    virtual void visit(PPDeleteDeep* op);
    virtual void visit(PPDeleteUndeep* op);
    virtual void visit(PPDropIndex* op);
    virtual void visit(PPDropDocument* op);
    virtual void visit(PPDropCollection* op);
    virtual void visit(PPDropDocumentInCollection* op);
    virtual void visit(PPLoadModule* op);
    virtual void visit(PPDropModule* op);
    virtual void visit(PPInsertTo* op);
    virtual void visit(PPInsertBefore* op);
    virtual void visit(PPInsertFollowing* op);
    virtual void visit(PPRename* op);
    virtual void visit(PPReplace* op);
    virtual void visit(PPRetrieveDS* op);
    virtual void visit(PPRetrieveMetadata* op);
    virtual void visit(PPCreateUser* op);
    virtual void visit(PPDropUser* op);
    virtual void visit(PPAlterUser* op);
    virtual void visit(PPCreateRole* op);
    virtual void visit(PPDropRole* op);
    virtual void visit(PPGrantRole* op);
    virtual void visit(PPGrantRevokePriv* op);
    virtual void visit(PPRevokeRole* op);
};

#endif /* __PPEXPLAIN_VISITOR_H */
