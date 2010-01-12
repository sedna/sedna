/*
 * File:  PPExplainVisitor.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "tr/executor/base/visitor/PPExplainVisitor.h"
#include "tr/mo/mo.h"



PPExplainVisitor::PPExplainVisitor() : scm(doc_schema_node_object::create(false)),
                                       root(XNULL),
                                       parent(XNULL),
                                       left(XNULL),
                                       cached(false)
                                        
{
    /* insert_doc_node already returns indirection */
    parent = insert_doc_node(scm, "$explain", NULL);
    root = parent;
}

PPExplainVisitor::~PPExplainVisitor() 
{
    scm->drop();
}

void PPExplainVisitor::push()
{
    U_ASSERT(left != XNULL);
    
    pointers.push(xptr_pair(parent, left));
    parent = left;
    left = XNULL;
}

void PPExplainVisitor::pop()
{
    U_ASSERT(!pointers.empty());    

    xptr_pair ptrs = pointers.top();
    pointers.pop();
    parent = ptrs.first;
    left   = ptrs.second;
}
   

xptr PPExplainVisitor::result() 
{
    cached = true;
    return root;
}
    
/* Helper to insert nodes with indirection pointers */
void PPExplainVisitor::insertElement(const char* name, xptr& left, xptr& parent)
{
    if(!cached)
    {
        U_ASSERT(parent != XNULL);
    
        elog(EL_DBG, ("[EXPLAIN] Going to insert element '%s', parent (0x%x, 0x%x), left (0x%x, 0x%x)", name, 
                                                               parent.layer, parent.addr, 
                                                               left.layer, left.addr));
                                                               
        left = insert_element(indirectionDereferenceCP(left),
                              XNULL,
                              indirectionDereferenceCP(parent),
                              name,
                              xs_untyped,
                              NULL_XMLNS);
        left = getIndirectionSafeCP(left);
    }
}


void PPExplainVisitor::visit(PPDmStringValue* op)
{
    insertElement("PPDmStringValue", left, parent);
}

void PPExplainVisitor::visit(PPDmTypedValue* op)
{
    insertElement("PPDmTypedValue", left, parent);
}

void PPExplainVisitor::visit(PPDmNodeKind* op)
{
    insertElement("PPDmNodeKind", left, parent);
}

void PPExplainVisitor::visit(PPFnNodeName* op)
{
    insertElement("PPFnNodeName", left, parent);
}

void PPExplainVisitor::visit(PPFnNilled* op)
{
    insertElement("PPFnNilled", left, parent);
}

void PPExplainVisitor::visit(PPFnString* op)
{
    insertElement("PPFnString", left, parent);
}

void PPExplainVisitor::visit(PPFnData* op)
{
    insertElement("PPFnData", left, parent);
}

void PPExplainVisitor::visit(PPFnBaseURI* op)
{
    insertElement("PPFnBaseURI", left, parent);
}

void PPExplainVisitor::visit(PPFnDocumentURI* op)
{
    insertElement("PPFnDocumentURI", left, parent);
}

void PPExplainVisitor::visit(PPFnStaticBaseUri* op)
{
    insertElement("PPFnStaticBaseUri", left, parent);
}

void PPExplainVisitor::visit(PPFnDefaultCollation* op)
{
    insertElement("PPFnDefaultCollation", left, parent);
}

void PPExplainVisitor::visit(PPFnCount* op)
{
    insertElement("PPFnCount", left, parent);
}

void PPExplainVisitor::visit(PPFnMaxMin* op)
{
    insertElement("PPFnMaxMin", left, parent);
}

void PPExplainVisitor::visit(PPFnSumAvg* op)
{
    insertElement("PPFnSumAvg", left, parent);
}

void PPExplainVisitor::visit(PPAbsPath* op)
{
    insertElement("PPAbsPath", left, parent);
}

void PPExplainVisitor::visit(PPAxisAncestor* op)
{
    insertElement("PPAxisAncestor", left, parent);
}

void PPExplainVisitor::visit(PPAxisAncestorOrSelf* op)
{
    insertElement("PPAxisAncestorOrSelf", left, parent);
}

void PPExplainVisitor::visit(PPAxisAttribute* op)
{
    insertElement("PPAxisAttribute", left, parent);
}

void PPExplainVisitor::visit(PPAxisChild* op)
{
    insertElement("PPAxisChild", left, parent);
}

void PPExplainVisitor::visit(PPAxisDescendant* op)
{
    insertElement("PPAxisDescendant", left, parent);
}

void PPExplainVisitor::visit(PPAxisDescendantOrSelf* op)
{
    insertElement("PPAxisDescendantOrSelf", left, parent);
}

void PPExplainVisitor::visit(PPAxisDescendantAttr* op)
{
    insertElement("PPAxisDescendantAttr", left, parent);
}

void PPExplainVisitor::visit(PPAxisFP* op)
{
    insertElement("PPAxisFP", left, parent);
}

void PPExplainVisitor::visit(PPAxisParent* op)
{
    insertElement("PPAxisParent", left, parent);
}

void PPExplainVisitor::visit(PPAxisSelf* op)
{
    insertElement("PPAxisSelf", left, parent);
}

void PPExplainVisitor::visit(PPAxisSibling* op)
{
    insertElement("PPAxisSibling", left, parent);
}

void PPExplainVisitor::visit(PPPred1* op)
{
    insertElement("PPPred1", left, parent);
}

void PPExplainVisitor::visit(PPPred2* op)
{
    insertElement("PPPred2", left, parent);
}

void PPExplainVisitor::visit(PPFnTrue* op)
{
    insertElement("PPFnTrue", left, parent);
}

void PPExplainVisitor::visit(PPFnFalse* op)
{
    insertElement("PPFnFalse", left, parent);
}

void PPExplainVisitor::visit(PPFnNot* op)
{
    insertElement("PPFnNot", left, parent);
}

void PPExplainVisitor::visit(PPFnBoolean* op)
{
    insertElement("PPFnBoolean", left, parent);
}

void PPExplainVisitor::visit(PPCalculate* op)
{
    insertElement("PPCalculate", left, parent);
}

void PPExplainVisitor::visit(UnaryOp* op)
{
    insertElement("UnaryOp", left, parent);
}

void PPExplainVisitor::visit(BinaryOp* op)
{
    insertElement("BinaryOp", left, parent);
}

void PPExplainVisitor::visit(BinaryOpCollation* op)
{
    insertElement("BinaryOpCollation", left, parent);
}

void PPExplainVisitor::visit(BinaryOpAnd* op)
{
    insertElement("BinaryOpAnd", left, parent);
}

void PPExplainVisitor::visit(BinaryOpOr* op)
{
    insertElement("BinaryOpOr", left, parent);
}

void PPExplainVisitor::visit(LeafAtomOp* op)
{
    insertElement("LeafAtomOp", left, parent);
}

void PPExplainVisitor::visit(LeafEffectBoolOp* op)
{
    insertElement("LeafEffectBoolOp", left, parent);
}

void PPExplainVisitor::visit(PPElementConstructor* op)
{
    insertElement("PPElementConstructor", left, parent);
}

void PPExplainVisitor::visit(PPAttributeConstructor* op)
{
    insertElement("PPAttributeConstructor", left, parent);
}

void PPExplainVisitor::visit(PPNamespaceConstructor* op)
{
    insertElement("PPNamespaceConstructor", left, parent);
}

void PPExplainVisitor::visit(PPCommentConstructor* op)
{
    insertElement("PPCommentConstructor", left, parent);
}

void PPExplainVisitor::visit(PPTextConstructor* op)
{
    insertElement("PPTextConstructor", left, parent);
}

void PPExplainVisitor::visit(PPDocumentConstructor* op)
{
    insertElement("PPDocumentConstructor", left, parent);
}

void PPExplainVisitor::visit(PPPIConstructor* op)
{
    insertElement("PPPIConstructor", left, parent);
}

void PPExplainVisitor::visit(PPFnError* op)
{
    insertElement("PPFnError", left, parent);
}

void PPExplainVisitor::visit(PPFnTrace* op)
{
    insertElement("PPFnTrace", left, parent);
}

void PPExplainVisitor::visit(PPDebug* op)
{
    insertElement("PPDebug", left, parent);
}

void PPExplainVisitor::visit(PPExcept* op)
{
    insertElement("PPExcept", left, parent);
}

void PPExplainVisitor::visit(PPUnion* op)
{
    insertElement("PPUnion", left, parent);
}

void PPExplainVisitor::visit(PPIntersect* op)
{
    insertElement("PPIntersect", left, parent);
}

void PPExplainVisitor::visit(PPFnDeepEqual* op)
{
    insertElement("PPFnDeepEqual", left, parent);
}

void PPExplainVisitor::visit(PPFnDocAvailable* op)
{
    insertElement("PPFnDocAvailable", left, parent);
}

void PPExplainVisitor::visit(PPRange* op)
{
    insertElement("PPRange", left, parent);
}

void PPExplainVisitor::visit(PPSequence* op)
{
    insertElement("PPSequence", left, parent);
}

void PPExplainVisitor::visit(PPSpaceSequence* op)
{
    insertElement("PPSpaceSequence", left, parent);
}

void PPExplainVisitor::visit(PPFnEmpty* op)
{
    insertElement("PPFnEmpty", left, parent);
}

void PPExplainVisitor::visit(PPFnExists* op)
{
    insertElement("PPFnExists", left, parent);
}

void PPExplainVisitor::visit(PPFnItemAt* op)
{
    insertElement("PPFnItemAt", left, parent);
}

void PPExplainVisitor::visit(PPFnDistinctValues* op)
{
    insertElement("PPFnDistinctValues", left, parent);
}

void PPExplainVisitor::visit(PPFnIndexOf* op)
{
    insertElement("PPFnIndexOf", left, parent);
}

void PPExplainVisitor::visit(PPFnReverse* op)
{
    insertElement("PPFnReverse", left, parent);
}

void PPExplainVisitor::visit(PPFnSubsequence* op)
{
    insertElement("PPFnSubsequence", left, parent);
}

void PPExplainVisitor::visit(PPFnRemove* op)
{
    insertElement("PPFnRemove", left, parent);
}

void PPExplainVisitor::visit(PPFnInsertBefore* op)
{
    insertElement("PPFnInsertBefore", left, parent);
}

void PPExplainVisitor::visit(PPFnZeroOrOne* op)
{
    insertElement("PPFnZeroOrOne", left, parent);
}

void PPExplainVisitor::visit(PPFnOneOrMore* op)
{
    insertElement("PPFnOneOrMore", left, parent);
}

void PPExplainVisitor::visit(PPFnExactlyOne* op)
{
    insertElement("PPFnExactlyOne", left, parent);
}

void PPExplainVisitor::visit(PPFnDateTimeFuncNoParam* op)
{
    insertElement("PPFnDateTimeFuncNoParam", left, parent);
}

void PPExplainVisitor::visit(PPFnDateTimeFunc* op)
{
    insertElement("PPFnDateTimeFunc", left, parent);
}

void PPExplainVisitor::visit(PPFnDateTimeFunc2Params* op)
{
    insertElement("PPFnDateTimeFunc2Params", left, parent);
}

void PPExplainVisitor::visit(PPADFilter* op)
{
    insertElement("PPADFilter", left, parent);
}

void PPExplainVisitor::visit(PPDAFilter* op)
{
    insertElement("PPDAFilter", left, parent);
}

void PPExplainVisitor::visit(PPFilterEL* op)
{
    insertElement("PPFilterEL", left, parent);
}

void PPExplainVisitor::visit(PPCheckpoint* op)
{
    insertElement("PPCheckpoint", left, parent);
}

void PPExplainVisitor::visit(PPTest* op)
{
    insertElement("PPTest", left, parent);
}

void PPExplainVisitor::visit(PPConst* op)
{
    insertElement("PPConst", left, parent);
}

void PPExplainVisitor::visit(PPDDO* op)
{
    insertElement("PPDDO", left, parent);
}

void PPExplainVisitor::visit(PPSXptr* op)
{
    insertElement("PPSXptr", left, parent);
}

void PPExplainVisitor::visit(PPDocInCol* op)
{
    insertElement("PPDocInCol", left, parent);
}

void PPExplainVisitor::visit(PPExtFunCall* op)
{
    insertElement("PPExtFunCall", left, parent);
}

void PPExplainVisitor::visit(PPFnGetProperty* op)
{
    insertElement("PPFnGetProperty", left, parent);
}

void PPExplainVisitor::visit(PPIndexScan* op)
{
    insertElement("PPIndexScan", left, parent);
}

void PPExplainVisitor::visit(PPLast* op)
{
    insertElement("PPLast", left, parent);
}

void PPExplainVisitor::visit(PPNil* op)
{
    insertElement("PPNil", left, parent);
}

void PPExplainVisitor::visit(PPScan* op)
{
    insertElement("PPScan", left, parent);
}

void PPExplainVisitor::visit(PPSelect* op)
{
    insertElement("PPSelect", left, parent);
}

void PPExplainVisitor::visit(PPSeqChecker* op)
{
    insertElement("PPSeqChecker", left, parent);
}

void PPExplainVisitor::visit(PPStore* op)
{
    insertElement("PPStore", left, parent);
}

void PPExplainVisitor::visit(PPTuple* op)
{
    insertElement("PPTuple", left, parent);
}

void PPExplainVisitor::visit(PPUp* op)
{
    insertElement("PPUp", left, parent);
}

void PPExplainVisitor::visit(PPVarDecl* op)
{
    insertElement("PPVarDecl", left, parent);
}

void PPExplainVisitor::visit(PPVariable* op)
{
    insertElement("PPVariable", left, parent);
}

void PPExplainVisitor::visit(PPGlobalVariable* op)
{
    insertElement("PPGlobalVariable", left, parent);
}

void PPExplainVisitor::visit(PPXptr* op)
{
    insertElement("PPXptr", left, parent);
}



#ifdef SE_ENABLE_DTSEARCH
void PPExplainVisitor::visit(PPFtHighlight* op)
{
    insertElement("PPFtHighlight", left, parent);
}
void PPExplainVisitor::visit(PPFtScan* op)
{
    insertElement("PPFtScan", left, parent);
}
#endif /* SE_ENABLE_DTSEARCH */



#ifdef SE_ENABLE_FTSEARCH
void PPExplainVisitor::visit(PPFtIndexScan* op)
{
    insertElement("PPFtIndexScan", left, parent);
}
void PPExplainVisitor::visit(PPFtIndexScan2* op)
{
    insertElement("PPFtIndexScan2", left, parent);
}
#endif /* SE_ENABLE_FTSEARCH */



void PPExplainVisitor::visit(PPFunCall* op)
{
    insertElement("PPFunCall", left, parent);
}

void PPExplainVisitor::visit(PPGeneralComparison* op)
{
    insertElement("PPGeneralComparison", left, parent);
}

void PPExplainVisitor::visit(PPLMGeneralComparison* op)
{
    insertElement("PPLMGeneralComparison", left, parent);
}

void PPExplainVisitor::visit(PPNEQGeneralComparison* op)
{
    insertElement("PPNEQGeneralComparison", left, parent);
}

void PPExplainVisitor::visit(PPEQLGeneralComparison* op)
{
    insertElement("PPEQLGeneralComparison", left, parent);
}

void PPExplainVisitor::visit(PPNodeComparison* op)
{
    insertElement("PPNodeComparison", left, parent);
}

void PPExplainVisitor::visit(PPIf* op)
{
    insertElement("PPIf", left, parent);
}

void PPExplainVisitor::visit(PPLet* op)
{
    insertElement("PPLet", left, parent);
}

void PPExplainVisitor::visit(PPOrderBy* op)
{
    insertElement("PPOrderBy", left, parent);
}

void PPExplainVisitor::visit(PPSTuple* op)
{
    insertElement("PPSTuple", left, parent);
}

void PPExplainVisitor::visit(PPSLet* op)
{
    insertElement("PPSLet", left, parent);
}

void PPExplainVisitor::visit(PPReturn* op)
{
    insertElement("PPReturn", left, parent);
}

void PPExplainVisitor::visit(PPFnName* op)
{
    insertElement("PPFnName", left, parent);
}

void PPExplainVisitor::visit(PPFnLocalName* op)
{
    insertElement("PPFnLocalName", left, parent);
}

void PPExplainVisitor::visit(PPFnNamespaceUri* op)
{
    insertElement("PPFnNamespaceUri", left, parent);
}

void PPExplainVisitor::visit(PPFnNumber* op)
{
    insertElement("PPFnNumber", left, parent);
}

void PPExplainVisitor::visit(PPFnRoot* op)
{
    insertElement("PPFnRoot", left, parent);
}

void PPExplainVisitor::visit(PPNumericFuncs* op)
{
    insertElement("PPNumericFuncs", left, parent);
}

void PPExplainVisitor::visit(PPFnRoundHalfToEven* op)
{
    insertElement("PPFnRoundHalfToEven", left, parent);
}

void PPExplainVisitor::visit(PPPatMatch* op)
{
    insertElement("PPPatMatch", left, parent);
}

void PPExplainVisitor::visit(PPFnResolveQName* op)
{
    insertElement("PPFnResolveQName", left, parent);
}

void PPExplainVisitor::visit(PPFnQName* op)
{
    insertElement("PPFnQName", left, parent);
}

void PPExplainVisitor::visit(PPFnPrefixFromQName* op)
{
    insertElement("PPFnPrefixFromQName", left, parent);
}

void PPExplainVisitor::visit(PPFnLocalNameFromQName* op)
{
    insertElement("PPFnLocalNameFromQName", left, parent);
}

void PPExplainVisitor::visit(PPFnNamespaceUriFromQName* op)
{
    insertElement("PPFnNamespaceUriFromQName", left, parent);
}

void PPExplainVisitor::visit(PPFnNamespaceUriForPrefix* op)
{
    insertElement("PPFnNamespaceUriForPrefix", left, parent);
}

void PPExplainVisitor::visit(PPFnInScopePrefixes* op)
{
    insertElement("PPFnInScopePrefixes", left, parent);
}

void PPExplainVisitor::visit(PPCast* op)
{
    insertElement("PPCast", left, parent);
}

void PPExplainVisitor::visit(PPCastable* op)
{
    insertElement("PPCastable", left, parent);
}

void PPExplainVisitor::visit(PPTreat* op)
{
    insertElement("PPTreat", left, parent);
}

void PPExplainVisitor::visit(PPTypeswitch* op)
{
    insertElement("PPTypeswitch", left, parent);
}

void PPExplainVisitor::visit(PPInstanceOf* op)
{
    insertElement("PPInstanceOf", left, parent);
}



#ifdef SQL_CONNECTION
void PPExplainVisitor::visit(PPFnSQLConnect* op)
{
    insertElement("PPFnSQLConnect", left, parent);
}
void PPExplainVisitor::visit(PPFnSQLExecute* op)
{
    insertElement("PPFnSQLExecute", left, parent);
}
void PPExplainVisitor::visit(PPFnSQLPrepare* op)
{
    insertElement("PPFnSQLPrepare", left, parent);
}
void PPExplainVisitor::visit(PPFnSQLClose* op)
{
    insertElement("PPFnSQLClose", left, parent);
}
void PPExplainVisitor::visit(PPFnSQLCommit* op)
{
    insertElement("PPFnSQLCommit", left, parent);
}
void PPExplainVisitor::visit(PPFnSQLRollback* op)
{
    insertElement("PPFnSQLRollback", left, parent);
}
#endif /* SQL_CONNECTION */



void PPExplainVisitor::visit(PPFnConcat* op)
{
    insertElement("PPFnConcat", left, parent);
}

void PPExplainVisitor::visit(PPFnStringJoin* op)
{
    insertElement("PPFnStringJoin", left, parent);
}

void PPExplainVisitor::visit(PPFnStartsEndsWith* op)
{
    insertElement("PPFnStartsEndsWith", left, parent);
}

void PPExplainVisitor::visit(PPFnStringLength* op)
{
    insertElement("PPFnStringLength", left, parent);
}

void PPExplainVisitor::visit(PPFnNormalizeSpace* op)
{
    insertElement("PPFnNormalizeSpace", left, parent);
}

void PPExplainVisitor::visit(PPFnNormalizeUnicode* op)
{
    insertElement("PPFnNormalizeUnicode", left, parent);
}

void PPExplainVisitor::visit(PPFnString2CodePoints* op)
{
    insertElement("PPFnString2CodePoints", left, parent);
}

void PPExplainVisitor::visit(PPFnCodePoints2String* op)
{
    insertElement("PPFnCodePoints2String", left, parent);
}

void PPExplainVisitor::visit(PPFnTranslate* op)
{
    insertElement("PPFnTranslate", left, parent);
}

void PPExplainVisitor::visit(PPFnChangeCase* op)
{
    insertElement("PPFnChangeCase", left, parent);
}

void PPExplainVisitor::visit(PPFnSubsBeforeAfter* op)
{
    insertElement("PPFnSubsBeforeAfter", left, parent);
}

void PPExplainVisitor::visit(PPFnSubstring* op)
{
    insertElement("PPFnSubstring", left, parent);
}

void PPExplainVisitor::visit(PPFnCompare* op)
{
    insertElement("PPFnCompare", left, parent);
}

void PPExplainVisitor::visit(PPSubsMatch* op)
{
    insertElement("PPSubsMatch", left, parent);
}

void PPExplainVisitor::visit(PPFnUriEncoding* op)
{
    insertElement("PPFnUriEncoding", left, parent);
}

void PPExplainVisitor::visit(PPFnResolveUri* op)
{
    insertElement("PPFnResolveUri", left, parent);
}

void PPExplainVisitor::visit(PPQueryRoot* op)
{
    insertElement("PPQueryRoot", left, parent);
}

void PPExplainVisitor::visit(PPBulkLoad* op)
{
    insertElement("PPBulkLoad", left, parent);
}

void PPExplainVisitor::visit(PPCreateFtIndex* op)
{
    insertElement("PPCreateFtIndex", left, parent);
}

void PPExplainVisitor::visit(PPCreateIndex* op)
{
    insertElement("PPCreateIndex", left, parent);
}

void PPExplainVisitor::visit(PPCreateDocument* op)
{
    insertElement("PPCreateDocument", left, parent);
}

void PPExplainVisitor::visit(PPCreateCollection* op)
{
    insertElement("PPCreateCollection", left, parent);
}

void PPExplainVisitor::visit(PPCreateDocumentInCollection* op)
{
    insertElement("PPCreateDocumentInCollection", left, parent);
}

void PPExplainVisitor::visit(PPCreateTrigger* op)
{
    insertElement("PPCreateTrigger", left, parent);
}

void PPExplainVisitor::visit(PPDeleteDeep* op)
{
    insertElement("PPDeleteDeep", left, parent);
}

void PPExplainVisitor::visit(PPDeleteUndeep* op)
{
    insertElement("PPDeleteUndeep", left, parent);
}

void PPExplainVisitor::visit(PPDropFtIndex* op)
{
    insertElement("PPDropFtIndex", left, parent);
}

void PPExplainVisitor::visit(PPDropIndex* op)
{
    insertElement("PPDropIndex", left, parent);
}

void PPExplainVisitor::visit(PPDropDocument* op)
{
    insertElement("PPDropDocument", left, parent);
}

void PPExplainVisitor::visit(PPDropCollection* op)
{
    insertElement("PPDropCollection", left, parent);
}

void PPExplainVisitor::visit(PPDropDocumentInCollection* op)
{
    insertElement("PPDropDocumentInCollection", left, parent);
}

void PPExplainVisitor::visit(PPLoadModule* op)
{
    insertElement("PPLoadModule", left, parent);
}

void PPExplainVisitor::visit(PPDropModule* op)
{
    insertElement("PPDropModule", left, parent);
}

void PPExplainVisitor::visit(PPDropTrigger* op)
{
    insertElement("PPDropTrigger", left, parent);
}

void PPExplainVisitor::visit(PPInsertTo* op)
{
    insertElement("PPInsertTo", left, parent);
}

void PPExplainVisitor::visit(PPInsertBefore* op)
{
    insertElement("PPInsertBefore", left, parent);
}

void PPExplainVisitor::visit(PPInsertFollowing* op)
{
    insertElement("PPInsertFollowing", left, parent);
}

void PPExplainVisitor::visit(PPRename* op)
{
    insertElement("PPRename", left, parent);
}

void PPExplainVisitor::visit(PPReplace* op)
{
    insertElement("PPReplace", left, parent);
}

void PPExplainVisitor::visit(PPRetrieveDS* op)
{
    insertElement("PPRetrieveDS", left, parent);
}

void PPExplainVisitor::visit(PPRetrieveMetadata* op)
{
    insertElement("PPRetrieveMetadata", left, parent);
}

void PPExplainVisitor::visit(PPCreateUser* op)
{
    insertElement("PPCreateUser", left, parent);
}

void PPExplainVisitor::visit(PPDropUser* op)
{
    insertElement("PPDropUser", left, parent);
}

void PPExplainVisitor::visit(PPAlterUser* op)
{
    insertElement("PPAlterUser", left, parent);
}

void PPExplainVisitor::visit(PPCreateRole* op)
{
    insertElement("PPCreateRole", left, parent);
}

void PPExplainVisitor::visit(PPDropRole* op)
{
    insertElement("PPDropRole", left, parent);
}

void PPExplainVisitor::visit(PPGrantRole* op)
{
    insertElement("PPGrantRole", left, parent);
}

void PPExplainVisitor::visit(PPGrantRevokePriv* op)
{
    insertElement("PPGrantRevokePriv", left, parent);
}

void PPExplainVisitor::visit(PPRevokeRole* op)
{
    insertElement("PPRevokeRole", left, parent);
}

