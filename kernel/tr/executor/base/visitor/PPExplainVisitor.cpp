/*
 * File:  PPExplainVisitor.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "tr/executor/base/visitor/PPExplainVisitor.h"
#include "tr/mo/mo.h"


xptr PPExplainVisitor::root   = XNULL;
bool PPExplainVisitor::cached = false;


PPExplainVisitor::PPExplainVisitor() : scm(doc_schema_node_object::create(false)),
                                       parent(XNULL),
                                       left(XNULL),
                                       visited(true)
                                        
{
    /* insert_doc_node already returns indirection */
    PPExplainVisitor::root = insert_doc_node(scm, "$explain", NULL);
}

PPExplainVisitor::~PPExplainVisitor() 
{
    scm->drop();
}

void PPExplainVisitor::push()
{
    U_ASSERT(visited);
    
    parent = left;
    left = XNULL;
    if(parent == XNULL) parent = root;
    parents.push(parent);
    visited = false;
}

void PPExplainVisitor::pop()
{
    U_ASSERT(visited);
    U_ASSERT(parent != XNULL && parent != root);
    U_ASSERT(!parents.empty());

    left = parent;
    parent = parents.top();
    parents.pop();
}
   

xptr PPExplainVisitor::result() 
{
    PPExplainVisitor::cached = true;
    return root;
}
    
/* Local helper to insert nodes with indirection pointers */
static inline void 
insertElement(const char* name, xptr& left, xptr& parent)
{
    if(!PPExplainVisitor::cached)
    {
        U_ASSERT(parent != XNULL);
    
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
    visited = true;
}

void PPExplainVisitor::visit(PPDmTypedValue* op)
{
    insertElement("PPDmTypedValue", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDmNodeKind* op)
{
    insertElement("PPDmNodeKind", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnNodeName* op)
{
    insertElement("PPFnNodeName", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnNilled* op)
{
    insertElement("PPFnNilled", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnString* op)
{
    insertElement("PPFnString", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnData* op)
{
    insertElement("PPFnData", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnBaseURI* op)
{
    insertElement("PPFnBaseURI", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnDocumentURI* op)
{
    insertElement("PPFnDocumentURI", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnStaticBaseUri* op)
{
    insertElement("PPFnStaticBaseUri", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnDefaultCollation* op)
{
    insertElement("PPFnDefaultCollation", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnCount* op)
{
    insertElement("PPFnCount", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnMaxMin* op)
{
    insertElement("PPFnMaxMin", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnSumAvg* op)
{
    insertElement("PPFnSumAvg", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAbsPath* op)
{
    insertElement("PPAbsPath", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAxisAncestor* op)
{
    insertElement("PPAxisAncestor", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAxisAncestorOrSelf* op)
{
    insertElement("PPAxisAncestorOrSelf", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAxisAttribute* op)
{
    insertElement("PPAxisAttribute", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAxisChild* op)
{
    insertElement("PPAxisChild", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAxisDescendant* op)
{
    insertElement("PPAxisDescendant", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAxisDescendantOrSelf* op)
{
    insertElement("PPAxisDescendantOrSelf", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAxisDescendantAttr* op)
{
    insertElement("PPAxisDescendantAttr", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAxisFP* op)
{
    insertElement("PPAxisFP", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAxisParent* op)
{
    insertElement("PPAxisParent", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAxisSelf* op)
{
    insertElement("PPAxisSelf", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAxisSibling* op)
{
    insertElement("PPAxisSibling", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPPred1* op)
{
    insertElement("PPPred1", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPPred2* op)
{
    insertElement("PPPred2", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnTrue* op)
{
    insertElement("PPFnTrue", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnFalse* op)
{
    insertElement("PPFnFalse", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnNot* op)
{
    insertElement("PPFnNot", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnBoolean* op)
{
    insertElement("PPFnBoolean", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCalculate* op)
{
    insertElement("PPCalculate", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(UnaryOp* op)
{
    insertElement("UnaryOp", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(BinaryOp* op)
{
    insertElement("BinaryOp", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(BinaryOpCollation* op)
{
    insertElement("BinaryOpCollation", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(BinaryOpAnd* op)
{
    insertElement("BinaryOpAnd", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(BinaryOpOr* op)
{
    insertElement("BinaryOpOr", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(LeafAtomOp* op)
{
    insertElement("LeafAtomOp", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(LeafEffectBoolOp* op)
{
    insertElement("LeafEffectBoolOp", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPElementConstructor* op)
{
    insertElement("PPElementConstructor", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAttributeConstructor* op)
{
    insertElement("PPAttributeConstructor", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPNamespaceConstructor* op)
{
    insertElement("PPNamespaceConstructor", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCommentConstructor* op)
{
    insertElement("PPCommentConstructor", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPTextConstructor* op)
{
    insertElement("PPTextConstructor", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDocumentConstructor* op)
{
    insertElement("PPDocumentConstructor", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPPIConstructor* op)
{
    insertElement("PPPIConstructor", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnError* op)
{
    insertElement("PPFnError", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnTrace* op)
{
    insertElement("PPFnTrace", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDebug* op)
{
    insertElement("PPDebug", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPExcept* op)
{
    insertElement("PPExcept", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPUnion* op)
{
    insertElement("PPUnion", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPIntersect* op)
{
    insertElement("PPIntersect", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnDeepEqual* op)
{
    insertElement("PPFnDeepEqual", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnDocAvailable* op)
{
    insertElement("PPFnDocAvailable", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPRange* op)
{
    insertElement("PPRange", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPSequence* op)
{
    insertElement("PPSequence", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPSpaceSequence* op)
{
    insertElement("PPSpaceSequence", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnEmpty* op)
{
    insertElement("PPFnEmpty", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnExists* op)
{
    insertElement("PPFnExists", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnItemAt* op)
{
    insertElement("PPFnItemAt", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnDistinctValues* op)
{
    insertElement("PPFnDistinctValues", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnIndexOf* op)
{
    insertElement("PPFnIndexOf", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnReverse* op)
{
    insertElement("PPFnReverse", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnSubsequence* op)
{
    insertElement("PPFnSubsequence", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnRemove* op)
{
    insertElement("PPFnRemove", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnInsertBefore* op)
{
    insertElement("PPFnInsertBefore", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnZeroOrOne* op)
{
    insertElement("PPFnZeroOrOne", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnOneOrMore* op)
{
    insertElement("PPFnOneOrMore", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnExactlyOne* op)
{
    insertElement("PPFnExactlyOne", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnDateTimeFuncNoParam* op)
{
    insertElement("PPFnDateTimeFuncNoParam", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnDateTimeFunc* op)
{
    insertElement("PPFnDateTimeFunc", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnDateTimeFunc2Params* op)
{
    insertElement("PPFnDateTimeFunc2Params", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPADFilter* op)
{
    insertElement("PPADFilter", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDAFilter* op)
{
    insertElement("PPDAFilter", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFilterEL* op)
{
    insertElement("PPFilterEL", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCheckpoint* op)
{
    insertElement("PPCheckpoint", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPTest* op)
{
    insertElement("PPTest", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPConst* op)
{
    insertElement("PPConst", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDDO* op)
{
    insertElement("PPDDO", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPSXptr* op)
{
    insertElement("PPSXptr", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDocInCol* op)
{
    insertElement("PPDocInCol", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPExtFunCall* op)
{
    insertElement("PPExtFunCall", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnGetProperty* op)
{
    insertElement("PPFnGetProperty", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPIndexScan* op)
{
    insertElement("PPIndexScan", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPLast* op)
{
    insertElement("PPLast", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPNil* op)
{
    insertElement("PPNil", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPScan* op)
{
    insertElement("PPScan", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPSelect* op)
{
    insertElement("PPSelect", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPSeqChecker* op)
{
    insertElement("PPSeqChecker", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPStore* op)
{
    insertElement("PPStore", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPTuple* op)
{
    insertElement("PPTuple", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPUp* op)
{
    insertElement("PPUp", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPVarDecl* op)
{
    insertElement("PPVarDecl", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPVariable* op)
{
    insertElement("PPVariable", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPGlobalVariable* op)
{
    insertElement("PPGlobalVariable", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPXptr* op)
{
    insertElement("PPXptr", left, parent);
    visited = true;
}



#ifdef SE_ENABLE_DTSEARCH
void PPExplainVisitor::visit(PPFtHighlight* op)
{
    insertElement("PPFtHighlight", left, parent);
    visited = true;
}
void PPExplainVisitor::visit(PPFtScan* op)
{
    insertElement("PPFtScan", left, parent);
    visited = true;
}
#endif /* SE_ENABLE_DTSEARCH */



#ifdef SE_ENABLE_FTSEARCH
void PPExplainVisitor::visit(PPFtIndexScan* op)
{
    insertElement("PPFtIndexScan", left, parent);
    visited = true;
}
void PPExplainVisitor::visit(PPFtIndexScan2* op)
{
    insertElement("PPFtIndexScan2", left, parent);
    visited = true;
}
#endif /* SE_ENABLE_FTSEARCH */



void PPExplainVisitor::visit(PPFunCall* op)
{
    insertElement("PPFunCall", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPGeneralComparison* op)
{
    insertElement("PPGeneralComparison", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPLMGeneralComparison* op)
{
    insertElement("PPLMGeneralComparison", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPNEQGeneralComparison* op)
{
    insertElement("PPNEQGeneralComparison", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPEQLGeneralComparison* op)
{
    insertElement("PPEQLGeneralComparison", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPNodeComparison* op)
{
    insertElement("PPNodeComparison", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPIf* op)
{
    insertElement("PPIf", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPLet* op)
{
    insertElement("PPLet", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPOrderBy* op)
{
    insertElement("PPOrderBy", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPSTuple* op)
{
    insertElement("PPSTuple", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPSLet* op)
{
    insertElement("PPSLet", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPReturn* op)
{
    insertElement("PPReturn", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnName* op)
{
    insertElement("PPFnName", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnLocalName* op)
{
    insertElement("PPFnLocalName", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnNamespaceUri* op)
{
    insertElement("PPFnNamespaceUri", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnNumber* op)
{
    insertElement("PPFnNumber", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnRoot* op)
{
    insertElement("PPFnRoot", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPNumericFuncs* op)
{
    insertElement("PPNumericFuncs", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnRoundHalfToEven* op)
{
    insertElement("PPFnRoundHalfToEven", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPPatMatch* op)
{
    insertElement("PPPatMatch", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnResolveQName* op)
{
    insertElement("PPFnResolveQName", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnQName* op)
{
    insertElement("PPFnQName", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnPrefixFromQName* op)
{
    insertElement("PPFnPrefixFromQName", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnLocalNameFromQName* op)
{
    insertElement("PPFnLocalNameFromQName", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnNamespaceUriFromQName* op)
{
    insertElement("PPFnNamespaceUriFromQName", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnNamespaceUriForPrefix* op)
{
    insertElement("PPFnNamespaceUriForPrefix", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnInScopePrefixes* op)
{
    insertElement("PPFnInScopePrefixes", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCast* op)
{
    insertElement("PPCast", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCastable* op)
{
    insertElement("PPCastable", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPTreat* op)
{
    insertElement("PPTreat", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPTypeswitch* op)
{
    insertElement("PPTypeswitch", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPInstanceOf* op)
{
    insertElement("PPInstanceOf", left, parent);
    visited = true;
}



#ifdef SQL_CONNECTION
void PPExplainVisitor::visit(PPFnSQLConnect* op)
{
    insertElement("PPFnSQLConnect", left, parent);
    visited = true;
}
void PPExplainVisitor::visit(PPFnSQLExecute* op)
{
    insertElement("PPFnSQLExecute", left, parent);
    visited = true;
}
void PPExplainVisitor::visit(PPFnSQLPrepare* op)
{
    insertElement("PPFnSQLPrepare", left, parent);
    visited = true;
}
void PPExplainVisitor::visit(PPFnSQLClose* op)
{
    insertElement("PPFnSQLClose", left, parent);
    visited = true;
}
void PPExplainVisitor::visit(PPFnSQLCommit* op)
{
    insertElement("PPFnSQLCommit", left, parent);
    visited = true;
}
void PPExplainVisitor::visit(PPFnSQLRollback* op)
{
    insertElement("PPFnSQLRollback", left, parent);
    visited = true;
}
#endif /* SQL_CONNECTION */



void PPExplainVisitor::visit(PPFnConcat* op)
{
    insertElement("PPFnConcat", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnStringJoin* op)
{
    insertElement("PPFnStringJoin", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnStartsEndsWith* op)
{
    insertElement("PPFnStartsEndsWith", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnStringLength* op)
{
    insertElement("PPFnStringLength", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnNormalizeSpace* op)
{
    insertElement("PPFnNormalizeSpace", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnNormalizeUnicode* op)
{
    insertElement("PPFnNormalizeUnicode", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnString2CodePoints* op)
{
    insertElement("PPFnString2CodePoints", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnCodePoints2String* op)
{
    insertElement("PPFnCodePoints2String", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnTranslate* op)
{
    insertElement("PPFnTranslate", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnChangeCase* op)
{
    insertElement("PPFnChangeCase", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnSubsBeforeAfter* op)
{
    insertElement("PPFnSubsBeforeAfter", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnSubstring* op)
{
    insertElement("PPFnSubstring", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnCompare* op)
{
    insertElement("PPFnCompare", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPSubsMatch* op)
{
    insertElement("PPSubsMatch", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnUriEncoding* op)
{
    insertElement("PPFnUriEncoding", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPFnResolveUri* op)
{
    insertElement("PPFnResolveUri", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPQueryRoot* op)
{
    insertElement("PPQueryRoot", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPBulkLoad* op)
{
    insertElement("PPBulkLoad", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCreateFtIndex* op)
{
    insertElement("PPCreateFtIndex", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCreateIndex* op)
{
    insertElement("PPCreateIndex", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCreateDocument* op)
{
    insertElement("PPCreateDocument", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCreateCollection* op)
{
    insertElement("PPCreateCollection", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCreateDocumentInCollection* op)
{
    insertElement("PPCreateDocumentInCollection", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCreateTrigger* op)
{
    insertElement("PPCreateTrigger", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDeleteDeep* op)
{
    insertElement("PPDeleteDeep", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDeleteUndeep* op)
{
    insertElement("PPDeleteUndeep", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDropFtIndex* op)
{
    insertElement("PPDropFtIndex", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDropIndex* op)
{
    insertElement("PPDropIndex", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDropDocument* op)
{
    insertElement("PPDropDocument", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDropCollection* op)
{
    insertElement("PPDropCollection", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDropDocumentInCollection* op)
{
    insertElement("PPDropDocumentInCollection", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPLoadModule* op)
{
    insertElement("PPLoadModule", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDropModule* op)
{
    insertElement("PPDropModule", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDropTrigger* op)
{
    insertElement("PPDropTrigger", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPInsertTo* op)
{
    insertElement("PPInsertTo", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPInsertBefore* op)
{
    insertElement("PPInsertBefore", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPInsertFollowing* op)
{
    insertElement("PPInsertFollowing", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPRename* op)
{
    insertElement("PPRename", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPReplace* op)
{
    insertElement("PPReplace", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPRetrieveDS* op)
{
    insertElement("PPRetrieveDS", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPRetrieveMetadata* op)
{
    insertElement("PPRetrieveMetadata", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCreateUser* op)
{
    insertElement("PPCreateUser", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDropUser* op)
{
    insertElement("PPDropUser", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPAlterUser* op)
{
    insertElement("PPAlterUser", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPCreateRole* op)
{
    insertElement("PPCreateRole", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPDropRole* op)
{
    insertElement("PPDropRole", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPGrantRole* op)
{
    insertElement("PPGrantRole", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPGrantRevokePriv* op)
{
    insertElement("PPGrantRevokePriv", left, parent);
    visited = true;
}

void PPExplainVisitor::visit(PPRevokeRole* op)
{
    insertElement("PPRevokeRole", left, parent);
    visited = true;
}

