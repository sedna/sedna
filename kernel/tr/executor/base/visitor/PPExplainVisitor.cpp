/*
 * File:  PPExplainVisitor.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "common/base.h"
#include "common/u/uutils.h"

#include "tr/executor/base/visitor/PPExplainVisitor.h"
#include "tr/cat/catptr.h"
#include "tr/mo/mo.h"

static xmlns_ptr explain_ns = NULL_XMLNS;

PPExplainVisitor::PPExplainVisitor(dynamic_context* _cxt_,
                                   xptr _root_) : cxt(_cxt_), 
                                                  parent(_root_),
                                                  left(XNULL)
{
    explain_ns = cxt->st_cxt->get_xmlns_by_prefix(SEDNA_NAMESPACE_PREFIX);
}

PPExplainVisitor::~PPExplainVisitor() 
{
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
   

/* Helper to insert operation nodes */
void PPExplainVisitor::insertOperationElement(const char* name, xptr& left, xptr& parent, PPIterator* op = NULL)
{
    U_ASSERT(parent != XNULL);
    elog(EL_DBG, ("[EXPLAIN] Going to insert element '%s', parent (0x%x, 0x%x), left (0x%x, 0x%x)", name, 
                                                           parent.layer, parent.addr, 
                                                           left.layer, left.addr));
    left = insert_element_i(left,XNULL,parent,"operation",xs_untyped,explain_ns);
    xptr attr_left = insert_attribute_i(XNULL,XNULL,left,"name",xs_untypedAtomic, name, strlen(name), explain_ns);

    if(NULL != op) 
    {
        char buf[20];
        if(op->get_operation_info().query_line != 0)
        {
            u_itoa(op->get_operation_info().query_line,buf,10);
            attr_left = insert_attribute_i(attr_left,XNULL,left,"line",xs_untypedAtomic,buf,strlen(buf),explain_ns);
        }
        if(op->get_operation_info().query_col != 0)
        {
            u_itoa(op->get_operation_info().query_col,buf,10);
            attr_left = insert_attribute_i(attr_left,XNULL,left,"column",xs_untypedAtomic,buf,strlen(buf),explain_ns);
        }
    }
}


void PPExplainVisitor::visit(PPDmStringValue* op)
{
    insertOperationElement("PPDmStringValue", left, parent, op);
}

void PPExplainVisitor::visit(PPDmTypedValue* op)
{
    insertOperationElement("PPDmTypedValue", left, parent, op);
}

void PPExplainVisitor::visit(PPDmNodeKind* op)
{
    insertOperationElement("PPDmNodeKind", left, parent, op);
}

void PPExplainVisitor::visit(PPFnNodeName* op)
{
    insertOperationElement("PPFnNodeName", left, parent, op);
}

void PPExplainVisitor::visit(PPFnNilled* op)
{
    insertOperationElement("PPFnNilled", left, parent, op);
}

void PPExplainVisitor::visit(PPFnString* op)
{
    insertOperationElement("PPFnString", left, parent, op);
}

void PPExplainVisitor::visit(PPFnData* op)
{
    insertOperationElement("PPFnData", left, parent, op);
}

void PPExplainVisitor::visit(PPFnBaseURI* op)
{
    insertOperationElement("PPFnBaseURI", left, parent, op);
}

void PPExplainVisitor::visit(PPFnDocumentURI* op)
{
    insertOperationElement("PPFnDocumentURI", left, parent, op);
}

void PPExplainVisitor::visit(PPFnStaticBaseUri* op)
{
    insertOperationElement("PPFnStaticBaseUri", left, parent, op);
}

void PPExplainVisitor::visit(PPFnDefaultCollation* op)
{
    insertOperationElement("PPFnDefaultCollation", left, parent, op);
}

void PPExplainVisitor::visit(PPFnCount* op)
{
    insertOperationElement("PPFnCount", left, parent, op);
}

void PPExplainVisitor::visit(PPFnMaxMin* op)
{
    insertOperationElement("PPFnMaxMin", left, parent, op);
}

void PPExplainVisitor::visit(PPFnSumAvg* op)
{
    insertOperationElement("PPFnSumAvg", left, parent, op);
}

void PPExplainVisitor::visit(PPAbsPath* op)
{
    insertOperationElement("PPAbsPath", left, parent, op);
}

void PPExplainVisitor::visit(PPAxisAncestor* op)
{
    insertOperationElement("PPAxisAncestor", left, parent, op);
}

void PPExplainVisitor::visit(PPAxisAncestorOrSelf* op)
{
    insertOperationElement("PPAxisAncestorOrSelf", left, parent, op);
}

void PPExplainVisitor::visit(PPAxisAttribute* op)
{
    insertOperationElement("PPAxisAttribute", left, parent, op);
}

void PPExplainVisitor::visit(PPAxisChild* op)
{
    insertOperationElement("PPAxisChild", left, parent, op);
}

void PPExplainVisitor::visit(PPAxisDescendant* op)
{
    insertOperationElement("PPAxisDescendant", left, parent, op);
}

void PPExplainVisitor::visit(PPAxisDescendantOrSelf* op)
{
    insertOperationElement("PPAxisDescendantOrSelf", left, parent, op);
}

void PPExplainVisitor::visit(PPAxisDescendantAttr* op)
{
    insertOperationElement("PPAxisDescendantAttr", left, parent, op);
}

void PPExplainVisitor::visit(PPAxisFP* op)
{
    insertOperationElement("PPAxisFP", left, parent, op);
}

void PPExplainVisitor::visit(PPAxisParent* op)
{
    insertOperationElement("PPAxisParent", left, parent, op);
}

void PPExplainVisitor::visit(PPAxisSelf* op)
{
    insertOperationElement("PPAxisSelf", left, parent, op);
}

void PPExplainVisitor::visit(PPAxisSibling* op)
{
    insertOperationElement("PPAxisSibling", left, parent, op);
}

void PPExplainVisitor::visit(PPPred1* op)
{
    insertOperationElement("PPPred1", left, parent, op);
}

void PPExplainVisitor::visit(PPPred2* op)
{
    insertOperationElement("PPPred2", left, parent, op);
}

void PPExplainVisitor::visit(PPFnTrue* op)
{
    insertOperationElement("PPFnTrue", left, parent, op);
}

void PPExplainVisitor::visit(PPFnFalse* op)
{
    insertOperationElement("PPFnFalse", left, parent, op);
}

void PPExplainVisitor::visit(PPFnNot* op)
{
    insertOperationElement("PPFnNot", left, parent, op);
}

void PPExplainVisitor::visit(PPFnBoolean* op)
{
    insertOperationElement("PPFnBoolean", left, parent, op);
}

void PPExplainVisitor::visit(PPCalculate* op)
{
    insertOperationElement("PPCalculate", left, parent, op);
}

void PPExplainVisitor::visit(UnaryOp* op)
{
    insertOperationElement("UnaryOp", left, parent);
}

void PPExplainVisitor::visit(BinaryOp* op)
{
    insertOperationElement("BinaryOp", left, parent);
}

void PPExplainVisitor::visit(BinaryOpCollation* op)
{
    insertOperationElement("BinaryOpCollation", left, parent);
}

void PPExplainVisitor::visit(BinaryOpAnd* op)
{
    insertOperationElement("BinaryOpAnd", left, parent);
}

void PPExplainVisitor::visit(BinaryOpOr* op)
{
    insertOperationElement("BinaryOpOr", left, parent);
}

void PPExplainVisitor::visit(LeafAtomOp* op)
{
    insertOperationElement("LeafAtomOp", left, parent);
}

void PPExplainVisitor::visit(LeafEffectBoolOp* op)
{
    insertOperationElement("LeafEffectBoolOp", left, parent);
}

void PPExplainVisitor::visit(PPElementConstructor* op)
{
    insertOperationElement("PPElementConstructor", left, parent, op);
}

void PPExplainVisitor::visit(PPAttributeConstructor* op)
{
    insertOperationElement("PPAttributeConstructor", left, parent, op);
}

void PPExplainVisitor::visit(PPNamespaceConstructor* op)
{
    insertOperationElement("PPNamespaceConstructor", left, parent, op);
}

void PPExplainVisitor::visit(PPCommentConstructor* op)
{
    insertOperationElement("PPCommentConstructor", left, parent, op);
}

void PPExplainVisitor::visit(PPTextConstructor* op)
{
    insertOperationElement("PPTextConstructor", left, parent, op);
}

void PPExplainVisitor::visit(PPDocumentConstructor* op)
{
    insertOperationElement("PPDocumentConstructor", left, parent, op);
}

void PPExplainVisitor::visit(PPPIConstructor* op)
{
    insertOperationElement("PPPIConstructor", left, parent, op);
}

void PPExplainVisitor::visit(PPFnError* op)
{
    insertOperationElement("PPFnError", left, parent, op);
}

void PPExplainVisitor::visit(PPFnTrace* op)
{
    insertOperationElement("PPFnTrace", left, parent, op);
}

void PPExplainVisitor::visit(PPDebug* op)
{
    insertOperationElement("PPDebug", left, parent, op);
}

void PPExplainVisitor::visit(PPExcept* op)
{
    insertOperationElement("PPExcept", left, parent, op);
}

void PPExplainVisitor::visit(PPUnion* op)
{
    insertOperationElement("PPUnion", left, parent, op);
}

void PPExplainVisitor::visit(PPIntersect* op)
{
    insertOperationElement("PPIntersect", left, parent, op);
}

void PPExplainVisitor::visit(PPFnDeepEqual* op)
{
    insertOperationElement("PPFnDeepEqual", left, parent, op);
}

void PPExplainVisitor::visit(PPFnDocAvailable* op)
{
    insertOperationElement("PPFnDocAvailable", left, parent, op);
}

void PPExplainVisitor::visit(PPRange* op)
{
    insertOperationElement("PPRange", left, parent, op);
}

void PPExplainVisitor::visit(PPSequence* op)
{
    insertOperationElement("PPSequence", left, parent, op);
}

void PPExplainVisitor::visit(PPSpaceSequence* op)
{
    insertOperationElement("PPSpaceSequence", left, parent, op);
}

void PPExplainVisitor::visit(PPFnEmpty* op)
{
    insertOperationElement("PPFnEmpty", left, parent, op);
}

void PPExplainVisitor::visit(PPFnExists* op)
{
    insertOperationElement("PPFnExists", left, parent, op);
}

void PPExplainVisitor::visit(PPFnItemAt* op)
{
    insertOperationElement("PPFnItemAt", left, parent, op);
}

void PPExplainVisitor::visit(PPFnDistinctValues* op)
{
    insertOperationElement("PPFnDistinctValues", left, parent, op);
}

void PPExplainVisitor::visit(PPFnIndexOf* op)
{
    insertOperationElement("PPFnIndexOf", left, parent, op);
}

void PPExplainVisitor::visit(PPFnReverse* op)
{
    insertOperationElement("PPFnReverse", left, parent, op);
}

void PPExplainVisitor::visit(PPFnSubsequence* op)
{
    insertOperationElement("PPFnSubsequence", left, parent, op);
}

void PPExplainVisitor::visit(PPFnRemove* op)
{
    insertOperationElement("PPFnRemove", left, parent, op);
}

void PPExplainVisitor::visit(PPFnInsertBefore* op)
{
    insertOperationElement("PPFnInsertBefore", left, parent, op);
}

void PPExplainVisitor::visit(PPFnZeroOrOne* op)
{
    insertOperationElement("PPFnZeroOrOne", left, parent, op);
}

void PPExplainVisitor::visit(PPFnOneOrMore* op)
{
    insertOperationElement("PPFnOneOrMore", left, parent, op);
}

void PPExplainVisitor::visit(PPFnExactlyOne* op)
{
    insertOperationElement("PPFnExactlyOne", left, parent, op);
}

void PPExplainVisitor::visit(PPFnDateTimeFuncNoParam* op)
{
    insertOperationElement("PPFnDateTimeFuncNoParam", left, parent, op);
}

void PPExplainVisitor::visit(PPFnDateTimeFunc* op)
{
    insertOperationElement("PPFnDateTimeFunc", left, parent, op);
}

void PPExplainVisitor::visit(PPFnDateTimeFunc2Params* op)
{
    insertOperationElement("PPFnDateTimeFunc2Params", left, parent, op);
}

void PPExplainVisitor::visit(PPADFilter* op)
{
    insertOperationElement("PPADFilter", left, parent, op);
}

void PPExplainVisitor::visit(PPDAFilter* op)
{
    insertOperationElement("PPDAFilter", left, parent, op);
}

void PPExplainVisitor::visit(PPFilterEL* op)
{
    insertOperationElement("PPFilterEL", left, parent, op);
}

void PPExplainVisitor::visit(PPCheckpoint* op)
{
    insertOperationElement("PPCheckpoint", left, parent, op);
}

void PPExplainVisitor::visit(PPTest* op)
{
    insertOperationElement("PPTest", left, parent, op);
}

void PPExplainVisitor::visit(PPConst* op)
{
    insertOperationElement("PPConst", left, parent, op);
}

void PPExplainVisitor::visit(PPDDO* op)
{
    insertOperationElement("PPDDO", left, parent, op);
}

void PPExplainVisitor::visit(PPSXptr* op)
{
    insertOperationElement("PPSXptr", left, parent, op);
}

void PPExplainVisitor::visit(PPDocInCol* op)
{
    insertOperationElement("PPDocInCol", left, parent, op);
}

void PPExplainVisitor::visit(PPExtFunCall* op)
{
    insertOperationElement("PPExtFunCall", left, parent, op);
}

void PPExplainVisitor::visit(PPFnGetProperty* op)
{
    insertOperationElement("PPFnGetProperty", left, parent, op);
}

void PPExplainVisitor::visit(PPIndexScan* op)
{
    insertOperationElement("PPIndexScan", left, parent, op);
}

void PPExplainVisitor::visit(PPLast* op)
{
    insertOperationElement("PPLast", left, parent, op);
}

void PPExplainVisitor::visit(PPNil* op)
{
    insertOperationElement("PPNil", left, parent, op);
}

void PPExplainVisitor::visit(PPScan* op)
{
    insertOperationElement("PPScan", left, parent, op);
}

void PPExplainVisitor::visit(PPSelect* op)
{
    insertOperationElement("PPSelect", left, parent, op);
}

void PPExplainVisitor::visit(PPSeqChecker* op)
{
    insertOperationElement("PPSeqChecker", left, parent, op);
}

void PPExplainVisitor::visit(PPStore* op)
{
    insertOperationElement("PPStore", left, parent, op);
}

void PPExplainVisitor::visit(PPTuple* op)
{
    insertOperationElement("PPTuple", left, parent, op);
}

void PPExplainVisitor::visit(PPUp* op)
{
    insertOperationElement("PPUp", left, parent, op);
}

void PPExplainVisitor::visit(PPVarDecl* op)
{
    insertOperationElement("PPVarDecl", left, parent, op);
}

void PPExplainVisitor::visit(PPVariable* op)
{
    insertOperationElement("PPVariable", left, parent, op);
}

void PPExplainVisitor::visit(PPGlobalVariable* op)
{
    insertOperationElement("PPGlobalVariable", left, parent, op);
}

void PPExplainVisitor::visit(PPXptr* op)
{
    insertOperationElement("PPXptr", left, parent, op);
}



#ifdef SE_ENABLE_DTSEARCH
void PPExplainVisitor::visit(PPFtHighlight* op)
{
    insertOperationElement("PPFtHighlight", left, parent, op);
}
void PPExplainVisitor::visit(PPFtScan* op)
{
    insertOperationElement("PPFtScan", left, parent, op);
}
#endif /* SE_ENABLE_DTSEARCH */



#ifdef SE_ENABLE_FTSEARCH
void PPExplainVisitor::visit(PPFtIndexScan* op)
{
    insertOperationElement("PPFtIndexScan", left, parent, op);
}
void PPExplainVisitor::visit(PPFtIndexScan2* op)
{
    insertOperationElement("PPFtIndexScan2", left, parent, op);
}
#endif /* SE_ENABLE_FTSEARCH */



void PPExplainVisitor::visit(PPFunCall* op)
{
    insertOperationElement("PPFunCall", left, parent, op);
}

void PPExplainVisitor::visit(PPGeneralComparison* op)
{
    insertOperationElement("PPGeneralComparison", left, parent, op);
}

void PPExplainVisitor::visit(PPLMGeneralComparison* op)
{
    insertOperationElement("PPLMGeneralComparison", left, parent, op);
}

void PPExplainVisitor::visit(PPNEQGeneralComparison* op)
{
    insertOperationElement("PPNEQGeneralComparison", left, parent, op);
}

void PPExplainVisitor::visit(PPEQLGeneralComparison* op)
{
    insertOperationElement("PPEQLGeneralComparison", left, parent, op);
}

void PPExplainVisitor::visit(PPNodeComparison* op)
{
    insertOperationElement("PPNodeComparison", left, parent, op);
}

void PPExplainVisitor::visit(PPIf* op)
{
    insertOperationElement("PPIf", left, parent, op);
}

void PPExplainVisitor::visit(PPLet* op)
{
    insertOperationElement("PPLet", left, parent, op);
}

void PPExplainVisitor::visit(PPOrderBy* op)
{
    insertOperationElement("PPOrderBy", left, parent, op);
}

void PPExplainVisitor::visit(PPSTuple* op)
{
    insertOperationElement("PPSTuple", left, parent, op);
}

void PPExplainVisitor::visit(PPSLet* op)
{
    insertOperationElement("PPSLet", left, parent, op);
}

void PPExplainVisitor::visit(PPReturn* op)
{
    insertOperationElement("PPReturn", left, parent, op);
}

void PPExplainVisitor::visit(PPFnName* op)
{
    insertOperationElement("PPFnName", left, parent, op);
}

void PPExplainVisitor::visit(PPFnLocalName* op)
{
    insertOperationElement("PPFnLocalName", left, parent, op);
}

void PPExplainVisitor::visit(PPFnNamespaceUri* op)
{
    insertOperationElement("PPFnNamespaceUri", left, parent, op);
}

void PPExplainVisitor::visit(PPFnNumber* op)
{
    insertOperationElement("PPFnNumber", left, parent, op);
}

void PPExplainVisitor::visit(PPFnRoot* op)
{
    insertOperationElement("PPFnRoot", left, parent, op);
}

void PPExplainVisitor::visit(PPNumericFuncs* op)
{
    insertOperationElement("PPNumericFuncs", left, parent, op);
}

void PPExplainVisitor::visit(PPFnRoundHalfToEven* op)
{
    insertOperationElement("PPFnRoundHalfToEven", left, parent, op);
}

void PPExplainVisitor::visit(PPPatMatch* op)
{
    insertOperationElement("PPPatMatch", left, parent, op);
}

void PPExplainVisitor::visit(PPFnResolveQName* op)
{
    insertOperationElement("PPFnResolveQName", left, parent, op);
}

void PPExplainVisitor::visit(PPFnQName* op)
{
    insertOperationElement("PPFnQName", left, parent, op);
}

void PPExplainVisitor::visit(PPFnPrefixFromQName* op)
{
    insertOperationElement("PPFnPrefixFromQName", left, parent, op);
}

void PPExplainVisitor::visit(PPFnLocalNameFromQName* op)
{
    insertOperationElement("PPFnLocalNameFromQName", left, parent, op);
}

void PPExplainVisitor::visit(PPFnNamespaceUriFromQName* op)
{
    insertOperationElement("PPFnNamespaceUriFromQName", left, parent, op);
}

void PPExplainVisitor::visit(PPFnNamespaceUriForPrefix* op)
{
    insertOperationElement("PPFnNamespaceUriForPrefix", left, parent, op);
}

void PPExplainVisitor::visit(PPFnInScopePrefixes* op)
{
    insertOperationElement("PPFnInScopePrefixes", left, parent, op);
}

void PPExplainVisitor::visit(PPCast* op)
{
    insertOperationElement("PPCast", left, parent, op);
}

void PPExplainVisitor::visit(PPCastable* op)
{
    insertOperationElement("PPCastable", left, parent, op);
}

void PPExplainVisitor::visit(PPTreat* op)
{
    insertOperationElement("PPTreat", left, parent, op);
}

void PPExplainVisitor::visit(PPTypeswitch* op)
{
    insertOperationElement("PPTypeswitch", left, parent, op);
}

void PPExplainVisitor::visit(PPInstanceOf* op)
{
    insertOperationElement("PPInstanceOf", left, parent, op);
}



#ifdef SQL_CONNECTION
void PPExplainVisitor::visit(PPFnSQLConnect* op)
{
    insertOperationElement("PPFnSQLConnect", left, parent, op);
}
void PPExplainVisitor::visit(PPFnSQLExecute* op)
{
    insertOperationElement("PPFnSQLExecute", left, parent, op);
}
void PPExplainVisitor::visit(PPFnSQLPrepare* op)
{
    insertOperationElement("PPFnSQLPrepare", left, parent, op);
}
void PPExplainVisitor::visit(PPFnSQLClose* op)
{
    insertOperationElement("PPFnSQLClose", left, parent, op);
}
void PPExplainVisitor::visit(PPFnSQLCommit* op)
{
    insertOperationElement("PPFnSQLCommit", left, parent, op);
}
void PPExplainVisitor::visit(PPFnSQLRollback* op)
{
    insertOperationElement("PPFnSQLRollback", left, parent, op);
}
#endif /* SQL_CONNECTION */



void PPExplainVisitor::visit(PPFnConcat* op)
{
    insertOperationElement("PPFnConcat", left, parent, op);
}

void PPExplainVisitor::visit(PPFnStringJoin* op)
{
    insertOperationElement("PPFnStringJoin", left, parent, op);
}

void PPExplainVisitor::visit(PPFnStartsEndsWith* op)
{
    insertOperationElement("PPFnStartsEndsWith", left, parent, op);
}

void PPExplainVisitor::visit(PPFnStringLength* op)
{
    insertOperationElement("PPFnStringLength", left, parent, op);
}

void PPExplainVisitor::visit(PPFnNormalizeSpace* op)
{
    insertOperationElement("PPFnNormalizeSpace", left, parent, op);
}

void PPExplainVisitor::visit(PPFnNormalizeUnicode* op)
{
    insertOperationElement("PPFnNormalizeUnicode", left, parent, op);
}

void PPExplainVisitor::visit(PPFnString2CodePoints* op)
{
    insertOperationElement("PPFnString2CodePoints", left, parent, op);
}

void PPExplainVisitor::visit(PPFnCodePoints2String* op)
{
    insertOperationElement("PPFnCodePoints2String", left, parent, op);
}

void PPExplainVisitor::visit(PPFnTranslate* op)
{
    insertOperationElement("PPFnTranslate", left, parent, op);
}

void PPExplainVisitor::visit(PPFnChangeCase* op)
{
    insertOperationElement("PPFnChangeCase", left, parent, op);
}

void PPExplainVisitor::visit(PPFnSubsBeforeAfter* op)
{
    insertOperationElement("PPFnSubsBeforeAfter", left, parent, op);
}

void PPExplainVisitor::visit(PPFnSubstring* op)
{
    insertOperationElement("PPFnSubstring", left, parent, op);
}

void PPExplainVisitor::visit(PPFnCompare* op)
{
    insertOperationElement("PPFnCompare", left, parent, op);
}

void PPExplainVisitor::visit(PPSubsMatch* op)
{
    insertOperationElement("PPSubsMatch", left, parent, op);
}

void PPExplainVisitor::visit(PPFnUriEncoding* op)
{
    insertOperationElement("PPFnUriEncoding", left, parent, op);
}

void PPExplainVisitor::visit(PPFnResolveUri* op)
{
    insertOperationElement("PPFnResolveUri", left, parent, op);
}

void PPExplainVisitor::visit(PPQueryRoot* op)
{
    insertOperationElement("PPQueryRoot", left, parent);
}

void PPExplainVisitor::visit(PPBulkLoad* op)
{
    insertOperationElement("PPBulkLoad", left, parent);
}

void PPExplainVisitor::visit(PPCreateIndex* op)
{
    insertOperationElement("PPCreateIndex", left, parent);
}

void PPExplainVisitor::visit(PPCreateDocument* op)
{
    insertOperationElement("PPCreateDocument", left, parent);
}

void PPExplainVisitor::visit(PPCreateCollection* op)
{
    insertOperationElement("PPCreateCollection", left, parent);
}

void PPExplainVisitor::visit(PPCreateDocumentInCollection* op)
{
    insertOperationElement("PPCreateDocumentInCollection", left, parent);
}

#ifdef SE_ENABLE_TRIGGERS
void PPExplainVisitor::visit(PPCreateTrigger* op)
{
    insertOperationElement("PPCreateTrigger", left, parent);
}
void PPExplainVisitor::visit(PPDropTrigger* op)
{
    insertOperationElement("PPDropTrigger", left, parent);
}
#endif


void PPExplainVisitor::visit(PPDeleteDeep* op)
{
    insertOperationElement("PPDeleteDeep", left, parent);
}

void PPExplainVisitor::visit(PPDeleteUndeep* op)
{
    insertOperationElement("PPDeleteUndeep", left, parent);
}


#ifdef SE_ENABLE_FTSEARCH
void PPExplainVisitor::visit(PPCreateFtIndex* op)
{
    insertOperationElement("PPCreateFtIndex", left, parent);
}
void PPExplainVisitor::visit(PPDropFtIndex* op)
{
    insertOperationElement("PPDropFtIndex", left, parent);
}
#endif


void PPExplainVisitor::visit(PPDropIndex* op)
{
    insertOperationElement("PPDropIndex", left, parent);
}

void PPExplainVisitor::visit(PPDropDocument* op)
{
    insertOperationElement("PPDropDocument", left, parent);
}

void PPExplainVisitor::visit(PPDropCollection* op)
{
    insertOperationElement("PPDropCollection", left, parent);
}

void PPExplainVisitor::visit(PPDropDocumentInCollection* op)
{
    insertOperationElement("PPDropDocumentInCollection", left, parent);
}

void PPExplainVisitor::visit(PPLoadModule* op)
{
    insertOperationElement("PPLoadModule", left, parent);
}

void PPExplainVisitor::visit(PPDropModule* op)
{
    insertOperationElement("PPDropModule", left, parent);
}

void PPExplainVisitor::visit(PPInsertTo* op)
{
    insertOperationElement("PPInsertTo", left, parent);
}

void PPExplainVisitor::visit(PPInsertBefore* op)
{
    insertOperationElement("PPInsertBefore", left, parent);
}

void PPExplainVisitor::visit(PPInsertFollowing* op)
{
    insertOperationElement("PPInsertFollowing", left, parent);
}

void PPExplainVisitor::visit(PPRename* op)
{
    insertOperationElement("PPRename", left, parent);
}

void PPExplainVisitor::visit(PPReplace* op)
{
    insertOperationElement("PPReplace", left, parent);
}

void PPExplainVisitor::visit(PPRetrieveDS* op)
{
    insertOperationElement("PPRetrieveDS", left, parent);
}

void PPExplainVisitor::visit(PPRetrieveMetadata* op)
{
    insertOperationElement("PPRetrieveMetadata", left, parent);
}

void PPExplainVisitor::visit(PPCreateUser* op)
{
    insertOperationElement("PPCreateUser", left, parent);
}

void PPExplainVisitor::visit(PPDropUser* op)
{
    insertOperationElement("PPDropUser", left, parent);
}

void PPExplainVisitor::visit(PPAlterUser* op)
{
    insertOperationElement("PPAlterUser", left, parent);
}

void PPExplainVisitor::visit(PPCreateRole* op)
{
    insertOperationElement("PPCreateRole", left, parent);
}

void PPExplainVisitor::visit(PPDropRole* op)
{
    insertOperationElement("PPDropRole", left, parent);
}

void PPExplainVisitor::visit(PPGrantRole* op)
{
    insertOperationElement("PPGrantRole", left, parent);
}

void PPExplainVisitor::visit(PPGrantRevokePriv* op)
{
    insertOperationElement("PPGrantRevokePriv", left, parent);
}

void PPExplainVisitor::visit(PPRevokeRole* op)
{
    insertOperationElement("PPRevokeRole", left, parent);
}

