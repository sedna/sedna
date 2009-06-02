/*
 * File:  AST.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_COMMON_
#define _AST_COMMON_

#include <string>
#include <vector>

class ASTVisitor;
class ASTNode;
//#include "tr/xqp/ast/ASTNode.h"

/* vector to store sequence of AST nodes */
typedef std::vector<ASTNode *> ASTNodesVector;

/* vector to store sequence of std::string nodes */
typedef std::vector<std::string *> ASTStringVector;

// Parses QName to prefix and local parts and stores them in pref and loc strings
// NOTE: function doesn't check corectness, if qname is 'pref:loc' then it is straightforward, else it is pref="", loc="qname"
//
// Parameters:
//      qname -- QName to parse
//      pref(ret) -- string to store prefix (or "")
//      loc(ret) -- string to store local part
//
// Returns:
//      pref and loc
void ASTParseQName(const std::string *qname, std::string **pref, std::string **loc);

/* helper to destroy vector of nodes and corresponding elements */
void destroyASTNodesVector(ASTNodesVector *nodes);

/* helper to destroy vector of strings and corresponding elements */
void destroyASTStringVector(ASTStringVector *strs);

/* duplicates nodes vector and its content */
ASTNodesVector *duplicateASTNodes(ASTNodesVector *nodes);

/* duplicates string vector and its content */
ASTStringVector *duplicateASTStringVector(ASTStringVector *strs);

#include "tr/xqp/ast/ASTAlterUser.h"
#include "tr/xqp/ast/ASTAtomicTest.h"
#include "tr/xqp/ast/ASTAttr.h"
#include "tr/xqp/ast/ASTAttrConst.h"
#include "tr/xqp/ast/ASTAttribTest.h"
#include "tr/xqp/ast/ASTAxis.h"
#include "tr/xqp/ast/ASTAxisStep.h"
#include "tr/xqp/ast/ASTBaseURI.h"
#include "tr/xqp/ast/ASTBop.h"
#include "tr/xqp/ast/ASTBoundSpaceDecl.h"
#include "tr/xqp/ast/ASTCase.h"
#include "tr/xqp/ast/ASTCast.h"
#include "tr/xqp/ast/ASTCastable.h"
#include "tr/xqp/ast/ASTCharCont.h"
#include "tr/xqp/ast/ASTCommentConst.h"
#include "tr/xqp/ast/ASTCommTest.h"
#include "tr/xqp/ast/ASTConstDecl.h"
#include "tr/xqp/ast/ASTCreateColl.h"
#include "tr/xqp/ast/ASTCreateDoc.h"
#include "tr/xqp/ast/ASTCreateFtIndex.h"
#include "tr/xqp/ast/ASTCreateIndex.h"
#include "tr/xqp/ast/ASTCreateRole.h"
#include "tr/xqp/ast/ASTCreateTrg.h"
#include "tr/xqp/ast/ASTCreateUser.h"
#include "tr/xqp/ast/ASTDDO.h"
#include "tr/xqp/ast/ASTDeclareCopyNsp.h"
#include "tr/xqp/ast/ASTDefCollation.h"
#include "tr/xqp/ast/ASTDefNamespaceDecl.h"
#include "tr/xqp/ast/ASTDocConst.h"
#include "tr/xqp/ast/ASTDocTest.h"
#include "tr/xqp/ast/ASTDropColl.h"
#include "tr/xqp/ast/ASTDropDoc.h"
#include "tr/xqp/ast/ASTDropFtIndex.h"
#include "tr/xqp/ast/ASTDropIndex.h"
#include "tr/xqp/ast/ASTDropMod.h"
#include "tr/xqp/ast/ASTDropRole.h"
#include "tr/xqp/ast/ASTDropTrg.h"
#include "tr/xqp/ast/ASTDropUser.h"
#include "tr/xqp/ast/ASTElem.h"
#include "tr/xqp/ast/ASTElemConst.h"
#include "tr/xqp/ast/ASTElementTest.h"
#include "tr/xqp/ast/ASTEmptyTest.h"
#include "tr/xqp/ast/ASTError.h"
#include "tr/xqp/ast/ASTExtExpr.h"
#include "tr/xqp/ast/ASTFilterStep.h"
#include "tr/xqp/ast/ASTFor.h"
#include "tr/xqp/ast/ASTFunCall.h"
#include "tr/xqp/ast/ASTFuncDecl.h"
#include "tr/xqp/ast/ASTFunDef.h"
#include "tr/xqp/ast/ASTGrantPriv.h"
#include "tr/xqp/ast/ASTGrantRole.h"
#include "tr/xqp/ast/ASTIf.h"
#include "tr/xqp/ast/ASTInstOf.h"
#include "tr/xqp/ast/ASTItemTest.h"
#include "tr/xqp/ast/ASTLet.h"
#include "tr/xqp/ast/ASTLibModule.h"
#include "tr/xqp/ast/ASTLit.h"
#include "tr/xqp/ast/ASTLoadFile.h"
#include "tr/xqp/ast/ASTLoadModule.h"
#include "tr/xqp/ast/ASTMainModule.h"
#include "tr/xqp/ast/ASTMetaCols.h"
#include "tr/xqp/ast/ASTMetaDocs.h"
#include "tr/xqp/ast/ASTMetaSchemaDoc.h"
#include "tr/xqp/ast/ASTMetaSchemaCol.h"
#include "tr/xqp/ast/ASTModImport.h"
#include "tr/xqp/ast/ASTModuleDecl.h"
#include "tr/xqp/ast/ASTNamespaceDecl.h"
#include "tr/xqp/ast/ASTNameTest.h"
#include "tr/xqp/ast/ASTNodeTest.h"
#include "tr/xqp/ast/ASTNsp.h"
#include "tr/xqp/ast/ASTOption.h"
#include "tr/xqp/ast/ASTOrdExpr.h"
#include "tr/xqp/ast/ASTOrder.h"
#include "tr/xqp/ast/ASTOrderBy.h"
#include "tr/xqp/ast/ASTOrderByRet.h"
#include "tr/xqp/ast/ASTOrderEmpty.h"
#include "tr/xqp/ast/ASTOrderMod.h"
#include "tr/xqp/ast/ASTOrderModInt.h"
#include "tr/xqp/ast/ASTOrderSpec.h"
#include "tr/xqp/ast/ASTPi.h"
#include "tr/xqp/ast/ASTPiTest.h"
#include "tr/xqp/ast/ASTPIConst.h"
#include "tr/xqp/ast/ASTPosVar.h"
#include "tr/xqp/ast/ASTPragma.h"
#include "tr/xqp/ast/ASTPred.h"
#include "tr/xqp/ast/ASTProlog.h"
#include "tr/xqp/ast/ASTQuantExpr.h"
#include "tr/xqp/ast/ASTQuery.h"
#include "tr/xqp/ast/ASTRenameColl.h"
#include "tr/xqp/ast/ASTRet.h"
#include "tr/xqp/ast/ASTRevokePriv.h"
#include "tr/xqp/ast/ASTRevokeRole.h"
#include "tr/xqp/ast/ASTSchemaAttrTest.h"
#include "tr/xqp/ast/ASTSchemaElemTest.h"
#include "tr/xqp/ast/ASTSeq.h"
#include "tr/xqp/ast/ASTScript.h"
#include "tr/xqp/ast/ASTSpaceSeq.h"
#include "tr/xqp/ast/ASTTextConst.h"
#include "tr/xqp/ast/ASTTextTest.h"
#include "tr/xqp/ast/ASTTreat.h"
#include "tr/xqp/ast/ASTTypeSeq.h"
#include "tr/xqp/ast/ASTTypeSingle.h"
#include "tr/xqp/ast/ASTTypeSwitch.h"
#include "tr/xqp/ast/ASTTypeVar.h"
#include "tr/xqp/ast/ASTUop.h"
#include "tr/xqp/ast/ASTUnio.h"
#include "tr/xqp/ast/ASTUpdInsert.h"
#include "tr/xqp/ast/ASTUpdDel.h"
#include "tr/xqp/ast/ASTUpdMove.h"
#include "tr/xqp/ast/ASTUpdReplace.h"
#include "tr/xqp/ast/ASTUpdRename.h"
#include "tr/xqp/ast/ASTVar.h"
#include "tr/xqp/ast/ASTVarDecl.h"
#include "tr/xqp/ast/ASTVersionDecl.h"
#include "tr/xqp/ast/ASTXMLComm.h"

#endif
