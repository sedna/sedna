/*
 * File:  ASTVisitor.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/ast/AST.h"

class ASTVisitor
{
public:
    virtual ~ASTVisitor() {}

    void VisitNodesVector(ASTNodesVector *nodes, ASTVisitor &v);

    virtual void visit(const ASTAlterUser &n) = 0;
    virtual void visit(const ASTAtomicTest &n) = 0;
    virtual void visit(const ASTAttrConst &n) = 0;
    virtual void visit(const ASTAttribTest &n) = 0;
    virtual void visit(const ASTAttr &n) = 0;
    virtual void visit(const ASTAxis &n) = 0;
    virtual void visit(const ASTAxisStep &n) = 0;
    virtual void visit(const ASTBaseURI &n) = 0;
    virtual void visit(const ASTBop &n) = 0;
    virtual void visit(const ASTBoundSpaceDecl &n) = 0;
    virtual void visit(const ASTCase &n) = 0;
    virtual void visit(const ASTCastable &n) = 0;
    virtual void visit(const ASTCast &n) = 0;
    virtual void visit(const ASTCharCont &n) = 0;
    virtual void visit(const ASTCommentConst &n) = 0;
    virtual void visit(const ASTCommTest &n) = 0;
    virtual void visit(const ASTConstDecl &n) = 0;
    virtual void visit(const ASTCreateColl &n) = 0;
    virtual void visit(const ASTCreateDoc &n) = 0;
    virtual void visit(const ASTCreateFtIndex &n) = 0;
    virtual void visit(const ASTCreateIndex &n) = 0;
    virtual void visit(const ASTCreateRole &n) = 0;
    virtual void visit(const ASTCreateTrg &n) = 0;
    virtual void visit(const ASTCreateUser &n) = 0;
    virtual void visit(const ASTDDO &n) = 0;
    virtual void visit(const ASTDeclareCopyNsp &n) = 0;
    virtual void visit(const ASTDefCollation &n) = 0;
    virtual void visit(const ASTDefNamespaceDecl &n) = 0;
    virtual void visit(const ASTDocConst &n) = 0;
    virtual void visit(const ASTDocTest &n) = 0;
    virtual void visit(const ASTDropColl &n) = 0;
    virtual void visit(const ASTDropDoc &n) = 0;
    virtual void visit(const ASTDropFtIndex &n) = 0;
    virtual void visit(const ASTDropIndex &n) = 0;
    virtual void visit(const ASTDropMod &n) = 0;
    virtual void visit(const ASTDropRole &n) = 0;
    virtual void visit(const ASTDropTrg &n) = 0;
    virtual void visit(const ASTDropUser &n) = 0;
    virtual void visit(const ASTElemConst &n) = 0;
    virtual void visit(const ASTElementTest &n) = 0;
    virtual void visit(const ASTElem &n) = 0;
    virtual void visit(const ASTEmptyTest &n) = 0;
    virtual void visit(const ASTExtExpr &n) = 0;
    virtual void visit(const ASTError &n) = 0;
    virtual void visit(const ASTFilterStep &n) = 0;
    virtual void visit(const ASTFor &n) = 0;
    virtual void visit(const ASTFunCall &n) = 0;
    virtual void visit(const ASTFuncDecl &n) = 0;
    virtual void visit(const ASTFunDef &n) = 0;
    virtual void visit(const ASTGrantPriv &n) = 0;
    virtual void visit(const ASTGrantRole &n) = 0;
    virtual void visit(const ASTIf &n) = 0;
    virtual void visit(const ASTInstOf &n) = 0;
    virtual void visit(const ASTItemTest &n) = 0;
    virtual void visit(const ASTLet &n) = 0;
    virtual void visit(const ASTLibModule &n) = 0;
    virtual void visit(const ASTLit &n) = 0;
    virtual void visit(const ASTLoadFile &n) = 0;
    virtual void visit(const ASTLoadModule &n) = 0;
    virtual void visit(const ASTMainModule &n) = 0;
    virtual void visit(const ASTMetaCols &n) = 0;
    virtual void visit(const ASTMetaDocs &n) = 0;
    virtual void visit(const ASTMetaSchemaCol &n) = 0;
    virtual void visit(const ASTMetaSchemaDoc &n) = 0;
    virtual void visit(const ASTModImport &n) = 0;
    virtual void visit(const ASTModuleDecl &n) = 0;
    virtual void visit(const ASTNamespaceDecl &n) = 0;
    virtual void visit(const ASTNameTest &n) = 0;
    virtual void visit(const ASTNodeTest &n) = 0;
    virtual void visit(const ASTNsp &n) = 0;
    virtual void visit(const ASTOption &n) = 0;
    virtual void visit(const ASTOrderBy &n) = 0;
    virtual void visit(const ASTOrderByRet &n) = 0;
    virtual void visit(const ASTOrderEmpty &n) = 0;
    virtual void visit(const ASTOrderModInt &n) = 0;
    virtual void visit(const ASTOrderMod &n) = 0;
    virtual void visit(const ASTOrder &n) = 0;
    virtual void visit(const ASTOrderSpec &n) = 0;
    virtual void visit(const ASTOrdExpr &n) = 0;
    virtual void visit(const ASTPIConst &n) = 0;
    virtual void visit(const ASTPi &n) = 0;
    virtual void visit(const ASTPiTest &n) = 0;
    virtual void visit(const ASTPosVar &n) = 0;
    virtual void visit(const ASTPragma &n) = 0;
    virtual void visit(const ASTPred &n) = 0;
    virtual void visit(const ASTProlog &n) = 0;
    virtual void visit(const ASTQuantExpr &n) = 0;
    virtual void visit(const ASTQuery &n) = 0;
    virtual void visit(const ASTRenameColl &n) = 0;
    virtual void visit(const ASTRet &n) = 0;
    virtual void visit(const ASTRevokePriv &n) = 0;
    virtual void visit(const ASTRevokeRole &n) = 0;
    virtual void visit(const ASTSchemaAttrTest &n) = 0;
    virtual void visit(const ASTSchemaElemTest &n) = 0;
    virtual void visit(const ASTScript &n) = 0;
    virtual void visit(const ASTSeq &n) = 0;
    virtual void visit(const ASTSpaceSeq &n) = 0;
    virtual void visit(const ASTTextConst &n) = 0;
    virtual void visit(const ASTTextTest &n) = 0;
    virtual void visit(const ASTTreat &n) = 0;
    virtual void visit(const ASTTypeSeq &n) = 0;
    virtual void visit(const ASTTypeSingle &n) = 0;
    virtual void visit(const ASTTypeSwitch &n) = 0;
    virtual void visit(const ASTTypeVar &n) = 0;
    virtual void visit(const ASTUnio &n) = 0;
    virtual void visit(const ASTUop &n) = 0;
    virtual void visit(const ASTUpdDel &n) = 0;
    virtual void visit(const ASTUpdInsert &n) = 0;
    virtual void visit(const ASTUpdMove &n) = 0;
    virtual void visit(const ASTUpdRename &n) = 0;
    virtual void visit(const ASTUpdReplace &n) = 0;
    virtual void visit(const ASTVarDecl &n) = 0;
    virtual void visit(const ASTVar &n) = 0;
    virtual void visit(const ASTVersionDecl &n) = 0;
    virtual void visit(const ASTXMLComm &n) = 0;
};
