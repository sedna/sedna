/*
 * File:  ASTVisitor.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_VISITOR_H_
#define _AST_VISITOR_H_

#include "tr/xqp/ast/AST.h"

namespace sedna {
    class XQueryDriver;
}

class ASTVisitor
{
protected:
    sedna::XQueryDriver *drv;
    sedna::XQueryModule *mod;

    ASTNodesVector vis_path;
    ASTNodesVector garbNodes;

protected:
    void modifyParent(ASTNode *newc, bool toAccept, bool toGarbage);
    void putToGarbage(ASTNode *nod);
    void freeGarbage();

    ASTNode *getParent();

    // some AST related common functions
    bool isVarSequence(ASTTypeVar *var); // is variable bound to a 0+ sequence

public:
    ASTVisitor(sedna::XQueryDriver *drv_, sedna::XQueryModule *mod_) : drv(drv_), mod(mod_) {}
    virtual ~ASTVisitor()
    {
        freeGarbage();
    }

    void VisitNodesVector(ASTNodesVector *nodes, ASTVisitor &v);

    virtual void addToPath(ASTNode *nod);
    virtual void removeFromPath(ASTNode *nod);

    virtual void visit(ASTAlterUser &n) = 0;
    virtual void visit(ASTAttr&n) = 0;
    virtual void visit(ASTAttribTest &n) = 0;
    virtual void visit(ASTAttrConst &n) = 0;
    virtual void visit(ASTAxisStep &n) = 0;
    virtual void visit(ASTBaseURI &n) = 0;
    virtual void visit(ASTBop &n) = 0;
    virtual void visit(ASTBoundSpaceDecl &n) = 0;
    virtual void visit(ASTCase &n) = 0;
    virtual void visit(ASTCastable &n) = 0;
    virtual void visit(ASTCast &n) = 0;
    virtual void visit(ASTCharCont &n) = 0;
    virtual void visit(ASTCommentConst &n) = 0;
    virtual void visit(ASTCommTest &n) = 0;
    virtual void visit(ASTConstDecl &n) = 0;
    virtual void visit(ASTCreateColl &n) = 0;
    virtual void visit(ASTCreateDoc &n) = 0;
    virtual void visit(ASTCreateFtIndex &n) = 0;
    virtual void visit(ASTCreateIndex &n) = 0;
    virtual void visit(ASTCreateRole &n) = 0;
    virtual void visit(ASTCreateTrg &n) = 0;
    virtual void visit(ASTCreateUser &n) = 0;
    virtual void visit(ASTDDO &n) = 0;
    virtual void visit(ASTDeclareCopyNsp &n) = 0;
    virtual void visit(ASTDefCollation &n) = 0;
    virtual void visit(ASTDefNamespaceDecl &n) = 0;
    virtual void visit(ASTDocConst &n) = 0;
    virtual void visit(ASTDocTest &n) = 0;
    virtual void visit(ASTDropColl &n) = 0;
    virtual void visit(ASTDropDoc &n) = 0;
    virtual void visit(ASTDropFtIndex &n) = 0;
    virtual void visit(ASTDropIndex &n) = 0;
    virtual void visit(ASTDropMod &n) = 0;
    virtual void visit(ASTDropRole &n) = 0;
    virtual void visit(ASTDropTrg &n) = 0;
    virtual void visit(ASTDropUser &n) = 0;
    virtual void visit(ASTElemConst &n) = 0;
    virtual void visit(ASTElementTest &n) = 0;
    virtual void visit(ASTElem &n) = 0;
    virtual void visit(ASTEmptyTest &n) = 0;
    virtual void visit(ASTExtExpr &n) = 0;
    virtual void visit(ASTError &n) = 0;
    virtual void visit(ASTFilterStep &n) = 0;
    virtual void visit(ASTFLWOR &n) = 0;
    virtual void visit(ASTFor &n) = 0;
    virtual void visit(ASTFunCall &n) = 0;
    virtual void visit(ASTFuncDecl &n) = 0;
    virtual void visit(ASTGrantPriv &n) = 0;
    virtual void visit(ASTGrantRole &n) = 0;
    virtual void visit(ASTIf &n) = 0;
    virtual void visit(ASTInstOf &n) = 0;
    virtual void visit(ASTItemTest &n) = 0;
    virtual void visit(ASTLet &n) = 0;
    virtual void visit(ASTLibModule &n) = 0;
    virtual void visit(ASTLit &n) = 0;
    virtual void visit(ASTLoadFile &n) = 0;
    virtual void visit(ASTLoadModule &n) = 0;
    virtual void visit(ASTMainModule &n) = 0;
    virtual void visit(ASTMetaCols &n) = 0;
    virtual void visit(ASTMetaDocs &n) = 0;
    virtual void visit(ASTMetaSchemaCol &n) = 0;
    virtual void visit(ASTMetaSchemaDoc &n) = 0;
    virtual void visit(ASTModImport &n) = 0;
    virtual void visit(ASTModuleDecl &n) = 0;
    virtual void visit(ASTNamespaceDecl &n) = 0;
    virtual void visit(ASTNameTest &n) = 0;
    virtual void visit(ASTNodeTest &n) = 0;
    virtual void visit(ASTNsp &n) = 0;
    virtual void visit(ASTOption &n) = 0;
    virtual void visit(ASTOrderBy &n) = 0;
    virtual void visit(ASTOrderEmpty &n) = 0;
    virtual void visit(ASTOrderModInt &n) = 0;
    virtual void visit(ASTOrderMod &n) = 0;
    virtual void visit(ASTOrder &n) = 0;
    virtual void visit(ASTOrderSpec &n) = 0;
    virtual void visit(ASTOrdExpr &n) = 0;
    virtual void visit(ASTPIConst &n) = 0;
    virtual void visit(ASTPi &n) = 0;
    virtual void visit(ASTPiTest &n) = 0;
    virtual void visit(ASTPosVar &n) = 0;
    virtual void visit(ASTPragma &n) = 0;
    virtual void visit(ASTPred &n) = 0;
    virtual void visit(ASTProlog &n) = 0;
    virtual void visit(ASTQName &n) = 0;
    virtual void visit(ASTQuantExpr &n) = 0;
    virtual void visit(ASTQuery &n) = 0;
    virtual void visit(ASTRenameColl &n) = 0;
    virtual void visit(ASTRevokePriv &n) = 0;
    virtual void visit(ASTRevokeRole &n) = 0;
    virtual void visit(ASTSchemaAttrTest &n) = 0;
    virtual void visit(ASTSchemaElemTest &n) = 0;
    virtual void visit(ASTSeq &n) = 0;
    virtual void visit(ASTSpaceSeq &n) = 0;
    virtual void visit(ASTTextConst &n) = 0;
    virtual void visit(ASTTextTest &n) = 0;
    virtual void visit(ASTTreat &n) = 0;
    virtual void visit(ASTType &n) = 0;
    virtual void visit(ASTTypeSeq &n) = 0;
    virtual void visit(ASTTypeSingle &n) = 0;
    virtual void visit(ASTTypeSwitch &n) = 0;
    virtual void visit(ASTTypeVar &n) = 0;
    virtual void visit(ASTUop &n) = 0;
    virtual void visit(ASTUpdDel &n) = 0;
    virtual void visit(ASTUpdInsert &n) = 0;
    virtual void visit(ASTUpdMove &n) = 0;
    virtual void visit(ASTUpdRename &n) = 0;
    virtual void visit(ASTUpdReplace &n) = 0;
    virtual void visit(ASTVarDecl &n) = 0;
    virtual void visit(ASTVar &n) = 0;
    virtual void visit(ASTVersionDecl &n) = 0;
    virtual void visit(ASTXMLComm &n) = 0;
};

#endif
