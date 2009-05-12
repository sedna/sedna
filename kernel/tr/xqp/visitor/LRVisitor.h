/*
 * File:  LRVisitor.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "ASTVisitor.h"
#include <string>

class LRVisitor : public ASTVisitor
{
private:
    std::string lr_str;

public:
    ~LRVisitor() {}

    std::string getResult() const
    {
        return lr_str;
    }

    void resetVisitor()
    {
        lr_str = "";
    }

    // visiting functions
    void visit(const ASTAlterUser &n);
    void visit(const ASTAtomicTest &n);
    void visit(const ASTAttr &n);
    void visit(const ASTAttrConst &n);
    void visit(const ASTAttribTest &n);
    void visit(const ASTAxis &n);
    void visit(const ASTAxisStep &n);
    void visit(const ASTBaseURI &n);
    void visit(const ASTBop &n);
    void visit(const ASTBoundSpaceDecl &n);
    void visit(const ASTCase &n);
    void visit(const ASTCast &n);
    void visit(const ASTCastable &n);
    void visit(const ASTCharCont &n);
    void visit(const ASTCommTest &n);
    void visit(const ASTCommentConst &n);
    void visit(const ASTConstDecl &n);
    void visit(const ASTCreateColl &n);
    void visit(const ASTCreateDoc &n);
    void visit(const ASTCreateFtIndex &n);
    void visit(const ASTCreateIndex &n);
    void visit(const ASTCreateRole &n);
    void visit(const ASTCreateTrg &n);
    void visit(const ASTCreateUser &n);
    void visit(const ASTDDO &n);
    void visit(const ASTDeclareCopyNsp &n);
    void visit(const ASTDefCollation &n);
    void visit(const ASTDefNamespaceDecl &n);
    void visit(const ASTDocConst &n);
    void visit(const ASTDocTest &n);
    void visit(const ASTDropColl &n);
    void visit(const ASTDropDoc &n);
    void visit(const ASTDropFtIndex &n);
    void visit(const ASTDropIndex &n);
    void visit(const ASTDropMod &n);
    void visit(const ASTDropRole &n);
    void visit(const ASTDropTrg &n);
    void visit(const ASTDropUser &n);
    void visit(const ASTElem &n);
    void visit(const ASTElemConst &n);
    void visit(const ASTElementTest &n);
    void visit(const ASTEmptyTest &n);
    void visit(const ASTError &n);
    void visit(const ASTExtExpr &n);
    void visit(const ASTFilterStep &n);
    void visit(const ASTFor &n);
    void visit(const ASTFunCall &n);
    void visit(const ASTFunDef &n);
    void visit(const ASTFuncDecl &n);
    void visit(const ASTGrantPriv &n);
    void visit(const ASTGrantRole &n);
    void visit(const ASTIf &n);
    void visit(const ASTInstOf &n);
    void visit(const ASTItemTest &n);
    void visit(const ASTLet &n);
    void visit(const ASTLibModule &n);
    void visit(const ASTLit &n);
    void visit(const ASTLoadFile &n);
    void visit(const ASTLoadModule &n);
    void visit(const ASTMainModule &n);
    void visit(const ASTMetaCols &n);
    void visit(const ASTMetaDocs &n);
    void visit(const ASTMetaSchemaCol &n);
    void visit(const ASTMetaSchemaDoc &n);
    void visit(const ASTModImport &n);
    void visit(const ASTModuleDecl &n);
    void visit(const ASTNameTest &n);
    void visit(const ASTNamespaceDecl &n);
    void visit(const ASTNodeTest &n);
    void visit(const ASTNsp &n);
    void visit(const ASTOption &n);
    void visit(const ASTOrdExpr &n);
    void visit(const ASTOrder &n);
    void visit(const ASTOrderBy &n);
    void visit(const ASTOrderByRet &n);
    void visit(const ASTOrderEmpty &n);
    void visit(const ASTOrderMod &n);
    void visit(const ASTOrderModInt &n);
    void visit(const ASTOrderSpec &n);
    void visit(const ASTPIConst &n);
    void visit(const ASTPi &n);
    void visit(const ASTPiTest &n);
    void visit(const ASTPosVar &n);
    void visit(const ASTPragma &n);
    void visit(const ASTPred &n);
    void visit(const ASTProlog &n);
    void visit(const ASTQuantExpr &n);
    void visit(const ASTQuery &n);
    void visit(const ASTRenameColl &n);
    void visit(const ASTRet &n);
    void visit(const ASTRevokePriv &n);
    void visit(const ASTRevokeRole &n);
    void visit(const ASTSchemaAttrTest &n);
    void visit(const ASTSchemaElemTest &n);
    void visit(const ASTScript &n);
    void visit(const ASTSeq &n);
    void visit(const ASTSpaceSeq &n);
    void visit(const ASTTextConst &n);
    void visit(const ASTTextTest &n);
    void visit(const ASTTreat &n);
    void visit(const ASTTypeSeq &n);
    void visit(const ASTTypeSingle &n);
    void visit(const ASTTypeSwitch &n);
    void visit(const ASTTypeVar &n);
    void visit(const ASTUnio &n);
    void visit(const ASTUop &n);
    void visit(const ASTUpdDel &n);
    void visit(const ASTUpdInsert &n);
    void visit(const ASTUpdMove &n);
    void visit(const ASTUpdRename &n);
    void visit(const ASTUpdReplace &n);
    void visit(const ASTVar &n);
    void visit(const ASTVarDecl &n);
    void visit(const ASTVersionDecl &n);
    void visit(const ASTXMLComm &n);
};
