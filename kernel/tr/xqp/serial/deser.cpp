#include "deser.h"
#include "tr/executor/por2qep/scheme_tree.h"
#include "tr/xqp/ast/AST.h"

createNode_fun dsASTTable[AST_DUMMY] =
{
    NULL,
    &ASTAlterUser::createNode,
    &ASTAttrConst::createNode,
    &ASTAttr::createNode,
    &ASTAttribTest::createNode,
    &ASTAxisStep::createNode,
    &ASTBaseURI::createNode,
    &ASTBop::createNode,
    &ASTBoundSpaceDecl::createNode,
    &ASTCase::createNode,
    &ASTCastable::createNode,
    &ASTCast::createNode,
    &ASTCharCont::createNode,
    &ASTCommentConst::createNode,
    &ASTCommTest::createNode,
    &ASTConstDecl::createNode,
    &ASTCreateColl::createNode,
    &ASTCreateDoc::createNode,
    &ASTCreateFtIndex::createNode,
    &ASTCreateIndex::createNode,
    &ASTCreateRole::createNode,
    &ASTCreateTrg::createNode,
    &ASTCreateUser::createNode,
    &ASTDDO::createNode,
    &ASTDeclareCopyNsp::createNode,
    &ASTDefCollation::createNode,
    &ASTDefNamespaceDecl::createNode,
    &ASTDocConst::createNode,
    &ASTDocTest::createNode,
    &ASTDropColl::createNode,
    &ASTDropDoc::createNode,
    &ASTDropFtIndex::createNode,
    &ASTDropIndex::createNode,
    &ASTDropMod::createNode,
    &ASTDropRole::createNode,
    &ASTDropTrg::createNode,
    &ASTDropUser::createNode,
    &ASTElemConst::createNode,
    &ASTElementTest::createNode,
    &ASTElem::createNode,
    &ASTEmptyTest::createNode,
    NULL, // ASTError
    &ASTExtExpr::createNode,
    &ASTFilterStep::createNode,
    &ASTFor::createNode,
    &ASTFunCall::createNode,
    &ASTFuncDecl::createNode,
    &ASTGrantPriv::createNode,
    &ASTGrantRole::createNode,
    &ASTIf::createNode,
    &ASTInstOf::createNode,
    &ASTItemTest::createNode,
    &ASTLet::createNode,
    &ASTLibModule::createNode,
    &ASTLit::createNode,
    &ASTLoadFile::createNode,
    &ASTLoadModule::createNode,
    &ASTMainModule::createNode,
    &ASTMetaCols::createNode,
    &ASTMetaDocs::createNode,
    &ASTMetaSchemaCol::createNode,
    &ASTMetaSchemaDoc::createNode,
    &ASTModImport::createNode,
    &ASTModuleDecl::createNode,
    &ASTNamespaceDecl::createNode,
    &ASTNameTest::createNode,
    NULL, // ASTNode
    &ASTNodeTest::createNode,
    &ASTNsp::createNode,
    &ASTOption::createNode,
    &ASTOrderBy::createNode,
    &ASTOrderByRet::createNode,
    &ASTOrderEmpty::createNode,
    &ASTOrder::createNode,
    &ASTOrderMod::createNode,
    &ASTOrderModInt::createNode,
    &ASTOrderSpec::createNode,
    &ASTOrdExpr::createNode,
    &ASTPIConst::createNode,
    &ASTPi::createNode,
    &ASTPiTest::createNode,
    &ASTPosVar::createNode,
    &ASTPragma::createNode,
    &ASTPred::createNode,
    &ASTProlog::createNode,
    &ASTQName::createNode,
    &ASTQuantExpr::createNode,
    &ASTQuery::createNode,
    &ASTRenameColl::createNode,
    &ASTRevokePriv::createNode,
    &ASTRevokeRole::createNode,
    &ASTSchemaAttrTest::createNode,
    &ASTSchemaElemTest::createNode,
    &ASTSeq::createNode,
    &ASTSpaceSeq::createNode,
    &ASTTextConst::createNode,
    &ASTTextTest::createNode,
    &ASTTreat::createNode,
    &ASTType::createNode,
    &ASTTypeSeq::createNode,
    &ASTTypeSingle::createNode,
    &ASTTypeSwitch::createNode,
    &ASTTypeVar::createNode,
    &ASTUnio::createNode,
    &ASTUop::createNode,
    &ASTUpdDel::createNode,
    &ASTUpdInsert::createNode,
    &ASTUpdMove::createNode,
    &ASTUpdRename::createNode,
    &ASTUpdReplace::createNode,
    &ASTVarDecl::createNode,
    &ASTVar::createNode,
    &ASTVersionDecl::createNode,
    &ASTXMLComm::createNode
};

ASTNode *dsGetASTFromString(const char *mod)
{
    scheme_list *sl_mod;
    ASTNode *res;

    sl_mod = make_tree_from_scheme_list(mod);

    res = dsGetASTFromSchemeList(*sl_mod);

    delete_scheme_list(sl_mod);

    return res;
}

ASTNode *dsGetASTFromSchemeList(scheme_list &sl)
{
    ASTNodeType type;

    U_ASSERT(sl[0].type == SCM_NUMBER);

    type = ASTNodeType(atoi(sl[0].internal.num));

    if (type == 0) return NULL;

    return dsASTTable[type](sl);
}

/* builds AST nodes vector from string; calls dsGetASTNodesFromSList */
ASTNodesVector *dsGetASTNodesFromString(const char *str)
{
    scheme_list *sl_mod;
    ASTNodesVector *res;

    sl_mod = make_tree_from_scheme_list(str);

    res = dsGetASTNodesFromSList(*sl_mod);

    delete_scheme_list(sl_mod);

    return res;
}

ASTNodesVector *dsGetASTNodesFromSList(scheme_list &slist)
{
    ASTNodesVector *res;
    ASTNode *nres;

    if (slist.size() == 0) return NULL;

    res = new ASTNodesVector();

    for (unsigned int i = 0; i < slist.size(); i++)
    {
        U_ASSERT(slist[i].type == SCM_LIST);

        nres = dsGetASTFromSchemeList(*slist[i].internal.list);

        res->push_back(nres);
    }

    return res;
}

/* retrireves location from scheme_list (serialize-deserialize AST logic)
    location is stored in SCM_LIST as four SCM_NUMBER */
ASTNodeCommonData dsGetASTCommonFromSList(scheme_list &slist)
{
    ASTNodeCommonData res;

    U_ASSERT(slist.size() == 5 && slist[0].type == SCM_NUMBER &&
            slist[1].type == SCM_NUMBER && slist[2].type == SCM_NUMBER && slist[3].type == SCM_NUMBER && slist[4].type == SCM_BOOL);

    res.loc.begin.line = atol(slist[0].internal.num);
    res.loc.begin.column = atol(slist[1].internal.num);
    res.loc.end.line = atol(slist[2].internal.num);
    res.loc.end.column = atol(slist[3].internal.num);

    res.isCached = slist[4].internal.b;

    return res;
}

ASTStringVector *dsGetASTStringsFromSList(scheme_list &slist)
{
    ASTStringVector *res;
    std::string *nres;

    if (slist.size() == 0) return NULL;

    res = new ASTStringVector();

    for (unsigned int i = 0; i < slist.size(); i++)
    {
        U_ASSERT(slist[i].type == SCM_STRING);

        nres = new std::string(slist[i].internal.str);

        res->push_back(nres);
    }

    return res;
}
