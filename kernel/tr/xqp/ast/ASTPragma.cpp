/*
 * File:  ASTPragma.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPragma.h"

ASTPragma::ASTPragma(const ASTNodeCommonData &cd, std::string *pragma_name, std::string *pragma_cont) : ASTNode(cd), cont(pragma_cont)
{
    ASTParseQName(pragma_name, &pref, &local);

    delete pragma_name;
}

ASTPragma::ASTPragma(const ASTNodeCommonData &cd, std::string *pr_pref, std::string *pr_local, std::string *pragma_cont)
        : ASTNode(cd),
          pref(pr_pref),
          local(pr_local),
          cont(pragma_cont)
{
}

ASTPragma::~ASTPragma()
{
    delete pref;
    delete local;
    delete cont;
}

void ASTPragma::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTPragma::dup()
{
    return new ASTPragma(cd, new std::string(*pref), new std::string(*local), (cont == NULL) ? NULL : new std::string(*cont));
}

ASTNode *ASTPragma::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *pref = NULL, *local = NULL, *cont = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING && sl[4].type == SCM_STRING);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    pref = new std::string(sl[2].internal.str);
    local = new std::string(sl[3].internal.str);
    cont = new std::string(sl[4].internal.str);

    return new ASTPragma(cd, pref, local, cont);
}

void ASTPragma::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}
