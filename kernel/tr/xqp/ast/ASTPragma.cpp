/*
 * File:  ASTPragma.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPragma.h"

ASTPragma::ASTPragma(ASTLocation &loc, std::string *pragma_name, std::string *pragma_cont) : ASTNode(loc), cont(pragma_cont)
{
    ASTParseQName(pragma_name, &pref, &local);
}

ASTPragma::ASTPragma(ASTLocation &loc, std::string *pr_pref, std::string *pr_local, std::string *pragma_cont)
        : ASTNode(loc),
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
    v.visit(*this);
}

ASTNode *ASTPragma::dup()
{
    return new ASTPragma(loc, new std::string(*pref), new std::string(*local), (cont == NULL) ? NULL : new std::string(*cont));
}
