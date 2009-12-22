/*
 * File:  ASTPi.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPi.h"

ASTPi::~ASTPi()
{
    delete name;
    delete cont;
}

void ASTPi::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTPi::dup()
{
    return new ASTPi(cd, new std::string(*name), (cont) ? new std::string(*cont) : NULL);
}

ASTNode *ASTPi::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    std::string *name = NULL, *cont = NULL;
    ASTPi *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    name = new std::string(sl[2].internal.str);
    cont = new std::string(sl[3].internal.str);

    res = new ASTPi(cd, name, cont);

    U_ASSERT(sl[4].type == SCM_BOOL);
    res->deep_copy = sl[4].internal.b;

    return res;
}

void ASTPi::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}
