/*
 * File:  ASTPosVar.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPosVar.h"

ASTPosVar::~ASTPosVar()
{
    delete var;
}

void ASTPosVar::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTPosVar::dup()
{
    return new ASTPosVar(cd, var->dup());
}

ASTNode *ASTPosVar::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *var = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    var = dsGetASTFromSchemeList(*sl[2].internal.list);

    return new ASTPosVar(cd, var);
}

void ASTPosVar::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (var == oldc)
    {
        var = newc;
        return;
    }
}
