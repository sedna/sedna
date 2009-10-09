/*
 * File:  ASTVarDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTVarDecl.h"

ASTVarDecl::~ASTVarDecl()
{
    delete var;
    delete type;
    delete expr;
}

void ASTVarDecl::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTVarDecl::dup()
{
    return new ASTVarDecl(cd, var->dup(), (type == NULL) ? NULL: type->dup(), (expr == NULL) ? NULL: expr->dup());
}

ASTNode *ASTVarDecl::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *var = NULL, *type = NULL, *expr = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    var = dsGetASTFromSchemeList(*sl[2].internal.list);
    type = dsGetASTFromSchemeList(*sl[3].internal.list);
    expr = dsGetASTFromSchemeList(*sl[4].internal.list);

    return new ASTVarDecl(cd, var, type, expr);
}

void ASTVarDecl::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (var == oldc)
    {
        var = newc;
        return;
    }
    if (type == oldc)
    {
        type = newc;
        return;
    }
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
}
