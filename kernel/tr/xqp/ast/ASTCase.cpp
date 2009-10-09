/*
 * File:  ASTCase.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCase.h"

ASTCase::~ASTCase()
{
    delete type;
    delete var;
    delete expr;
}

void ASTCase::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTCase::dup()
{
    return new ASTCase(cd, (var) ? var->dup() : NULL, (type) ? type->dup() : NULL, expr->dup());
}

ASTNode *ASTCase::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *var = NULL, *type = NULL, *expr = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    var = dsGetASTFromSchemeList(*sl[2].internal.list);
    type = dsGetASTFromSchemeList(*sl[3].internal.list);
    expr = dsGetASTFromSchemeList(*sl[4].internal.list);

    return new ASTCase(cd, var, type, expr);
}

void ASTCase::modifyChild(const ASTNode *oldc, ASTNode *newc)
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
