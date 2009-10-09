/*
 * File:  ASTQuantExpr.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTQuantExpr.h"

ASTQuantExpr::~ASTQuantExpr()
{
    delete var;
    delete expr;
    delete sat;
}

void ASTQuantExpr::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTQuantExpr::dup()
{
    return new ASTQuantExpr(cd, var->dup(), expr->dup(), sat->dup(), type);
}

ASTNode *ASTQuantExpr::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *var = NULL, *expr = NULL, *sat = NULL;
    QuantMod mod;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_LIST && sl[5].type == SCM_NUMBER);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    var = dsGetASTFromSchemeList(*sl[2].internal.list);
    expr = dsGetASTFromSchemeList(*sl[3].internal.list);
    sat = dsGetASTFromSchemeList(*sl[4].internal.list);
    mod = QuantMod(atol(sl[5].internal.num));

    return new ASTQuantExpr(cd, var, expr, sat, mod);
}

void ASTQuantExpr::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (var == oldc)
    {
        var = newc;
        return;
    }
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
    if (sat == oldc)
    {
        sat = newc;
        return;
    }
}
