/*
 * File:  ASTQuantExpr.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTQuantExpr.h"

ASTQuantExpr::~ASTQuantExpr()
{
    delete expr;
    delete fd;
}

void ASTQuantExpr::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTQuantExpr::dup()
{
    return new ASTQuantExpr(loc, expr->dup(), fd->dup(), type);
}

ASTNode *ASTQuantExpr::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL, *fd = NULL;
    QuantMod mod;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_NUMBER);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    fd = dsGetASTFromSchemeList(*sl[3].internal.list);
    mod = QuantMod(atol(sl[4].internal.num));

    return new ASTQuantExpr(loc, expr, fd, mod);
}

void ASTQuantExpr::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
    if (fd == oldc)
    {
        fd = newc;
        return;
    }
}
