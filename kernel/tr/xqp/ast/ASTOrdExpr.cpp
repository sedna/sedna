/*
 * File:  ASTOrdExpr.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrdExpr.h"

ASTOrdExpr::~ASTOrdExpr()
{
    delete expr;
}

void ASTOrdExpr::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTOrdExpr::dup()
{
    return new ASTOrdExpr(loc, type, expr->dup());
}

ASTNode *ASTOrdExpr::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL;
    OrdType type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_NUMBER);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    type = OrdType(atol(sl[3].internal.num));

    return new ASTOrdExpr(loc, type, expr);
}

void ASTOrdExpr::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
}
