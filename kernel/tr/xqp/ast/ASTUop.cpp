/*
 * File:  ASTUop.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUop.h"

ASTUop::~ASTUop()
{
    delete expr;
}

void ASTUop::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode* ASTUop::dup()
{
    return new ASTUop(loc, op, expr->dup());
}

ASTNode *ASTUop::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL;
    Oper op;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_NUMBER);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    op = Oper(atol(sl[3].internal.num));

    return new ASTUop(loc, op, expr);
}

void ASTUop::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
}
