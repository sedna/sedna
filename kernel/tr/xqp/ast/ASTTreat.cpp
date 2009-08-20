/*
 * File:  ASTTreat.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTreat.h"

ASTTreat::~ASTTreat()
{
    delete expr;
    delete type;
}

void ASTTreat::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTTreat::dup()
{
    return new ASTTreat(loc, expr->dup(), type->dup());
}

ASTNode *ASTTreat::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL, *type = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    type = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTTreat(loc, expr, type);
}

void ASTTreat::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
    if (type == oldc)
    {
        type = newc;
        return;
    }
}
