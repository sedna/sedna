/*
 * File:  ASTInstOf.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTInstOf.h"

ASTInstOf::~ASTInstOf()
{
    delete expr;
    delete type;
}

void ASTInstOf::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTInstOf::dup()
{
    return new ASTInstOf(loc, expr->dup(), static_cast<ASTTypeSeq *>(type->dup()));
}

ASTNode *ASTInstOf::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL, *type = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    type = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTInstOf(loc, expr, type);
}

void ASTInstOf::modifyChild(const ASTNode *oldc, ASTNode *newc)
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
