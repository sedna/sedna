/*
 * File:  ASTCastable.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCastable.h"

ASTCastable::~ASTCastable()
{
    delete expr;
    delete type;
}

void ASTCastable::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTCastable::dup()
{
    ASTCastable *res;

    res = new ASTCastable(loc, expr->dup(), type->dup());

    return res;
}

ASTNode *ASTCastable::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *type = NULL, *expr = NULL;
    ASTCastable *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);

    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    type = dsGetASTFromSchemeList(*sl[3].internal.list);

    res = new ASTCastable(loc, expr, type);

    return res;
}

void ASTCastable::modifyChild(const ASTNode *oldc, ASTNode *newc)
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
