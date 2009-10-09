/*
 * File:  ASTDDO.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDDO.h"

ASTDDO::~ASTDDO()
{
    delete expr;
}

void ASTDDO::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDDO::dup()
{
    return new ASTDDO(loc, expr->dup(), distinct_only);
}

ASTNode *ASTDDO::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL;
    bool dist;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_BOOL);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    dist = sl[3].internal.b;

    return new ASTDDO(loc, expr, dist);
}

void ASTDDO::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
}
