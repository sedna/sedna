/*
 * File:  ASTOrderSpec.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderSpec.h"

ASTOrderSpec::~ASTOrderSpec()
{
    delete expr;
    delete mod;
}

void ASTOrderSpec::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTOrderSpec::dup()
{
    return new ASTOrderSpec(loc, expr->dup(), (mod) ? mod->dup() : NULL);
}

ASTNode *ASTOrderSpec::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL, *mod = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    mod = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTOrderSpec(loc, expr, mod);
}

void ASTOrderSpec::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
    if (mod == oldc)
    {
        mod = newc;
        return;
    }
}
