/*
 * File:  ASTCast.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCast.h"

ASTCast::~ASTCast()
{
    delete expr;
    delete type;
}

void ASTCast::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTCast::dup()
{
    ASTCast *res;

    res = new ASTCast(cd, expr->dup(), type->dup());

    return res;
}

ASTNode *ASTCast::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *type = NULL, *expr = NULL;
    ASTCast *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);

    expr = dsGetASTFromSchemeList(*sl[2].internal.list);
    type = dsGetASTFromSchemeList(*sl[3].internal.list);

    res = new ASTCast(cd, expr, type);

    return res;
}

void ASTCast::modifyChild(const ASTNode *oldc, ASTNode *newc)
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
