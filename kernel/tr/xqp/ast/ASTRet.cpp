/*
 * File:  ASTRet.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTRet.h"

ASTRet::~ASTRet()
{
    delete iter_expr;
    delete ret_expr;
}

void ASTRet::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTRet::dup()
{
    return new ASTRet(loc, iter_expr->dup(), ret_expr->dup());
}

ASTNode *ASTRet::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *iter = NULL, *ret = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    iter = dsGetASTFromSchemeList(*sl[2].internal.list);
    ret = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTRet(loc, iter, ret);
}

void ASTRet::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (iter_expr == oldc)
    {
        iter_expr = newc;
        return;
    }
    if (ret_expr == oldc)
    {
        ret_expr = newc;
        return;
    }
}
