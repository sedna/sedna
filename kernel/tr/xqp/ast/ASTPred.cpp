/*
 * File:  ASTPred.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPred.h"

ASTPred::~ASTPred()
{
    delete iter_expr;
    delete pred_expr;
}

void ASTPred::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTPred::dup()
{
    return new ASTPred(loc, iter_expr->dup(), pred_expr->dup());
}

ASTNode *ASTPred::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *iter = NULL, *pred = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    iter = dsGetASTFromSchemeList(*sl[2].internal.list);
    pred = dsGetASTFromSchemeList(*sl[3].internal.list);

    return new ASTPred(loc, iter, pred);
}

void ASTPred::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (iter_expr == oldc)
    {
        iter_expr = newc;
        return;
    }
    if (pred_expr == oldc)
    {
        pred_expr = newc;
        return;
    }
}
