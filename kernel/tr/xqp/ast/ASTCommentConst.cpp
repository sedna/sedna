/*
 * File:  ASTCommentConst.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCommentConst.h"

ASTCommentConst::~ASTCommentConst()
{
    delete expr;
}

void ASTCommentConst::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTCommentConst::dup()
{
    return new ASTCommentConst(loc, expr->dup());
}

ASTNode *ASTCommentConst::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);

    return new ASTCommentConst(loc, expr);
}

void ASTCommentConst::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (expr == oldc)
    {
        expr = newc;
        return;
    }
}
