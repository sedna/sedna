/*
 * File:  ASTCommentConst.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCommentConst.h"

ASTCommentConst::~ASTCommentConst()
{
    delete expr;
}

void ASTCommentConst::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCommentConst::dup()
{
    return new ASTCommentConst(loc, expr->dup());
}
