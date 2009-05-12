/*
 * File:  ASTRet.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTRet.h"

ASTRet::~ASTRet()
{
    delete iter_expr;
    delete ret_expr;
}

void ASTRet::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTRet::dup()
{
    return new ASTRet(loc, iter_expr->dup(), ret_expr->dup());
}
