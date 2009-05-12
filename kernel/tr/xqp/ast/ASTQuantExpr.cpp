/*
 * File:  ASTQuantExpr.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTQuantExpr.h"

ASTQuantExpr::~ASTQuantExpr()
{
    delete expr;
    delete fd;
}

void ASTQuantExpr::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTQuantExpr::dup()
{
    return new ASTQuantExpr(loc, expr->dup(), static_cast<ASTFunDef *>(fd->dup()), type);
}
