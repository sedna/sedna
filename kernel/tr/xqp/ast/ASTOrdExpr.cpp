/*
 * File:  ASTOrdExpr.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrdExpr.h"

ASTOrdExpr::~ASTOrdExpr()
{
    delete expr;
}

void ASTOrdExpr::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOrdExpr::dup()
{
    return new ASTOrdExpr(loc, type, expr->dup());
}
