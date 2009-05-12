/*
 * File:  ASTExtExpr.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTExtExpr.h"

ASTExtExpr::~ASTExtExpr()
{
    destroyASTNodesVector(pragmas);
    delete expr;
}

void ASTExtExpr::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode* ASTExtExpr::dup()
{
    return new ASTExtExpr(loc, duplicateASTNodes(pragmas), expr->dup());
}
