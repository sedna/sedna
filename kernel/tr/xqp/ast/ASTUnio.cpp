/*
 * File:  ASTUnio.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUnio.h"

ASTUnio::~ASTUnio()
{
    destroyASTNodesVector(vars);
}

void ASTUnio::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTUnio::dup()
{
    return new ASTUnio(loc, duplicateASTNodes(vars));
}
