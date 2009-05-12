/*
 * File:  ASTOrderBy.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderBy.h"

ASTOrderBy::~ASTOrderBy()
{
    destroyASTNodesVector(specs);
}

void ASTOrderBy::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOrderBy::dup()
{
    return new ASTOrderBy(loc, type, duplicateASTNodes(specs));
}
