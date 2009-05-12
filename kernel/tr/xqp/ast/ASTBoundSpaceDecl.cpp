/*
 * File:  ASTBoundSpaceDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTBoundSpaceDecl.h"

void ASTBoundSpaceDecl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTBoundSpaceDecl::dup()
{
    return new ASTBoundSpaceDecl(loc, mod);
}
