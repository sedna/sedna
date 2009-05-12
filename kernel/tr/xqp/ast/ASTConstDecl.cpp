/*
 * File:  ASTConstDecl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTConstDecl.h"

void ASTConstDecl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTConstDecl::dup()
{
    return new ASTConstDecl(loc, mod);
}
