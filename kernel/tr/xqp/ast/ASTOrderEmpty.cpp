/*
 * File:  ASTOrderEmpty.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrderEmpty.h"

void ASTOrderEmpty::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOrderEmpty::dup()
{
    return new ASTOrderEmpty(loc, mod);
}
