/*
 * File:  ASTEmptyTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTEmptyTest.h"

void ASTEmptyTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTEmptyTest::dup()
{
    return new ASTEmptyTest(loc);
}
