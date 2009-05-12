/*
 * File:  ASTNodeTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTNodeTest.h"

void ASTNodeTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTNodeTest::dup()
{
    return new ASTNodeTest(loc);
}
