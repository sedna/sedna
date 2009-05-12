/*
 * File:  ASTCommTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCommTest.h"

void ASTCommTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCommTest::dup()
{
    return new ASTCommTest(loc);
}
