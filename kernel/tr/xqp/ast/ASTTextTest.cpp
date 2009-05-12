/*
 * File:  ASTTextTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTextTest.h"

void ASTTextTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTTextTest::dup()
{
    return new ASTTextTest(loc);
}
