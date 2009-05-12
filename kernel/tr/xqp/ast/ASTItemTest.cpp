/*
 * File:  ASTItemTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTItemTest.h"

void ASTItemTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTItemTest::dup()
{
    return new ASTItemTest(loc);
}
