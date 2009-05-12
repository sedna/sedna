/*
 * File:  ASTAtomicTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAtomicTest.h"

ASTAtomicTest::~ASTAtomicTest()
{
    delete name;
}

void ASTAtomicTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTAtomicTest::dup()
{
    return new ASTAtomicTest(loc, new std::string(*name));
}
