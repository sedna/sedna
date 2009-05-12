/*
 * File:  ASTNameTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTNameTest.h"

ASTNameTest::~ASTNameTest()
{
    delete pref;
    delete local;
}

void ASTNameTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTNameTest::dup()
{
    return new ASTNameTest(loc, new std::string(*pref), new std::string(*local));
}
