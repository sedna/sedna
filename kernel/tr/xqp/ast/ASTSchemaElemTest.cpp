/*
 * File:  ASTSchemaElemTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTSchemaElemTest.h"

ASTSchemaElemTest::~ASTSchemaElemTest()
{
    delete npref;
    delete nloc;
}

void ASTSchemaElemTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTSchemaElemTest::dup()
{
    return new ASTSchemaElemTest(loc,
                             (npref) ? new std::string(*npref) : NULL,
                             (nloc) ? new std::string(*nloc) : NULL
                            );
}
