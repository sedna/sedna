/*
 * File:  ASTElementTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTElementTest.h"

ASTElementTest::~ASTElementTest()
{
    delete npref;
    delete nloc;

    delete tpref;
    delete tloc;
}

void ASTElementTest::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTElementTest::dup()
{
    return new ASTElementTest(loc,
                             (npref) ? new std::string(*npref) : NULL,
                             (nloc) ? new std::string(*nloc) : NULL,
                             (tpref) ? new std::string(*tpref) : NULL,
                             (tloc) ? new std::string(*tloc) : NULL,
                             mod
                            );
}
