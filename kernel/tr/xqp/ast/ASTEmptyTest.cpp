/*
 * File:  ASTEmptyTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTEmptyTest.h"

void ASTEmptyTest::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTEmptyTest::dup()
{
    return new ASTEmptyTest(loc);
}

ASTNode *ASTEmptyTest::createNode(scheme_list &sl)
{
    ASTLocation loc;

    U_ASSERT(sl[1].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);

    return new ASTEmptyTest(loc);
}

void ASTEmptyTest::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}
