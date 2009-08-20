/*
 * File:  ASTTextTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTextTest.h"

void ASTTextTest::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTTextTest::dup()
{
    return new ASTTextTest(loc);
}

ASTNode *ASTTextTest::createNode(scheme_list &sl)
{
    ASTLocation loc;

    U_ASSERT(sl[1].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);

    return new ASTTextTest(loc);
}

void ASTTextTest::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}
