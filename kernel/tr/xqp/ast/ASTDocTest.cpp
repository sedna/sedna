/*
 * File:  ASTDocTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDocTest.h"

ASTDocTest::~ASTDocTest()
{
    delete elem_test;
}

void ASTDocTest::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDocTest::dup()
{
    return new ASTDocTest(loc, (elem_test) ? elem_test->dup() : NULL);
}

ASTNode *ASTDocTest::createNode(scheme_list &sl)
{
    ASTLocation loc;
    ASTNode *expr = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    expr = dsGetASTFromSchemeList(*sl[2].internal.list);

    return new ASTDocTest(loc, expr);
}

void ASTDocTest::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (elem_test == oldc)
    {
        elem_test = newc;
        return;
    }
}
