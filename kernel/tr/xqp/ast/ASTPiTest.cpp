/*
 * File:  ASTPiTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTPiTest.h"

ASTPiTest::~ASTPiTest()
{
    delete test;
}

void ASTPiTest::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTPiTest::dup()
{
    return new ASTPiTest(loc, (test) ? new std::string(*test) : NULL, type);
}

ASTNode *ASTPiTest::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *test = NULL;
    TypeTest type;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_NUMBER);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    type = TypeTest(atol(sl[2].internal.num));

    if (type != NONE)
    {
        U_ASSERT(sl[3].type == SCM_STRING);
        test = new std::string(sl[3].internal.str);
    }

    return new ASTPiTest(loc, test, type);
}

void ASTPiTest::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}
