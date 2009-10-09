/*
 * File:  ASTSchemaElemTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTSchemaElemTest.h"

ASTSchemaElemTest::~ASTSchemaElemTest()
{
    delete name;
}

void ASTSchemaElemTest::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTSchemaElemTest::dup()
{
    return new ASTSchemaElemTest(cd, name->dup());
}

ASTNode *ASTSchemaElemTest::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *name;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    name = dsGetASTFromSchemeList(*sl[2].internal.list);

    return new ASTSchemaElemTest(cd, name);
}

void ASTSchemaElemTest::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (name == oldc)
    {
        name = newc;
        return;
    }
}
