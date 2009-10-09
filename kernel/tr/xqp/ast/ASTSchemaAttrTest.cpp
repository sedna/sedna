/*
 * File:  ASTSchemaAttrTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTSchemaAttrTest.h"

ASTSchemaAttrTest::~ASTSchemaAttrTest()
{
    delete name;
}

void ASTSchemaAttrTest::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTSchemaAttrTest::dup()
{
    return new ASTSchemaAttrTest(cd, name->dup());
}

ASTNode *ASTSchemaAttrTest::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *name;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    name = dsGetASTFromSchemeList(*sl[2].internal.list);

    return new ASTSchemaAttrTest(cd, name);
}

void ASTSchemaAttrTest::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (name == oldc)
    {
        name = newc;
        return;
    }
}
