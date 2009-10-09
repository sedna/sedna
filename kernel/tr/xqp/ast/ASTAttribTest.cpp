/*
 * File:  ASTAttribTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTAttribTest.h"

ASTAttribTest::~ASTAttribTest()
{
    delete name;
    delete type;
}

void ASTAttribTest::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTAttribTest::dup()
{
    ASTAttribTest *res;
    res = new ASTAttribTest(cd, (name) ? name->dup() : NULL, (type) ? type->dup() : NULL);

    return res;
}

ASTNode *ASTAttribTest::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *type = NULL, *name = NULL;
    ASTAttribTest *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    name = dsGetASTFromSchemeList(*sl[2].internal.list);
    type = dsGetASTFromSchemeList(*sl[3].internal.list);

    res = new ASTAttribTest(cd, name, type);

    return res;
}

void ASTAttribTest::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (name == oldc)
    {
        name = newc;
        return;
    }
    if (type == oldc)
    {
        type = newc;
        return;
    }
}
