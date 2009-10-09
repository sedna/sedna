/*
 * File:  ASTElementTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTElementTest.h"

ASTElementTest::~ASTElementTest()
{
    delete name;
    delete type;
}

void ASTElementTest::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTElementTest::dup()
{
    ASTElementTest *res;

    res = new ASTElementTest(cd, (name) ? name->dup() : NULL, (type) ? type->dup() : NULL, mod);

    return res;
}

ASTNode *ASTElementTest::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *type = NULL, *name = NULL;
    ASTElementTest *res;
    Mod mod = ASTElementTest::NON_NIL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST && sl[3].type == SCM_LIST && sl[4].type == SCM_NUMBER);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    name = dsGetASTFromSchemeList(*sl[2].internal.list);
    type = dsGetASTFromSchemeList(*sl[3].internal.list);
    mod = Mod(atol(sl[4].internal.num));

    res = new ASTElementTest(cd, name, type, mod);

    return res;
}

void ASTElementTest::modifyChild(const ASTNode *oldc, ASTNode *newc)
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
