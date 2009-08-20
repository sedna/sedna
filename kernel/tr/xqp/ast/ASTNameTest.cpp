/*
 * File:  ASTNameTest.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTNameTest.h"

ASTNameTest::~ASTNameTest()
{
    delete pref;
    delete local;

    delete uri;
}

void ASTNameTest::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTNameTest::dup()
{
    ASTNameTest *res;

    res = new ASTNameTest(loc, new std::string(*pref), new std::string(*local));

    if (uri)
        res->uri = new std::string(*uri);

    return res;
}

ASTNode *ASTNameTest::createNode(scheme_list &sl)
{
    ASTLocation loc;
    std::string *pref = NULL, *local = NULL;
    ASTNameTest *res;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_STRING && sl[3].type == SCM_STRING);

    loc = dsGetASTLocationFromSList(*sl[1].internal.list);
    pref = new std::string(sl[2].internal.str);
    local = new std::string(sl[3].internal.str);

    res = new ASTNameTest(loc, pref, local);

    if (sl.size() > 4)
    {
        U_ASSERT(sl[4].type == SCM_STRING);
        res->uri = new std::string(sl[4].internal.str);
    }

    return res;
}

void ASTNameTest::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
}
