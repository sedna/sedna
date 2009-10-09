/*
 * File:  ASTDropColl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropColl.h"

ASTDropColl::~ASTDropColl()
{
    delete coll;
}

void ASTDropColl::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDropColl::dup()
{
    return new ASTDropColl(cd, coll->dup());
}

ASTNode *ASTDropColl::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *coll = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    coll = dsGetASTFromSchemeList(*sl[2].internal.list);

    return new ASTDropColl(cd, coll);
}

void ASTDropColl::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (coll == oldc)
    {
        coll = newc;
        return;
    }
}
