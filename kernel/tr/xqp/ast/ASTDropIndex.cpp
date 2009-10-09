/*
 * File:  ASTDropIndex.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropIndex.h"

ASTDropIndex::~ASTDropIndex()
{
    delete index;
}

void ASTDropIndex::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDropIndex::dup()
{
    return new ASTDropIndex(cd, index->dup());
}

ASTNode *ASTDropIndex::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *ind = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    ind = dsGetASTFromSchemeList(*sl[2].internal.list);

    return new ASTDropIndex(cd, ind);
}

void ASTDropIndex::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (index == oldc)
    {
        index = newc;
        return;
    }
}
