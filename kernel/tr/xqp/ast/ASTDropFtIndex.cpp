/*
 * File:  ASTDropFtIndex.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/serial/deser.h"

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropFtIndex.h"

ASTDropFtIndex::~ASTDropFtIndex()
{
    delete index;
}

void ASTDropFtIndex::accept(ASTVisitor &v)
{
    v.addToPath(this);
    v.visit(*this);
    v.removeFromPath(this);
}

ASTNode *ASTDropFtIndex::dup()
{
    return new ASTDropFtIndex(cd, index->dup());
}

ASTNode *ASTDropFtIndex::createNode(scheme_list &sl)
{
    ASTNodeCommonData cd;
    ASTNode *ind = NULL;

    U_ASSERT(sl[1].type == SCM_LIST && sl[2].type == SCM_LIST);

    cd = dsGetASTCommonFromSList(*sl[1].internal.list);
    ind = dsGetASTFromSchemeList(*sl[2].internal.list);

    return new ASTDropFtIndex(cd, ind);
}

void ASTDropFtIndex::modifyChild(const ASTNode *oldc, ASTNode *newc)
{
    if (index == oldc)
    {
        index = newc;
        return;
    }
}
