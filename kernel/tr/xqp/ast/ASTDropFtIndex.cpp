/*
 * File:  ASTDropFtIndex.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropFtIndex.h"

ASTDropFtIndex::~ASTDropFtIndex()
{
    delete index;
}

void ASTDropFtIndex::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropFtIndex::dup()
{
    return new ASTDropFtIndex(loc, index->dup());
}
