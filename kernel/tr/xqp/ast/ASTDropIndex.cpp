/*
 * File:  ASTDropIndex.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropIndex.h"

ASTDropIndex::~ASTDropIndex()
{
    delete index;
}

void ASTDropIndex::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropIndex::dup()
{
    return new ASTDropIndex(loc, index->dup());
}
