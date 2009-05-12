/*
 * File:  ASTQuery.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTQuery.h"

ASTQuery::~ASTQuery()
{
    delete query;
}

void ASTQuery::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTQuery::dup()
{
    return new ASTQuery(loc, query->dup(), type);
}
