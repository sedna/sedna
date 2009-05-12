/*
 * File:  ASTCreateFtIndex.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateFtIndex.h"

ASTCreateFtIndex::~ASTCreateFtIndex()
{
    delete name;
    delete path;
    delete type;
    delete cust_expr;
}

void ASTCreateFtIndex::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCreateFtIndex::dup()
{
    return new ASTCreateFtIndex(loc, name->dup(), path->dup(), new std::string(*type), (cust_expr) ? cust_expr->dup() : NULL);
}
