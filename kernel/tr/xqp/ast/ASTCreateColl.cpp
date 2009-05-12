/*
 * File:  ASTCreateColl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateColl.h"

ASTCreateColl::~ASTCreateColl()
{
    delete coll;
}

void ASTCreateColl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCreateColl::dup()
{
    return new ASTCreateColl(loc, coll->dup());
}
