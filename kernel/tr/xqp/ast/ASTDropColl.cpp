/*
 * File:  ASTDropColl.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDropColl.h"

ASTDropColl::~ASTDropColl()
{
    delete coll;
}

void ASTDropColl::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDropColl::dup()
{
    return new ASTDropColl(loc, coll->dup());
}