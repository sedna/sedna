/*
 * File:  ASTUpdInsert.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUpdInsert.h"

ASTUpdInsert::~ASTUpdInsert()
{
    delete what;
    delete where;
}

void ASTUpdInsert::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTUpdInsert::dup()
{
    return new ASTUpdInsert(loc, what->dup(), where->dup(), type);
}
