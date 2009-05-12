/*
 * File:  ASTMetaSchemaCol.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTMetaSchemaCol.h"

ASTMetaSchemaCol::~ASTMetaSchemaCol()
{
    delete coll;
}

void ASTMetaSchemaCol::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTMetaSchemaCol::dup()
{
    return new ASTMetaSchemaCol(loc, coll->dup());
}
