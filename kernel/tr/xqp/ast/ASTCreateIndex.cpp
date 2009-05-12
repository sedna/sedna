/*
 * File:  ASTCreateIndex.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateIndex.h"

ASTCreateIndex::~ASTCreateIndex()
{
    delete name;
    delete on_path;
    delete by_path;
    delete type;
}

void ASTCreateIndex::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCreateIndex::dup()
{
    return new ASTCreateIndex(loc, name->dup(), on_path->dup(), by_path->dup(), static_cast<ASTTypeSingle *>(type->dup()));
}
