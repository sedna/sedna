/*
 * File:  ASTTypeSingle.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTypeSingle.h"

ASTTypeSingle::~ASTTypeSingle()
{
    delete type;
}

void ASTTypeSingle::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTTypeSingle::dup()
{
    return new ASTTypeSingle(loc, static_cast<ASTAtomicTest *>(type->dup()), mod);
}
