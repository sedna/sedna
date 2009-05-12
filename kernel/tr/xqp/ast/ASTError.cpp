/*
 * File:  ASTError.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTError.h"

void ASTError::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTError::dup()
{
    return new ASTError(loc);
}
