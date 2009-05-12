/*
 * File:  ASTOrder.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTOrder.h"

void ASTOrder::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTOrder::dup()
{
    return new ASTOrder(loc, mod);
}
