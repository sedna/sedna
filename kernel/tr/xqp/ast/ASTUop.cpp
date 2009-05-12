/*
 * File:  ASTUop.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTUop.h"

ASTUop::~ASTUop()
{
    delete expr;
}

void ASTUop::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode* ASTUop::dup()
{
    return new ASTUop(loc, op, expr->dup());
}
