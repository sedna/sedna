/*
 * File:  ASTDDO.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDDO.h"

ASTDDO::~ASTDDO()
{
    delete expr;
}

void ASTDDO::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDDO::dup()
{
    return new ASTDDO(loc, expr->dup());
}
