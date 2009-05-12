/*
 * File:  ASTDocConst.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTDocConst.h"

ASTDocConst::~ASTDocConst()
{
    delete expr;
}

void ASTDocConst::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTDocConst::dup()
{
    return new ASTDocConst(loc, expr->dup());
}
