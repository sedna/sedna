/*
 * File:  ASTTextConst.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTTextConst.h"

ASTTextConst::~ASTTextConst()
{
    delete expr;
}

void ASTTextConst::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTTextConst::dup()
{
    return new ASTTextConst(loc, expr->dup());
}
