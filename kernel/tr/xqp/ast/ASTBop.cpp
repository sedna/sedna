/*
 * File:  ASTBop.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTBop.h"

ASTBop::~ASTBop()
{
    delete lop;
    delete rop;
}

void ASTBop::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode* ASTBop::dup()
{
    return new ASTBop(loc, op, lop->dup(), rop->dup());
}
