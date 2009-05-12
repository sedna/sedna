/*
 * File:  ASTSpaceSeq.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTSpaceSeq.h"

ASTSpaceSeq::~ASTSpaceSeq()
{
    delete expr;
}

void ASTSpaceSeq::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTSpaceSeq::dup()
{
    return new ASTSpaceSeq(loc, expr->dup());
}
