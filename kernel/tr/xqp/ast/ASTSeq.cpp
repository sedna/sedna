/*
 * File:  ASTSeq.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTSeq.h"

ASTSeq::~ASTSeq()
{
    destroyASTNodesVector(exprs);
}

void ASTSeq::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTSeq::dup()
{
    return new ASTSeq(loc, duplicateASTNodes(exprs));
}
